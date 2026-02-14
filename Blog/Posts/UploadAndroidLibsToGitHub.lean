
import VersoBlog
import Blog.Categories
open Verso Genre Blog

#doc (Post) "Upload Gradle Build Scripts and Android Libraries to GitHub Packages" =>

%%%
authors := ["berberman"]
date := {year := 2023, month := 7, day := 5}
categories := [other]
%%%


# Foreword

This is a simple note I made while I was working on the plugin system in [fcitx5-android](https://github.com/fcitx5-android/fcitx5-android).
In general, we are using the monorepo policy for this project, which means that the main application and plugins are all in the same git repository
for the sake of reusing build scripts, versions, dependencies, as well as easy for creating releases.
However, recently we decided to make a series of plugins that provide a functionality of shipping data files like Pinyin dictionaries
and table dictionaries to the main program. Obviously, it would be cumbersome to have those plugins with data in our monorepo, and even more they could have different licenses.
So we need an approach to build a plugin application independently from the monorepo, yet reusing the build logic, which is quite complicated
and may deserve a long article to explain. Instead of somehow referencing the build scripts via git submodules, fortunately, [GitHub Packages](https://github.com/features/packages) provides a nice maven repository for each organization which we can publish those gradle scripts and libraries to.

# Publish the Build Scripts as Gradle Plugins

The build scripts, or the build logic, have to be gradle plugins in order to be published.
For example, in our project, we have a *standalone* gradle sub-project, rather than a module, called `build-logic`:

```
build-logic
├── convention
│   ├── build.gradle.kts
│   └── src
│       └── main
│           └── kotlin
│               ├── AndroidAppConventionPlugin.kt
│               ├── AndroidBaseConventionPlugin.kt
│               ├── AndroidLibConventionPlugin.kt
│               ├── AndroidPluginAppConventionPlugin.kt
│               ├── BuildMetadataPlugin.kt
│               ├── CMakeDirPlugin.kt
│               ├── DataDescriptorPlugin.kt
│               ├── FcitxComponentPlugin.kt
│               ├── FcitxHeadersPlugin.kt
│               ├── NativeAppConventionPlugin.kt
│               ├── NativeBaseConventionPlugin.kt
│               ├── NativeLibConventionPlugin.kt
│               ├── Utils.kt
│               └── Versions.kt
├── gradle
│   └── wrapper
│       ├── gradle-wrapper.jar -> ../../../gradle/wrapper/gradle-wrapper.jar
│       └── gradle-wrapper.properties -> ../../../gradle/wrapper/gradle-wrapper.properties
├── gradle.properties
└── settings.gradle.kts
```


which is included in `settings.gradle.kts` of the root project:

```
pluginManagement {
    includeBuild("build-logic")
    repositories {
        gradlePluginPortal()
        google()
        mavenCentral()
    }
}
```

Therefore, everything in `build-logic:convention` will be available in all gradle scripts of the project. The only thing left is to publish the module `build-logic`.
Generally, in `build.gradle.kts` (which is `convention` in the example), add `maven-publish` and `java-gradle-plugin`:

```
plugins {
    // ...
    `maven-publish`
    `java-gradle-plugin`
}
```

We do want to have the source code while using it as a library, so add the following snippet then:

```
java {
    withSourcesJar()
}
```

Finally, configure the maven publish plugin as the follows:

```
publishing {
    repositories {
        maven {
            name = "GitHubPackages"
            url = uri("https://maven.pkg.github.com/OWNER/REPO")
            credentials {
                username = System.getenv("GITHUB_ACTOR")
                password = System.getenv("GITHUB_TOKEN")
            }
        }
    }
}
```

Also don't forget to set a version for the project; otherwise the version of artifacts will be `unspecified`.

## Publish Android Libraries

Android Libraries will be packed into AAR files. This time we only need to add `maven-publish`:

```
plugins {
    // ...
    `maven-publish`
}
```

### Single Variant

If the library has only one build variant, saying `release`, we can add a `publishing {...}` to `android {...}` like:

```
android {
  // ...
  publishing {
    singleVariant("release") {
        withSourcesJar()
    }
  }
}
```

And then configure the maven publish plugin:

```
publishing {
    repositories {
        maven {
            name = "GitHubPackages"
            url = uri("https://maven.pkg.github.com/OWNER/REPO")
            credentials {
                username = System.getenv("GITHUB_ACTOR")
                password = System.getenv("GITHUB_TOKEN")
            }
        }
    }
    publications {
        register<MavenPublication>("release") {
            groupId = "..."
            artifactId = "..."
            afterEvaluate {
                from(components["release"])
            }
        }
    }
}
```

Note that we need to create a publication specifying the `groupId` and `artifactId`, and include the source from `release` component manually.

### Multiple Variants

If the library has multiple variants, we can use `multipleVariants {...}` in `publishing {...}`:

```
android {
  // other code
  publishing {
    multipleVariants {
      allVariants()
      withJavadocJar()
    }
  }
}
```

This will create a component called `default` which includes all build variants. You can find more information [here](https://developer.android.com/build/publish-library/configure-pub-variants).
The maven publish plugin configuration remains the same except that we should change the name of the component to `default`.


## Make a Reverse Proxy

Once the build logic and libraries are published, they can be introduced to other projects.
But I was surprised that as of this note was written, the GitHub maven repository *cannot* be read without a GitHub user token, even though it's public: [discussions/26634](https://github.com/orgs/community/discussions/26634).
To work around that, I created a new token with the permission to read GitHub packages, and then use a [Cloudflare Worker](https://workers.cloudflare.com/) to add authorization header to the requests to the repo:

```
addEventListener("fetch", event => {
  let url = new URL(event.request.url);
  url.hostname = "maven.pkg.github.com";
  let request = new Request(url, event.request);
  request.headers.append("Authorization","Basic <base64 encoded token>");
  event.respondWith(fetch(request));
});
```

Now in the project, we can add `maven ("https://<worker>.<subdomain>.workers.dev/OWNER/REPO/")`
to `repositories {...}` in `pluginManagement {...}` and `dependencyResolutionManagement {...}` block
to use gradle plugins and libraries from proxied maven repo respectively. The entire `settings.gradle.kts` looks like:

```

pluginManagement {
    repositories {
        maven("https://<worker>.<subdomain>.workers.dev/OWNER/REPO/")
        google()
        mavenCentral()
        gradlePluginPortal()
    }
}
dependencyResolutionManagement {
    repositoriesMode.set(RepositoriesMode.FAIL_ON_PROJECT_REPOS)
    repositories {
        maven("https://<worker>.<subdomain>.workers.dev/OWNER/REPO/")
        google()
        mavenCentral()
    }
}
rootProject.name = "Project name"
include(":app")

```

Since `pluginManagement` and `dependencyResolutionManagement` are evaluated before the whole script, there is no way to declare a variable and use it in those two blocks, so we need write the url two times.

## Summary

Although this is a very ad-hoc way to maintain a plugin system that has out-of-tree plugins, as the consistency is easy to get broken, implementing a sophisticated versioned API
requires a bit more work that would be overcomplicated and go beyond the initial purpose of shipping data files as apk.
