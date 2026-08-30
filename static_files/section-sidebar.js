(function () {
  var themeStorageKey = "space-theme";

  function preferredTheme() {
    var savedTheme;

    try {
      savedTheme = window.localStorage.getItem(themeStorageKey);
      if (savedTheme === "light" || savedTheme === "dark") return savedTheme;
    } catch (_error) {
      savedTheme = null;
    }

    return window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches
      ? "dark"
      : "light";
  }

  function giscusThemeUrl(theme) {
    var giscus = document.querySelector("script[data-theme-light][data-theme-dark]");
    return giscus ? giscus.getAttribute("data-theme-" + theme) : null;
  }

  function postGiscusTheme(theme, frame) {
    var themeUrl = giscusThemeUrl(theme);
    if (!themeUrl || !frame || !frame.contentWindow) return;

    frame.contentWindow.postMessage(
      { giscus: { setConfig: { theme: themeUrl } } },
      "https://giscus.app"
    );
  }

  function updateGiscusTheme(theme) {
    document.querySelectorAll(".giscus-frame").forEach(function (frame) {
      postGiscusTheme(theme, frame);
    });
  }

  function observeGiscusTheme(theme) {
    var comments = document.querySelector(".comments");
    if (!comments || !("MutationObserver" in window)) return;

    function configureFrame(frame) {
      if (frame.dataset.themeControllerBound === "true") return;
      frame.dataset.themeControllerBound = "true";
      frame.addEventListener("load", function () {
        postGiscusTheme(document.documentElement.dataset.theme, frame);
      });
      postGiscusTheme(theme, frame);
    }

    comments.querySelectorAll(".giscus-frame").forEach(configureFrame);
    new MutationObserver(function () {
      comments.querySelectorAll(".giscus-frame").forEach(configureFrame);
    }).observe(comments, { childList: true, subtree: true });
  }

  function setupThemeController() {
    var themeButton = document.querySelector(".md2-theme-toggle");

    function applyTheme(theme) {
      var dark = theme === "dark";
      document.documentElement.dataset.theme = theme;

      if (themeButton) {
        themeButton.setAttribute("aria-pressed", String(dark));
        themeButton.setAttribute("aria-label", dark ? "Switch to light theme" : "Switch to dark theme");
      }

      updateGiscusTheme(theme);
    }

    applyTheme(preferredTheme());
    observeGiscusTheme(document.documentElement.dataset.theme);

    if (!themeButton) return;

    themeButton.addEventListener("click", function () {
      var nextTheme = document.documentElement.dataset.theme === "dark" ? "light" : "dark";
      try {
        window.localStorage.setItem(themeStorageKey, nextTheme);
      } catch (_error) {
        themeButton.dataset.storageUnavailable = "true";
      }
      applyTheme(nextTheme);
    });
  }

  setupThemeController();

  function normalizePath(pathname) {
    if (!pathname) return "/";
    var normalized = pathname.replace(/\/+$/, "");
    return normalized || "/";
  }

  function markCurrentTopNav() {
    var currentPath = normalizePath(window.location.pathname);
    var topLinks = Array.prototype.slice.call(document.querySelectorAll("nav.top a[href]"));

    topLinks.forEach(function (link) {
      var linkPath;

      try {
        linkPath = normalizePath(new URL(link.href || link.getAttribute("href"), document.baseURI || window.location.href).pathname);
      } catch {
        linkPath = "";
      }

      var descendantPrefix = linkPath.concat("/");
      var active = linkPath === "/"
        ? currentPath === "/"
        : currentPath === linkPath || currentPath.indexOf(descendantPrefix) === 0;

      link.classList.toggle("is-current", active);
      if (active) {
        link.setAttribute("aria-current", "page");
      } else {
        link.removeAttribute("aria-current");
      }
    });
  }

  markCurrentTopNav();

  function setupDrawer() {
    var menuButton = document.querySelector(".md2-menu-button");
    var drawer = document.querySelector(".md2-drawer");
    var scrim = document.querySelector(".md2-drawer-scrim");

    if (!menuButton || !drawer || !scrim || !window.matchMedia) return;

    var desktop = window.matchMedia("(min-width: 1024px)");
    function setExpanded(open) {
      menuButton.setAttribute("aria-expanded", String(open));
    }

    function setButtonLabel(label) {
      menuButton.setAttribute("aria-label", label);
    }

    function syncDrawerButton() {
      setExpanded(isOpen());
      setButtonLabel(isOpen() ? "Close navigation" : "Open navigation");
    }

    function isOpen() {
      return document.body.classList.contains("md2-drawer-open");
    }

    function closeDrawer(restoreFocus) {
      if (!isOpen()) return;

      document.body.classList.remove("md2-drawer-open");
      syncDrawerButton();

      if (restoreFocus && !desktop.matches) {
        menuButton.focus();
      }
    }

    function openDrawer() {
      if (desktop.matches) return;

      document.body.classList.add("md2-drawer-open");
      syncDrawerButton();

      var firstLink = drawer.querySelector("a[href]");
      if (firstLink) {
        firstLink.focus();
      }
    }

    menuButton.addEventListener("click", function () {
      if (isOpen()) {
        closeDrawer(true);
      } else {
        openDrawer();
      }
    });

    scrim.addEventListener("click", function () {
      closeDrawer(true);
    });

    drawer.querySelectorAll("a[href]").forEach(function (link) {
      link.addEventListener("click", function () {
        closeDrawer(false);
      });
    });

    document.addEventListener("keydown", function (event) {
      if (event.key === "Escape") {
        closeDrawer(true);
      }
    });

    function handleBreakpointChange() {
      if (desktop.matches) {
        closeDrawer(false);
      }
      syncDrawerButton();
    }

    if (desktop.addEventListener) {
      desktop.addEventListener("change", handleBreakpointChange);
    } else {
      desktop.addListener(handleBreakpointChange);
    }

    handleBreakpointChange();
  }

  setupDrawer();

  var sidebar = document.querySelector(".section-sidebar");
  if (!sidebar) return;

  var links = Array.prototype.slice.call(sidebar.querySelectorAll("a[data-section-id]"));
  if (links.length === 0) return;

  var byId = {};
  var headings = links
    .map(function (link) {
      var id = link.getAttribute("data-section-id");
      if (!id) return null;
      var heading = document.getElementById(id);
      if (!heading) return null;
      byId[id] = link;
      return heading;
    })
    .filter(Boolean);

  if (headings.length === 0) return;

  function setCurrent(id) {
    links.forEach(function (link) {
      var active = link.getAttribute("data-section-id") === id;
      link.classList.toggle("is-current", active);
      if (active) {
        link.setAttribute("aria-current", "true");
      } else {
        link.removeAttribute("aria-current");
      }
    });
  }

  function scrollOffset() {
    var styles = window.getComputedStyle(document.documentElement);
    var padding = parseFloat(styles.scrollPaddingTop || styles.getPropertyValue("scroll-padding-top"));
    return Number.isFinite(padding) ? padding : 0;
  }

  function currentByScroll() {
    var current = headings[0];
    var activationLine = scrollOffset() + 80;

    headings.forEach(function (heading) {
      if (heading.getBoundingClientRect().top <= activationLine) {
        current = heading;
      }
    });

    setCurrent(current.id);
  }

  function prefersReducedMotion() {
    return window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  }

  function scrollToHeading(heading) {
    var targetY = window.scrollY + heading.getBoundingClientRect().top - scrollOffset();
    window.scrollTo({ top: Math.max(0, targetY), behavior: prefersReducedMotion() ? "auto" : "smooth" });
  }

  links.forEach(function (link) {
    link.addEventListener("click", function (event) {
      var id = link.getAttribute("data-section-id");
      var heading;

      if (id) {
        heading = document.getElementById(id);
        if (heading) {
          event.preventDefault();
          history.pushState(null, "", link.getAttribute("href"));
          scrollToHeading(heading);
        }
        setCurrent(id);
      }
    });
  });

  var visible = {};
  var observer;

  if ("IntersectionObserver" in window) {
    observer = new IntersectionObserver(
      function (entries) {
        entries.forEach(function (entry) {
          if (entry.isIntersecting) {
            visible[entry.target.id] = entry.boundingClientRect.top;
          } else {
            delete visible[entry.target.id];
          }
        });

        var currentId = Object.keys(visible).sort(function (left, right) {
          return visible[left] - visible[right];
        })[0];

        if (currentId && byId[currentId]) {
          setCurrent(currentId);
        } else {
          currentByScroll();
        }
      },
      {
        rootMargin: "-20% 0px -65% 0px",
        threshold: 0
      }
    );

    headings.forEach(function (heading) {
      observer.observe(heading);
    });
  }

  currentByScroll();
  window.addEventListener("scroll", currentByScroll, { passive: true });
  window.addEventListener("resize", currentByScroll);
})();
