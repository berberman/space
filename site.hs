{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Hakyll

main :: IO ()
main = hakyll $ do
  match (fromList ["favicon.ico", "CNAME", "README.md"]) $ do
    route idRoute
    compile copyFileCompiler

  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "contact.md" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "posts/*" $ do
    route pRoute
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Archives"
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts)
              `mappend` defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx `mappend` bodyField "description"
      posts <-
        fmap (take 10) . recentFirst
          =<< loadAllSnapshots "posts/*" "content"
      renderAtom myFeedConfiguration feedCtx posts

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` defaultContext

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration =
  FeedConfiguration
    { feedTitle = "SpaceHealthy cooking: latest recipes",
      feedDescription = "berberman's space",
      feedAuthorName = "berberman",
      feedAuthorEmail = "berberman@yandex.com",
      feedRoot = "https://berberman.space"
    }

pRoute :: Routes
pRoute = metadataRoute $ \m ->
  case lookupString "title" m of
    Just x -> constRoute $ f x <> ".html"
    Nothing -> error "no"
  where
    f = T.unpack . T.replace " " "-" . T.toLower . T.pack
