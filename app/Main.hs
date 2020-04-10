{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Lens
import           Control.Monad
import           Data.Aeson                 as A
import           Data.Aeson.TH
import           Data.Aeson.Lens
import           Data.Foldable(for_)
import           Data.List(sortOn)
import           Data.Time
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.Forward
import           Development.Shake.FilePath
import           GHC.Generics               (Generic)
import           Slick

import qualified Data.HashMap.Lazy as HML
import qualified Data.Text                  as T
import qualified Data.Map                   as M
import qualified Data.Set                   as S

---Config-----------------------------------------------------------------------

siteMeta :: SiteMeta
siteMeta =
    SiteMeta { siteAuthor = "Tim Docker"
             , baseUrl = "https://tim.dockerz.net"
             , siteTitle = "Haskell, electronics et al."
             , twitterHandle = Nothing
             , githubUser = Just "timbod7"
             }

outputFolder :: FilePath
outputFolder = "docs/"

--Data models-------------------------------------------------------------------

withSiteMeta :: Value -> Value
withSiteMeta (Object obj) = Object $ HML.union obj siteMetaObj
  where
    Object siteMetaObj = toJSON siteMeta
withSiteMeta _ = error "only add site meta to objects"

data SiteMeta =
    SiteMeta { siteAuthor    :: String
             , baseUrl       :: String -- e.g. https://example.ca
             , siteTitle     :: String
             , twitterHandle :: Maybe String -- Without @
             , githubUser    :: Maybe String
             }
    deriving (Generic, Eq, Ord, Show, ToJSON)

-- | Data for the index page
data IndexInfo =
  IndexInfo
    { posts :: [Post]
    } deriving (Generic, Show)

-- | Data for a page
data Page =
    Page { page_title   :: String
         , page_author  :: String
         , page_content :: String
         , page_url     :: String
         }
    deriving (Generic, Eq, Ord, Show, Binary)

-- | Data for a blog post
data Post =
    Post { post_title   :: String
         , post_author  :: String
         , post_content :: String
         , post_url     :: String
         , post_date    :: Maybe String
         , post_image   :: Maybe String
         , post_tags    :: [String]
         }
    deriving (Generic, Eq, Ord, Show, Binary)

data AtomData = AtomData
  { atom_title :: String
  , atom_domain :: String
  , atom_author :: String
  , atom_posts :: [Post]
  , atom_currentTime :: String
  , atom_url :: String
  } deriving (Generic, Eq, Ord, Show)


buildIndex :: FilePath -> String -> [Post] -> [String] -> Action ()
buildIndex filePath title posts' tags = do
  indexT <- compileTemplate' "site/templates/index.html"
  let sortedPosts = reverse (sortOn post_date posts')
      indexInfo = IndexInfo {posts = sortedPosts}
      jsonContext = withTitle title $ withTags tags $ withSiteMeta $ toJSON indexInfo
      indexHTML = T.unpack $ substitute indexT jsonContext
  writeFile' (outputFolder </> filePath) indexHTML
  where
    withTags :: [String] -> Value -> Value
    withTags [] = _Object . at "tags" ?~ toJSON ([]::[T.Text])
    withTags tags = _Object . at "tags" ?~ toJSON [tags]

    withTitle :: String -> Value -> Value
    withTitle title = _Object . at "title" ?~ toJSON title

toIsoDate :: UTCTime -> String
toIsoDate = formatTime defaultTimeLocale (iso8601DateFormat rfc3339)
  where
    rfc3339 = Just "%H:%M:%SZ"

buildFeed :: [Post] -> FilePath -> Action ()
buildFeed posts toFilePath = do
  now <- liftIO getCurrentTime
  let atomData =
        AtomData
          { atom_title = "Tim Docker"
          , atom_domain = "https://tim.dockerz.net"
          , atom_author = "Tim Docker"
          , atom_posts = posts
          , atom_currentTime = toIsoDate now
          , atom_url = "/atom/" <> takeFileName toFilePath
          }
  atomTempl <- compileTemplate' "site/templates/atom.xml"
  writeFile' toFilePath . T.unpack $ substitute atomTempl (toJSON atomData)

-- | Load a page, process metadata, write it to output, then return the page object
-- Detects changes to either page content or template
buildPage :: (Typeable a, FromJSON a, Binary a, Show a) => FilePath -> FilePath -> Action a
buildPage templatePath srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding page: " <> srcPath
  pageContent <- readFile' srcPath
  -- load page content and metadata as JSON blob
  pageData <- markdownToHTML . T.pack $ pageContent
  let pageUrl = T.pack . dropDirectory1 $ srcPath -<.> "html"
      withPageUrl = _Object . at "url" ?~ String ("/" <> pageUrl)
  -- Add additional metadata we've been able to compute
  let fullPageData = withSiteMeta . withPageUrl $ pageData
  template <- compileTemplate' templatePath
  writeFile' (outputFolder </> T.unpack pageUrl) . T.unpack $ substitute template fullPageData
  page <- convert fullPageData
  let datadir = dropExtensions srcPath
      pagedir = dropDirectory1 datadir
  -- Copy static files from the per-page direcory
  -- (Same name as the most, without the .md ext)
  hasStatic <- doesDirectoryExist datadir
  when hasStatic $ do
    staticpaths <- getDirectoryFiles datadir ["*"]
    void $ forP staticpaths $ \filepath ->
       copyFileChanged (datadir </> filepath) (outputFolder </> pagedir </> filepath)
  return page

-- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Action ()
copyStaticFiles = do
    filepaths <- getDirectoryFiles "./site/" ["images//*", "css//*", "js//*", "fonts//*.ttf"]
    void $ forP filepaths $ \filepath ->
        copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

-- | Specific build rules for the Shake system
--   defines workflow to build the website
buildRules :: Action ()
buildRules = do
  -- Pages
  do
    paths <- getDirectoryFiles "." ["site/pages/*.md"]
    forP paths (buildPage "site/templates/page.html") :: Action [Page]

  -- Posts
  allPosts <- do
    paths <- getDirectoryFiles "." ["site/posts/*.md"]
    forP paths (buildPage "site/templates/post.html" )

  -- Post indexes and feeds
  let allTags = (S.toList . S.fromList . concat) [post_tags p | p <- allPosts]
      postsByTag = M.fromListWith (<>) (concat [[(tag,[p]) | tag <- post_tags p] | p <- allPosts])
  for_ (M.toList postsByTag) $ \(tag,posts) -> do
    buildIndex ("tags" </> tag <> ".html") ("Posts with tag: " <> tag) posts []
    buildFeed posts (outputFolder </> "atom" </> (tag <> ".xml"))
  buildIndex "index.html" "All Posts" allPosts allTags
  buildFeed allPosts (outputFolder </> "atom/all.xml")

  copyStaticFiles

main :: IO ()
main = do
  let shOpts = shakeOptions { shakeVerbosity = Chatty, shakeLintInside = ["\\"]}
  shakeArgsForward shOpts buildRules

$(deriveJSON defaultOptions{fieldLabelModifier=drop 5} ''Page)
$(deriveJSON defaultOptions{fieldLabelModifier=drop 5} ''Post)
$(deriveJSON defaultOptions{fieldLabelModifier=drop 5} ''AtomData)
$(deriveJSON defaultOptions ''IndexInfo)

