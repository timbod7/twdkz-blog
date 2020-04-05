{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Lens
import           Control.Monad
import           Data.Aeson                 as A
import           Data.Aeson.Lens
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
    } deriving (Generic, Show, FromJSON, ToJSON)

-- | Data for a blog post
data Post =
    Post { title   :: String
         , author  :: String
         , content :: String
         , url     :: String
         , date    :: String
         , image   :: Maybe String
         }
    deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

data AtomData = AtomData
  { atomTitle :: String
  , atomDomain :: String
  , atomAuthor :: String
  , atomPosts :: [Post]
  , atomCurrentTime :: String
  , atomUrl :: String
  } deriving (Generic, Eq, Ord, Show)

instance ToJSON AtomData where
  toJSON AtomData{..} = object
    [ "title" A..= atomTitle
    , "domain" A..= atomDomain
    , "author" A..= atomAuthor
    , "posts" A..= atomPosts
    , "currentTime" A..= atomCurrentTime
    , "url" A..= atomUrl
    ]

buildIndex :: [Post] -> Action ()
buildIndex posts' = do
  indexT <- compileTemplate' "site/templates/index.html"
  let sortedPosts = reverse (sortOn date posts')
      indexInfo = IndexInfo {posts = sortedPosts}
      indexHTML = T.unpack $ substitute indexT (withSiteMeta $ toJSON indexInfo)
  writeFile' (outputFolder </> "index.html") indexHTML

toIsoDate :: UTCTime -> String
toIsoDate = formatTime defaultTimeLocale (iso8601DateFormat rfc3339)
  where
    rfc3339 = Just "%H:%M:%SZ"

buildFeed :: [Post] -> FilePath -> Action ()
buildFeed posts toFilePath = do
  now <- liftIO getCurrentTime
  let atomData =
        AtomData
          { atomTitle = "Tim Docker"
          , atomDomain = "https://tim.dockerz.net"
          , atomAuthor = "Tim Docker"
          , atomPosts = posts
          , atomCurrentTime = toIsoDate now
          , atomUrl = "/atom.xml"
          }
  atomTempl <- compileTemplate' "site/templates/atom.xml"
  writeFile' toFilePath . T.unpack $ substitute atomTempl (toJSON atomData)

-- | Find and build all posts
buildPosts :: Action [Post]
buildPosts = do
  pPaths <- getDirectoryFiles "." ["site/posts//*.md"]
  forP pPaths buildPost

-- | Load a post, process metadata, write it to output, then return the post object
-- Detects changes to either post content or template
buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  postContent <- readFile' srcPath
  -- load post content and metadata as JSON blob
  postData <- markdownToHTML . T.pack $ postContent
  let postUrl = T.pack . dropDirectory1 $ srcPath -<.> "html"
      withPostUrl = _Object . at "url" ?~ String postUrl
  -- Add additional metadata we've been able to compute
  let fullPostData = withSiteMeta . withPostUrl $ postData
  template <- compileTemplate' "site/templates/post.html"
  writeFile' (outputFolder </> T.unpack postUrl) . T.unpack $ substitute template fullPostData
  post <- convert fullPostData
  let datadir = dropExtensions srcPath
      postdir = dropDirectory1 datadir
  -- Copy static files from the per-post direcory
  -- (Same name as the most, without the .md ext)
  hasStatic <- doesDirectoryExist datadir
  when hasStatic $ do
    staticpaths <- getDirectoryFiles datadir ["*"]
    void $ forP staticpaths $ \filepath ->
       copyFileChanged (datadir </> filepath) (outputFolder </> postdir </> filepath)
  return post

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
  allPosts <- buildPosts
  buildIndex allPosts
  buildFeed allPosts (outputFolder </> "atom/all.xml")
  copyStaticFiles

main :: IO ()
main = do
  let shOpts = shakeOptions { shakeVerbosity = Chatty, shakeLintInside = ["\\"]}
  shakeArgsForward shOpts buildRules
