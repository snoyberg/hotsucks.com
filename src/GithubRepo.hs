{-# LANGUAGE FlexibleInstances #-}
module GithubRepo where

import ClassyPrelude.Yesod
import System.IO.Temp (createTempDirectory)
import System.Directory (getTemporaryDirectory, removeDirectory)
import Yesod.Core.Types
import Network.Wai
import Network.HTTP.Types
import Control.Concurrent (forkIO)
import Control.Exception (mask_)
import System.Process
import System.Exit

data GithubRepo a = GithubRepo
    { grRefresh :: IO ()
    , grContent :: IO a
    }

githubRepo :: Text -- ^ URL
           -> Text -- ^ branch name
           -> (FilePath -> IO a) -- ^ what to do on clone/refresh
           -> IO (GithubRepo a)
githubRepo url branch refresh = do
    tmpDir <- getTemporaryDirectory
    contentDir' <- createTempDirectory tmpDir "github-repo"
    let contentDir = fpFromString contentDir'
    removeDirectory contentDir'
    git Nothing ["clone", "-b", branch, url, pack contentDir']
    ref <- refresh contentDir >>= newIORef
    var <- newEmptyMVar
    mask_ $ void $ forkIO $ forever $ do
        takeMVar var
        void $ tryAny $ do
            git (Just contentDir) ["fetch"]
            git (Just contentDir) ["reset", "--hard", "origin/" <> branch]
            refresh contentDir >>= writeIORef ref

    return GithubRepo
        { grRefresh = void $ tryPutMVar var ()
        , grContent = readIORef ref
        }

instance RenderRoute (GithubRepo a) where
    data Route (GithubRepo a) = GithubRepoRoute
        deriving (Show, Eq, Read)
    renderRoute _ = ([], [])

instance ParseRoute (GithubRepo a) where
    parseRoute ([], []) = Just GithubRepoRoute
    parseRoute _ = Nothing

instance YesodSubDispatch (GithubRepo a) (HandlerT site IO) where
    yesodSubDispatch env req send = do
        void $ forkIO $ grRefresh gr
        send $ responseLBS status200 [("Content-Type", typePlain)]
            "Reload initiated"
      where
        gr = ysreGetSub env $ yreSite $ ysreParentEnv env

git :: Maybe FilePath -> [Text] -> IO ()
git mdir args = do
    (Nothing, Nothing, Nothing, ph) <- createProcess
        (proc "git" $ map unpack args)
            { cwd = fpToString <$> mdir
            }
    ec <- waitForProcess ph
    case ec of
        ExitSuccess -> return ()
        ExitFailure i -> error $ concat
            [ "Ran git in dir "
            , show mdir
            , " with args "
            , show args
            , " failed with exit code "
            , show ec
            ]