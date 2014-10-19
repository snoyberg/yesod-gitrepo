{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.GitRepo where

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

data GitRepo a = GitRepo
    { grRefresh :: IO ()
    , grContent :: IO a
    }

gitRepo :: Text -- ^ URL
           -> Text -- ^ branch name
           -> (FilePath -> IO a) -- ^ what to do on clone/refresh
           -> IO (GitRepo a)
gitRepo url branch refresh = do
    tmpDir <- getTemporaryDirectory
    contentDir' <- createTempDirectory tmpDir "git-repo"
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

    return GitRepo
        { grRefresh = void $ tryPutMVar var ()
        , grContent = readIORef ref
        }

instance RenderRoute (GitRepo a) where
    data Route (GitRepo a) = GitRepoRoute
        deriving (Show, Eq, Read)
    renderRoute _ = ([], [])

instance ParseRoute (GitRepo a) where
    parseRoute ([], []) = Just GitRepoRoute
    parseRoute _ = Nothing

instance YesodSubDispatch (GitRepo a) (HandlerT site IO) where
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
