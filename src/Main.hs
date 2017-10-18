{-# LANGUAGE OverloadedStrings #-} {- -*- Coding: utf-8 -*- -}

module Main where

import System.Environment

import Control.Monad (forM_)
import Control.Concurrent.MVar

import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Aeson

import qualified Codec.Binary.UTF8.String as US

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Route

import Line.Messaging.API
import Line.Messaging.Webhook ( Event(..), EventMessage(..), ReplyToken(..)
                              , ReplyableEvent(..), webhookApp
                              , defaultOnFailure, getMessage, getReplyToken)
import Line.Messaging.Webhook.Types
import Line.Messaging.Types (Text(..), getText)
import Line.Messaging.Common.Types (ID)

import InitUsers
import ExerciseList

import Core
import Answers
import App

main :: IO ()
main = do
  args <- getArgs
  port <- maybe 80 read <$> lookupEnv "PORT" :: IO Int
  mvU <- newMVar $ nub initUsers
  mvC <- newMVar initMVcomm
  mvR <- newMVar Map.empty
  mvT <- newMVar $ fromInteger 0
  let mv = MV { mvUser = mvU, mvComm = mvC, mvReward = mvR, mvTime = mvT }
  run port $ route
    [ ("line-bot", const (lineBot mv))
    , ("comment-reset", const (commentReset mv))
    , ("comment-test", const (commentTest mv))
    , ("comment", const (comment mv))
    , ("user-merge", const (userMerge mv))
    , ("user-show", const (userShow mv))
    , ("exercise", const (exercise mv))
    , ("search-trial", const searchTrial)
    , ("thetrial", const theTrial)
    ]
  
-- line-bot

lineBot :: MV -> Application
lineBot mv req f = do
  channelSecret <- getChannelSecret
  webhookApp channelSecret (handler mv) defaultOnFailure req f

handler :: MV -> [Event] -> IO ()
handler mv events = forM_ events (handleEvent mv)

handleEvent :: MV -> Event -> IO ()
handleEvent mv (MessageEvent  event) = (handleMessageEvent  mv) event
handleEvent mv (PostbackEvent event) = (handlePostbackEvent mv) event
handleEvent mv (FollowEvent   event) = (handleFollowEvent   mv) event
handleEvent _ _ = return ()

handleMessageEvent :: MV -> ReplyableEvent EventMessage -> IO ()
handleMessageEvent mv event = 
  case getMessage event of
   TextEM mid (Text text) -> handleText mv event text
   _                      -> react event

handleFollowEvent mv event = addUser mv $ getUID event

-- comment

commentReset :: MV -> Application
commentReset mv _ respond = do
  let mvC = mvComm mv
  comms <- takeMVar mvC
  putMVar mvC initMVcomm
  respond $ responseLBS status200 [] "reset done"

commentTest :: MV -> Application
commentTest mv _ respond = do
  storeComment mv "テスト"
  respond $ responseLBS status200 [] "test"

comment :: MV -> Application
comment mv req respond = do
  rp <- case queryString req of
    []       -> return $ ((2, "error: specify number") :: TheResponse)
    (no,_):_ -> do
      comms <- readMVar $ mvComm mv
      let
        commsList = Map.toList comms
        commsListBS8 = zip (map (BS8.pack . show . fst) commsList) (map snd commsList)
      case lookup no commsListBS8 of
        Nothing   -> return $ (1, "no such number yet")
        Just text -> return $ (0, text)
  respond $ responseLBS status200 [(hContentType, "application/json")] $ encode rp
