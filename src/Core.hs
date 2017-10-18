{-# LANGUAGE OverloadedStrings #-} {- -*- Coding: utf-8 -*- -}

module Core where

import System.Environment
import System.Random hiding (split)

import Control.Concurrent.MVar

import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS8
import Data.Time.Clock
import Data.Aeson

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

herokuSite = "https://systemeng-thru-line-server.herokuapp.com"

postbackCorrect = "correct"
postbackWrong   = "wrong"

data FirstAnswer = NotYet | Done

data MV = MV
  { mvUser :: MVar [T.Text]
  , mvComm :: MVar (Map.Map Int T.Text)
  , mvReward :: MVar (Map.Map T.Text (FirstAnswer, [(Int, Int)]))
  , mvTime :: MVar NominalDiffTime
  }
type TheResponse = (Int, T.Text)

initMVcomm :: Map.Map Int T.Text
initMVcomm = Map.fromList [(0, "init")]

getChannelSecret :: IO ChannelSecret
getChannelSecret = getEnv "CHANNEL_SECRET" >>= return . T.pack

getChannelToken :: IO ChannelAccessToken
getChannelToken = getEnv "CHANNEL_TOKEN" >>= return . T.pack

api :: APIIO a -> IO (Either APIError a)
api = runAPI getChannelToken

echo :: ReplyToken -> T.Text -> IO ()
echo replyToken content = do
  api $ reply replyToken [ Message . Text $ content ]
  return ()

rewardList :: [(Int, Int)]
rewardList = map snd $ sort
  [ (99, (276,4))
  , (89, (277,4))
  , (6, (263,4))
  , (21, (268,4))
  , (81, (278,4))
  , (89, (282,4))
  , (91, (283,4))
  , (7, (293,4))
  , (14, (296,4))
  , (31, (297,4))
  , (14, (298,4))
  , (19, (302,4))
  , (71, (303,4))
  , (28, (304,4))
  , (59, (306,4))
  , (78, (307,4))
  , (72, (608,4))
  , (52, (620,4))
  , (51, (305,4))
  , (27, (299,4))
  , (48, (267,4))
  , (18, (2,1))
  , (77, (4,1))
  , (87, (5,1))
  , (37, (13,1))
  , (83, (14,1))
  , (23, (125,1))
  , (17, (134,1))
  , (53, (137,1))
  , (31, (138,1))
  , (63, (139,1))
  , (47, (407,1))
  , (63, (427,1))
  , (8, (144,2))
  , (82, (171,2))
  , (63, (179,2))
  , (58, (527,2))
  , (36, (184,3))
  , (27, (200,3))
  , (41, (213,3))
  ]

divideInto :: Int -> [a] -> [[a]]
divideInto n xs = split (((length xs) + n - 1) `div` n) xs

split :: Int -> [a] -> [[a]]
split _ [] = []
split m xs = xs1:(split m xs2)
  where
    (xs1, xs2) = splitAt m xs

-- user

getUID = getID . getSource

addUser mv uid = do
  let mvU = mvUser mv
  users <- takeMVar mvU
  putMVar mvU $ union users [uid]

userMerge :: MV -> Application
userMerge mv req respond = do
  let
    mvU = mvUser mv
    mvC = mvComm mv
    us = map (T.pack . BS8.unpack . fst) $ queryString req
  users <- takeMVar mvU
  putMVar mvU $ union users us
  respond $ responseLBS status200 [] "user merge done"

userShow :: MV -> Application
userShow mv req respond = do
  users <- readMVar $ mvUser mv
  respond $ responseLBS status200 [(hContentType, "application/json")] $ encode users

--

pushText user txt = do
  api $ push user $ return $ Message $ Text txt
  return ()
