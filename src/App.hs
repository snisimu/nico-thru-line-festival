{-# LANGUAGE OverloadedStrings #-} {- -*- Coding: utf-8 -*- -}

module App where

import System.Environment
import System.Random hiding (split)

import Control.Monad
import Control.Concurrent.MVar

import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Time.Clock.POSIX
import Data.Aeson

import qualified Codec.Binary.UTF8.String as US

import Text.Parsec
import Text.Parsec.Char

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

import MyLib
import Core
import Answers
import Search

handleText :: MV -> ReplyableEvent a -> T.Text -> IO ()
handleText mv event text = do
  let
    mvU = mvUser mv
    uid = getUID event
    hChar = T.head text
  addUser mv uid
  storeComment mv text

isHelp text =
  (map toLower (T.unpack text) == "help")
  || (text == "ヘルプ")
  || (text == "へるぷ")
help event = do
  echo (getReplyToken event) $ T.init $ T.unlines $ intersperse ""
    [ ("*なんとか → 「なんとか」を画面に流します" :: T.Text)
    , "用語？ → webノート内で「用語」をググります"
    , "Q1-2 → 問題1-2にトライします"
    ]

exercise :: MV -> Application
exercise mv req respond = case queryString req of
  (n, _) : (m, _) : _ -> do
    let
      n' = BS8.unpack n
      m' = BS8.unpack m
    case lookup n' exerciseList of
      Nothing -> do
        respond $ responseLBS status200 [] "exercise: error - incorrect coursework number1"
      Just exerL -> case lookup m' exerL of
        Nothing -> do
          respond $ responseLBS status200 [] "exercise: error - incorrect coursework number2"
        Just exer -> do
          users <- readMVar $ mvUser mv
          sequence_ $ map (pushButtons mv (n',m') exer) users
          sequence_ $ map (prepareReward mv $ length exerL) users
          respond $ responseLBS status200 [] "exercise: done"
  _ -> respond $ responseLBS status200 [] "exercise: error - specify two coursework numbers"

handlePostbackEvent :: MV -> ReplyableEvent Postback -> IO ()
handlePostbackEvent mv event = do
  let
    mvR = mvReward mv
    pbText = T.unpack $ getPostback event
    uid = getUID event
    mbCorrect = if pbText == postbackCorrect
      then Just True
      else if pbText == postbackWrong
        then Just False
        else Nothing
  case mbCorrect of
    Nothing      -> return () -- unlikely
    Just correct -> do
      if correct
      then do
        pushText uid "正解です！"
        storeComment mv "正解です！"
      else do
        pushText uid "ちがいます"
      rewardMap <- takeMVar mvR
      case Map.lookup uid rewardMap of
        Just (NotYet, stks) -> do
          stks' <- if correct 
            then do
              pushSticker uid $ head stks
              return $ if null (tail stks) then stks else tail stks
            else return stks
          putMVar mvR $ Map.insert uid (Done, stks') rewardMap
        _                   -> putMVar mvR rewardMap -- invoked by user

-- 

react :: ReplyableEvent a -> IO ()
react event = echo (getReplyToken event) =<< randomChoice anss

storeComment :: MV -> T.Text -> IO ()
storeComment mv text = do
  let mvc = mvComm mv
  comms <- takeMVar mvc
  let mx = maximum $ map fst $ Map.toList comms
  putMVar mvc $ Map.insert (mx+1) text comms

prepareReward mv n uid = do
  let mvR = mvReward mv
  rewardMap <- takeMVar mvR
  case Map.lookup uid rewardMap of
    Just (_, stks) -> do
      putMVar mvR $ Map.insert uid (NotYet, stks) rewardMap
    Nothing -> do
      let rewardss = split (length rewardList `div` n) rewardList
      stks <- mapM randomChoice rewardss
      putMVar mvR $ Map.insert uid (NotYet, stks) rewardMap

-- push

pushButtons mv (n,m) (ask, ops) user = do
  buttActs <- makeButtonsActions ops
  api $ push user $ return $ Message $ Template
    { getTemplateAltText = "問題です"
    , getTemplate = Buttons
      { getButtonsThumbnailURL = Nothing
          -- Just "https://upload.wikimedia.org/wikipedia/commons/e/e0/JPEG_example_JPG_RIP_050.jpg"
      , getButtonsTitle = Just $ T.pack $ "Q:" ++ n ++ "-" ++ m
      , getButtonsText = ask
      , getButtonsActions = buttActs
      }
    }
  time <- getPOSIXTime
  let mvT = mvTime mv
  _ <- takeMVar mvT
  putMVar mvT time
    where
    makeButtonsActions (op:ops) = do
      let
        cas = makeCA True op : map (makeCA False) ops
        cass = permutations cas
      randomChoice cass
    makeCA bool text = TplPostbackAction
      text
      (T.pack $ if bool then postbackCorrect else postbackWrong)
      $ Just $ (T.pack "A:") `T.append` text

pushSticker uid (n,m) = do
  api $ push uid $ return $
    Message $ Sticker { getPackageID = T.pack $ show m, getStickerID = T.pack $ show n }
  return ()

-- search

searchTrial :: Application
searchTrial req respond = do
  respond $ responseFile status200 [] "./src/SearchTrial.html" Nothing

-- example

confirm = Message $ Template
  { getTemplateAltText = "ALT"
  , getTemplate = Confirm
    { getConfirmText = "abc"
    , getConfirmActions =
      [ TplPostbackAction "L0" "PB0" (Just "MB0")
      , TplPostbackAction "L1" "PB1" (Just "MB1")
      ]
    }
  }

{- static example
exerciseImage :: Application
exerciseImage req respond = do
  respond $ responseFile status200 [] "./src/excercise.jpg" Nothing
-}

-- 

theTrial :: Application
theTrial req respond = case queryString req of
  (txt,tit):(_,ur):_ -> do
    api $ push "U8bcbe51bd44841268549ae791c6dab4f" $ return $
      Message $ Template
      { getTemplateAltText = "ALT"
      , getTemplate = Buttons
        { getButtonsThumbnailURL = maybe Nothing (Just . Data.Text.Encoding.decodeUtf8) ur
        , getButtonsTitle = maybe Nothing (Just . Data.Text.Encoding.decodeUtf8) tit
        , getButtonsText = T.pack $ BS8.unpack txt
        , getButtonsActions =
          [ TplPostbackAction "L0" "PB0" Nothing
          , TplURIAction "Link" "http://www.oita-ct.ac.jp/seigyo/nishimura_hp/2S-ALH/07-26/main.html"
          ]
        }
      }
    respond $ responseLBS status200 [(hContentType, "text/plain")] "message pushed"
  _ -> respond $ responseLBS status200 [(hContentType, "text/plain")] "no operation"
