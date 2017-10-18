{-# LANGUAGE OverloadedStrings #-} {- -*- Coding: utf-8 -*- -}

module Search where

import System.Exit
import System.Process

import Control.Monad

import Data.Maybe
import Data.List
import Data.List.Utils
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.ByteString.Base16

import qualified Codec.Binary.UTF8.String as UTF

import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
import Text.XML.HXT.DOM.FormatXmlTree
import Text.XML.HXT.Arrow.XmlArrow

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char

import qualified Codec.Binary.UTF8.String as US

import Network.HTTP.Simple
import Network.HTTP.Conduit
import Network.HTTP.Types.URI

import Numeric

import Core

num = 1 -- number to show

search :: T.Text -> T.Text -> IO ()
search uid term = do
  pushText uid $ "えーっと" `T.append` term `T.append` "は…"
  req <- parseRequest $
    "https://www.google.co.jp/search?q=site:www.oita-ct.ac.jp/seigyo/nishimura_hp/SystemEngineering "
      ++ T.unpack term
  let setH = setRequestHeaders
        [ ("Content-Type", "text/html; charset=utf-8")
        , ("Content-Language", "ja")
        , ("Accept-Language", "ja")
        , ("Accepe-Charset", "UTF-8")
        ]
  res <- httpLBS $ setH req
  let body = BSL8.unpack $ getResponseBody res
  -- BSL8.putStrLn $ BSL8.take 50000 $ body -- [debug]
  -- T.putStrLn $ T.take 10000 $ Data.Text.Encoding.decodeUtf8 $ BSL8.toStrict $ body
  -- writeFile "1.txt" $ take 10000 $ UTF.encodeString $ BSL8.unpack body
  hrefs <- runX $ readString [ withParseHTML True ] body //> arrHref
  texts <- runX $ readString [ withParseHTML True ] body //> arrText >>> writeDocumentToString []
  let
    parserHref :: Parser String
    parserHref = do
      manyTill anyChar $ string "http://"
      body <- manyTill anyChar $ char '&'
      return $ "http://" ++ body
    refine = foldr1 (.) $ map (flip replace "") ["<b>", "</b>", "<br/>"]
    hrefs' = map (\s -> either (const s) id $ parse parserHref "" s) hrefs
    texts' = map refine texts
    tx'lns = zip texts' hrefs'
  if null tx'lns
  then pushText uid "すみませんわかりません"
  else forM_ (take num $ tx'lns) $ \(tx,ln) -> do
    u8 <- sjisEncToUTF8Enc (BS8.unpack $ urlEncode True $ BS8.pack tx)
    -- let u8' = Data.Text.Encoding.decodeUtf8 $ urlDecode True $ BS8.pack u8 -- also works
    let u8' = UTF.decodeString $ BS8.unpack $ urlDecode True $ BS8.pack u8
    pushText uid $ T.pack $ unlines [u8', ln]

sjisEncToUTF8Enc str = do
  req <- parseRequest $
    "https://hogehoge.tk/tool/?flag=true&input=url&input_charset=SJIS&output_charset=UTF-8&output_hash=-&output=url&input_data="
    ++ str
  res <- httpLBS req { method = "POST" }
  let body = getResponseBody res
  -- BSL8.putStrLn body -- [debug]
  -- BSL8.writeFile "body.html" body -- [debug]
  let body' = BSL8.unpack body
  ss <- runX $ readString [ withParseHTML yes, withWarnings no ] body' //> arrow >>> writeDocumentToString []
  -- print ss -- [debug]
  return $ head ss
    where
    arrow = deep $
      hasName "textarea" >>> hasAttrValue "name" (=="output_data")
      -- >>> getChildren >>> hasName "tt"

arrHref =
  deep (hasName "div" >>> hasAttrValue "class" (=="g")) >>>
    deep (ifA (getChildren >>> hasName "a" >>> hasAttr "href" >>> neg (hasAttr "class")) this none)
    //> getAttrValue "href"

arrText =
  deep (hasName "div" >>> hasAttrValue "class" (=="g"))
    >>> getChildren >>> deep (ifA (hasName "span" >>> hasAttrValue "class" (=="st")) this none)

-- trial etc.
    
{-
sjisToUTF8 str = do
  req <- parseRequest $
    "http://www.eva.hi-ho.ne.jp/cgi-bin/user/zxcv/decodeUTF8.cgi?req=text&デコード実行&text="
    ++ str
  res <- httpLBS req
  let body = getResponseBody res
  let body' = BSL8.unpack body
  ss <- runX $ readString [ withParseHTML True ] body' //> arrow >>> writeDocumentToString []
  let
    s = replace "<br/>" "" $ last ss
    s' = urlDecode True $ BS8.pack s
    -- str = BS8.unpack s'
    -- t = T.pack str
  return (s,s')
    where
    arrow = deep $
      hasName "form" >>> hasAttrValue "action" (=="decodeUTF8.cgi")
      >>> getChildren >>> hasName "tt"

shiftJIStoUTF8 :: String -> IO T.Text
shiftJIStoUTF8 str = do
  req <- parseRequest $ "https://encodemaniax.com/?charset=utf-8&data=" ++ str
  -- https://encodemaniax.com/?charset=sjis&data=%E3%81%82
  -- https://encodemaniax.com/?charset=sjis&data=%82%a0
  -- https://encodemaniax.com/?charset=htf-8&data=%82%a0
  res <- httpLBS req
  let body = BSL8.unpack $ getResponseBody res
  en:_ <- runX $ readString [ withParseHTML True ] body //> arrURLEncode
  return $ Data.Text.Encoding.decodeUtf8 $ urlDecode True $ BS8.pack en
    where
    arrURLEncode = deep (hasName "pre" >>> hasAttrValue "id" (=="enc_data_url") >>> getChildren >>> getText)
-}

bsToHex :: BS8.ByteString -> String
bsToHex bs = if BS8.null bs
  then ""
  else "0x" ++ toHex (ord $ BS8.head bs) ++ " " ++ bsToHex (BS8.tail bs)
toHex n = showIntAtBase 16 intToDigit n ""

tr' = do
  req <- parseRequest $
    "https://www.google.co.jp/search?q=site:www.oita-ct.ac.jp/seigyo/nishimura_hp/SystemEngineering "
      ++ T.unpack "ウォーターフォール"
  let setH = setRequestHeaders
        [ ("Content-Type", "text/html; charset=utf-8")
        , ("Content-Language", "ja")
        , ("Accept-Language", "ja")
        , ("Accepe-Charset", "UTF-8")
        ]
  res <- httpLBS $ setH req
  let body = BSL8.unpack $ getResponseBody res
  mapM_ BS8.putStrLn $ getResponseHeader "Content-Type" res

{- ref
  case parse p "" body of
    Left  err  -> die (show err)
    Right rslt -> putStr $ unlines $ catMaybes rslt
p = many $ try target <|> (anyChar >> return Nothing)
target = do
  -- <a href=\"/url?q=http://www.oita-ct.ac.jp/seigyo/nishimura_hp/SystemEngineering/01/Note.html&amp;sa=U&amp;ved=0ahUKEwibrPGmtPjVAhXHfrwKHYcaC44QFgglMAQ&amp;usg=AFQjCNE0IhfJfbksy5Ail3_mnpgm80IgBQ\">
  string "<a href=\"/url?q="
  url <- manyTill anyChar (char '"')
  return $ Just url
-}

tr = do
  req <- parseRequest $
    "http://www.eva.hi-ho.ne.jp/cgi-bin/user/zxcv/decodeUTF8.cgi?req=text&デコード実行&text="
    ++ "%91%e6"
  res <- httpLBS req
  let body = getResponseBody res
  BSL8.writeFile "body.html" body
  exitSuccess
  let body' = BSL8.unpack body
  ss <- runX $ readString [ withParseHTML True ] body' //> arrow >>> writeDocumentToString []
  let
    s = replace "<br/>" "" $ last ss
    s' = urlDecode True $ BS8.pack s
    -- str = BS8.unpack s'
    -- t = T.pack str
  return (s,s')
    where
    arrow = deep $
      hasName "form" >>> hasAttrValue "action" (=="decodeUTF8.cgi")
      >>> getChildren >>> hasName "tt"
