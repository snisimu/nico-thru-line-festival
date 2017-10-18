{-# LANGUAGE OverloadedStrings #-} {- -*- Coding: utf-8 -*- -}

module MyLib where

import System.Environment
import System.IO
import System.Exit
import System.FilePath.Windows
import System.Process
import System.Random
import System.Directory

import Control.Monad
import Control.Exception
import Control.Concurrent

import Data.Maybe
import Data.List
import Data.List.Utils
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time
import Data.Time.LocalTime

import Network.Mail.Mime
import Network.Mail.Client.Gmail (sendGmail)

import MyLib.Coursework

getEnvVar name = do
  mbVal <- lookupEnv name
  case mbVal of
     Nothing  -> die $ "please set environment variable \"" ++ name ++ "\"."
     Just val -> return val

-- Token

table =
  let
    alph = map chr [97..122]
    mess ns tbl = concat $ map (\(l,n) -> permutations l !! n) $ zip (transpose $ divideInto 8 tbl) ns
    gen = foldr1 (.) $ map mess
      [ [838,33136,1421,1802,30477,14189,37350,29208]
      , [37198,23979,4215,14774,20136,15270,18437,2778]
      , [34978,37646,3188,23489,18602,38532,22233,12405]
      ]
  in gen $ '-' : alph ++ concatMap show [0..9] ++ "_" ++ map toUpper alph

orD :: Char -> Int
orD = fromJust . flip elemIndex table

chR :: Int -> Char
chR =
  let n = length table
  in  (!!) (cycle table) . flip mod n . (+) n

fact n = case n of
  1 -> 1
  n -> n * fact (n-1)

paStr :: String -> String
paStr s =
  let perm = permutations [0..length s - 1] !! (sum (map ord s) `mod` fact (length s))
  in  map ((!!) s) perm
gbStr s = 
  let
    ns = [0..length s - 1]
    perm = permutations ns !! (sum (map ord s) `mod` fact (length s))
    inv  = map (fromJust . flip findIndex perm . (==)) ns
  in  map ((!!) s) inv

putaway :: String -> String -> String
putaway key s = paStr $ map (chR . uncurry (+)) $ zip (map orD s) $ cycle $ map orD key
getback key s = map (chR . uncurry (+)) $ zip (map orD $ gbStr s) $ cycle $ map (negate . orD) key

setEnvVar name' var = do
  system $ "powershell -Command \"[Environment]::SetEnvironmentVariable(\\\""
    ++ name' ++ "\\\", \\\"" ++ var ++ "\\\", [EnvironmentVariableTarget]::User)\""
  return ()

getMyToken :: String -> IO String
getMyToken name = do
  let name' = "MYTOKEN_" ++ map toUpper name
  mbKey <- lookupEnv "MYTOKEN_KEY"
  mbTok <- lookupEnv name'
  case (mbKey, mbTok) of
    (Just key, Just tok) -> return $ getback key tok
    _ -> do
      putStrLn $ "Not found environment variable(s)."
      key <- case mbKey of
        Just key -> return key
        Nothing -> do
          key <- newStdGen >>= \gen -> return $ map ((!!) table) $ take 20 $ randomRs (0, length table - 1) gen
          setEnvVar "MYTOKEN_KEY" key
          return key
      when (isNothing mbTok) $ do
        putStrLn $ "Set value for " ++ name' ++ ": "
        src <- getLine
        setEnvVar name' $ putaway key src
      putStrLn $ "Please restart this process"
      exitSuccess

-- Log

genLogger :: FilePath -> String -> Maybe (MailThru, String) -> Bool -> String -> IO ()
genLogger fileLog myName mbMail sendMail message = do
  time <- getZonedTime >>= return . reverse . tail . dropWhile ((/=) '.') . reverse . show . zonedTimeToLocalTime
  let message' = time ++ " " ++ message ++ "\n"
  putStr $ myName ++ ": " ++ message'
  appendFile fileLog $ message'
  case (mbMail, sendMail) of
    (Just (mailTr, to), True) -> mailThru mailTr Nothing to (T.pack $ "from " ++ myName) (T.pack message)
    _                         -> return ()
  -- to use: logger = genLogger "Log.txt" "AppName" $ Just (NITOiC, "snisimumobile@gmail.com")

-- Mail

type From = String
type To = String
type Subject = T.Text
type Body = T.Text

gmail :: From -> To -> T.Text -> LT.Text -> IO ()
gmail from to subject body = do
  tok <- getMyToken "GmailNITOiC"
  sendGmail
    "nitoic.nishimura"
    (LT.pack tok) -- $ getback key token)
    (Address Nothing $ T.pack from)
    [Address Nothing $ T.pack to]
    []
    []
    subject
    body
    []
    10000000

data MailThru = Gmail | NITOiC
  deriving (Eq, Show)

mailThru :: MailThru -> Maybe String -> String -> T.Text -> T.Text -> IO ()
mailThru mailThru mbFrom to subject body = do
  let from = case mbFrom of
        Nothing   -> "snisimu@gmail.com"
        Just from -> from
  case mailThru of
    Gmail ->
      gmail from to subject (LT.pack $ T.unpack body)
    sendThru -> do
      ps1 <- getEnvVar "ps1"
      system $ ps1 </> "MailThru" ++ show sendThru ++ ".ps1 "
        ++ concat (intersperse " " [from, to, T.unpack subject, T.unpack body])
      return ()

-- misc

ftpMyWebSite :: String -> String -> IO ExitCode
ftpMyWebSite from to = do
  let
    ps1FTPSpecific = "FTPMyWebSiteSpecific.ps1"
    from' =
      let fromTmp = replace "/" "\\" from
      in  if 2 < length fromTmp && take 2 fromTmp == ".\\"
        then drop 2 fromTmp
        else fromTmp
    to'   =
      let toTmp = replace "\\" "/" to
      in  if elem '.' toTmp
        then toTmp
        else if last toTmp == '/'
          then toTmp
          else toTmp ++ "/"
  folderPs1 <- getEnvVar "ps1"
  system $ "powershell " ++ folderPs1 </> concat (intersperse " " [ps1FTPSpecific, from', to'])

randomChoice :: [a] -> IO a
randomChoice as = newStdGen >>= \gen -> return $ as !! (fst (random gen) `mod` length as)

divideInto :: Int -> [a] -> [[a]]
divideInto n xs = splitBy (((length xs) + n - 1) `div` n) xs
splitBy :: Int -> [a] -> [[a]]
splitBy _ [] = []
splitBy m xs = xs1:(splitBy m xs2)
  where
    (xs1, xs2) = splitAt m xs

shift :: [a] -> Int -> [a]
shift l n = if 0 < n
  then drop n l  ++ take n l
  else shift l (length l + n)
allRotations :: [a] -> [[a]]
allRotations l = [ shift l i | i <- [0 .. (length l) -1]]


getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass
withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

duringHour :: Int -> Int -> IO Bool
duringHour from to = do
  hour <- getZonedTime >>= return . todHour . localTimeOfDay . zonedTimeToLocalTime
  return $ if from < to
    then from <= hour && hour < to
    else hour <= from || to < hour

createDirectoryIfNotExist dir = doesDirectoryExist dir >>= flip when (createDirectory dir) . not

sleepH'M'S (hour, min, sec) = do
  let s = hour * 60^2 + min * 60 + sec
  system $ "powershell -Command \"Start-Sleep -Seconds " ++ show s ++ "\""
  return ()

powerShell filePs1 = do
  folderPs1 <- getEnv "ps1"
  system $ "powershell " ++ folderPs1 </> filePs1
