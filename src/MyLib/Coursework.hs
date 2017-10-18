
module MyLib.Coursework where

import Control.Monad

import Data.Maybe
import Data.List
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Network.Socket hiding (sendTo)
import Network.Socket.ByteString

nicoAddress = "239.0.0.24"
nicoPort = "52460"

nicoSend :: T.Text -> IO ()
nicoSend msg = withSocketsDo $ do
  -- print msg -- [Debug]
  info <- getAddrInfo (Just hint) (Just nicoAddress) (Just nicoPort)
  let saddr =  find (\i -> AF_INET == addrFamily i) info
  unless (isNothing saddr) $ do
    sock <- socket (addrFamily $ fromJust saddr) Datagram defaultProtocol
    sendTo sock (encodeUtf8 msg) (addrAddress $ fromJust saddr)
    close sock
    where
    hint = defaultHints { addrSocketType = Datagram, addrProtocol = 17 }
