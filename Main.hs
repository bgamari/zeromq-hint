{-# LANGUAGE OverloadedStrings #-} 


module Main where
import System.IO
import Control.Monad --do
import Data.Aeson
-- import Data.Typeable
import System.ZMQ4.Monadic
import Language.Haskell.Interpreter --hint for evaluation

-- data type import
import Data.Either --fromRight
import Prelude hiding (lookup)
import Data.Maybe(fromMaybe)
import qualified Data.Text as T
import Data.Text (Text, pack)
import Data.ByteString.Char8 (pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Map --decode
-- Aeson's "encode" to json generates lazy bytestrings
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BSL


-- JSON encoding section
data Outdata = Outdata { x1 :: String }
             deriving (Show)

instance ToJSON Outdata where
  toJSON (Outdata xV) = object [ "output" .= xV]

packStr :: String -> B.ByteString
packStr = encodeUtf8 . T.pack

main ::IO ()

main = runZMQ $ do
    responder <- socket Pair
    bind responder "tcp://*:5558"
    liftIO $ do
        putStrLn "tcp://127.0.0.1:5558"
        hFlush stdout
    
    
    forever $ do
        message  <- receive responder
        let msg = BSL.pack $ unpack message
        let decodedMsg = decode msg :: Maybe (Map Text Text)
        let inputMsg = fromMaybe "" $ lookup "input" $ fromMaybe (fromList []) decodedMsg
        let toEvalStr = T.unpack inputMsg
        
        r <- liftIO $ runInterpreter $ do { setImports ["Prelude"]; eval toEvalStr}
        let res  = fromRight "" (r)
        let reply = Outdata res
        send responder [] $ packStr $ BSL.unpack (encode reply)