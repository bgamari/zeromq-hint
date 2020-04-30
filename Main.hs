{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.Catch
import System.IO
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:))
-- import Data.Typeable
import System.ZMQ4.Monadic as ZMQ

 -- hint for evaluation
import Language.Haskell.Interpreter as Hint

-- data type import
import Data.Either --fromRight
import Prelude hiding (lookup)
import Data.Maybe(fromMaybe)
import qualified Data.Text as T
import Data.Text (Text, pack)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Control.Monad.Trans.Class

-- JSON encoding section
data Request
  = Evaluate { reqInput :: Text }
  deriving (Show)

instance Aeson.FromJSON Request where
    parseJSON = Aeson.withObject "request" $ \o ->
        Evaluate <$> o .: "input"

data EvalResult
  = Success { x1 :: String }
  | Error { errmsg :: String }
  deriving (Show)

instance Aeson.ToJSON EvalResult where
    toJSON (Success xV) = Aeson.object [ "output" Aeson..= xV]
    toJSON (Error msg)  = Aeson.object [ "error" Aeson..= msg]

main :: IO ()
main = runZMQ $ do
    responder <- ZMQ.socket Pair
    ZMQ.bind responder "tcp://*:5558"
    liftIO $ do
        putStrLn "tcp://127.0.0.1:5558"
        hFlush stdout

    res <- runInterpreter $ do
        Hint.setImports ["Prelude"]
        forever $ do
            req <- lift $ readRequest responder
            liftIO $ print req
            result <- evaluateRequest req
            lift $ ZMQ.send responder [] $ BSL.toStrict $ Aeson.encode result

        return ()

    -- TODO: Handle failure
    liftIO $ print res

readRequest :: ZMQ.Socket z ZMQ.Pair -> ZMQ z Request
readRequest sock = do
    message <- ZMQ.receive sock
    case Aeson.eitherDecode $ BSL.fromStrict message of
        Left err  -> fail "invalid request" -- TODO: Handle failure
        Right req -> return req

evaluateRequest :: MonadInterpreter m => Request -> m EvalResult
evaluateRequest (Evaluate input) =
    handle onError $ fmap Success $ Hint.eval $ T.unpack input
  where
    onError :: Monad m => InterpreterError -> m EvalResult
    onError = return . Error . show
