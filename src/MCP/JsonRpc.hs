{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module MCP.JsonRpc
  ( -- * Types
    RequestId(..)
  , JsonRpcRequest(..)
  , JsonRpcNotification(..)
  , JsonRpcResponse(..)
  , JsonRpcError(..)
  , JsonRpcMessage(..)
    -- * Error codes
  , parseError
  , invalidRequest
  , methodNotFound
  , invalidParams
  , internalError
    -- * Helpers
  , mkResponse
  , mkErrorResponse
  , mkNotification
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Data.Aeson.KeyMap as KM

-- | JSON-RPC request ID, either a string or integer.
data RequestId
  = IdString Text
  | IdNumber Int
  deriving (Show, Eq, Generic)

instance ToJSON RequestId where
  toJSON (IdString s) = toJSON s
  toJSON (IdNumber n) = toJSON n

instance FromJSON RequestId where
  parseJSON (String s) = pure (IdString s)
  parseJSON (Number n) = pure (IdNumber (round n))
  parseJSON _          = fail "Expected string or number for request ID"

-- | A JSON-RPC 2.0 request.
data JsonRpcRequest = JsonRpcRequest
  { reqId     :: RequestId
  , reqMethod :: Text
  , reqParams :: Maybe Value
  } deriving (Show, Eq, Generic)

instance ToJSON JsonRpcRequest where
  toJSON r = object $
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id"      .= reqId r
    , "method"  .= reqMethod r
    ] ++ maybe [] (\p -> ["params" .= p]) (reqParams r)

instance FromJSON JsonRpcRequest where
  parseJSON = withObject "JsonRpcRequest" $ \o -> do
    _ <- o .: "jsonrpc" :: Parser Text
    JsonRpcRequest
      <$> o .: "id"
      <*> o .: "method"
      <*> o .:? "params"

-- | A JSON-RPC 2.0 notification (no id, no response expected).
data JsonRpcNotification = JsonRpcNotification
  { notifMethod :: Text
  , notifParams :: Maybe Value
  } deriving (Show, Eq, Generic)

instance ToJSON JsonRpcNotification where
  toJSON n = object $
    [ "jsonrpc" .= ("2.0" :: Text)
    , "method"  .= notifMethod n
    ] ++ maybe [] (\p -> ["params" .= p]) (notifParams n)

instance FromJSON JsonRpcNotification where
  parseJSON = withObject "JsonRpcNotification" $ \o -> do
    _ <- o .: "jsonrpc" :: Parser Text
    JsonRpcNotification
      <$> o .: "method"
      <*> o .:? "params"

-- | A JSON-RPC 2.0 error object.
data JsonRpcError = JsonRpcError
  { errCode    :: Int
  , errMessage :: Text
  , errData    :: Maybe Value
  } deriving (Show, Eq, Generic)

instance ToJSON JsonRpcError where
  toJSON e = object $
    [ "code"    .= errCode e
    , "message" .= errMessage e
    ] ++ maybe [] (\d -> ["data" .= d]) (errData e)

instance FromJSON JsonRpcError where
  parseJSON = withObject "JsonRpcError" $ \o ->
    JsonRpcError
      <$> o .: "code"
      <*> o .: "message"
      <*> o .:? "data"

-- | A JSON-RPC 2.0 response (success or error).
data JsonRpcResponse = JsonRpcResponse
  { resId     :: RequestId
  , resResult :: Maybe Value
  , resError  :: Maybe JsonRpcError
  } deriving (Show, Eq, Generic)

instance ToJSON JsonRpcResponse where
  toJSON r = object $
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id"      .= resId r
    ] ++ case resError r of
          Just e  -> ["error" .= e]
          Nothing -> ["result" .= resResult r]

instance FromJSON JsonRpcResponse where
  parseJSON = withObject "JsonRpcResponse" $ \o -> do
    _ <- o .: "jsonrpc" :: Parser Text
    JsonRpcResponse
      <$> o .: "id"
      <*> o .:? "result"
      <*> o .:? "error"

-- | Any JSON-RPC 2.0 message.
data JsonRpcMessage
  = MsgRequest      JsonRpcRequest
  | MsgNotification JsonRpcNotification
  | MsgResponse     JsonRpcResponse
  deriving (Show, Eq)

instance FromJSON JsonRpcMessage where
  parseJSON val = withObject "JsonRpcMessage" (\o -> do
    hasId     <- pure (KM.member "id" o)
    hasMethod <- pure (KM.member "method" o)
    case (hasId, hasMethod) of
      (True, True)   -> MsgRequest      <$> parseJSON val
      (False, True)  -> MsgNotification <$> parseJSON val
      (True, False)  -> MsgResponse     <$> parseJSON val
      _              -> fail "Invalid JSON-RPC message"
    ) val

instance ToJSON JsonRpcMessage where
  toJSON (MsgRequest r)      = toJSON r
  toJSON (MsgNotification n) = toJSON n
  toJSON (MsgResponse r)     = toJSON r

-- | Standard JSON-RPC error codes.
parseError, invalidRequest, methodNotFound, invalidParams, internalError :: Int
parseError     = -32700
invalidRequest = -32600
methodNotFound = -32601
invalidParams  = -32602
internalError  = -32603

-- | Construct a success response.
mkResponse :: RequestId -> Value -> JsonRpcResponse
mkResponse rid val = JsonRpcResponse rid (Just val) Nothing

-- | Construct an error response.
mkErrorResponse :: RequestId -> Int -> Text -> Maybe Value -> JsonRpcResponse
mkErrorResponse rid code msg dat =
  JsonRpcResponse rid Nothing (Just (JsonRpcError code msg dat))

-- | Construct a notification.
mkNotification :: Text -> Maybe Value -> JsonRpcNotification
mkNotification = JsonRpcNotification
