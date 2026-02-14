{-# LANGUAGE OverloadedStrings #-}

module MCP.Server
  ( -- * Server configuration
    ServerConfig(..)
  , ToolHandler
  , ResourceHandler
  , PromptHandler
    -- * Server state
  , ServerState(..)
  , newServerState
    -- * Message handling
  , handleMessage
  ) where

import Data.Aeson
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T

import MCP.JsonRpc
import MCP.Protocol

-- | Handler for a tool invocation.
type ToolHandler = Value -> IO ToolResult

-- | Handler for reading a resource.
type ResourceHandler = Text -> IO (Either Text [ResourceContents])

-- | Handler for getting a prompt.
type PromptHandler = Text -> Maybe Value -> IO (Either Text GetPromptResult)

-- | Configuration for the MCP server.
data ServerConfig = ServerConfig
  { cfgServerName    :: Text
  , cfgServerVersion :: Text
  , cfgInstructions  :: Maybe Text
  , cfgTools         :: [Tool]
  , cfgToolHandler   :: Text -> ToolHandler
  , cfgResources     :: [Resource]
  , cfgResourceTemplates :: [ResourceTemplate]
  , cfgResourceHandler :: ResourceHandler
  , cfgPrompts       :: [Prompt]
  , cfgPromptHandler :: PromptHandler
  }

-- | Mutable server state.
data ServerState = ServerState
  { ssInitialized :: IORef Bool
  , ssLogLevel    :: IORef LoggingLevel
  }

-- | Create a new server state.
newServerState :: IO ServerState
newServerState =
  ServerState
    <$> newIORef False
    <*> newIORef LogDebug

-- | Handle an incoming JSON-RPC message, returning an optional response.
handleMessage :: ServerConfig -> ServerState -> JsonRpcMessage -> IO (Maybe JsonRpcResponse)
handleMessage cfg st msg = case msg of
  MsgRequest req      -> Just <$> handleRequest cfg st req
  MsgNotification n   -> handleNotification cfg st n >> pure Nothing
  MsgResponse _       -> pure Nothing  -- servers don't handle responses in this impl

-- | Handle a JSON-RPC request and produce a response.
handleRequest :: ServerConfig -> ServerState -> JsonRpcRequest -> IO JsonRpcResponse
handleRequest cfg st req = case reqMethod req of
  "initialize"               -> handleInitialize cfg st req
  "ping"                     -> pure $ mkResponse (reqId req) (object [])
  "tools/list"               -> handleToolsList cfg req
  "tools/call"               -> handleToolsCall cfg req
  "resources/list"           -> handleResourcesList cfg req
  "resources/templates/list" -> handleResourceTemplatesList cfg req
  "resources/read"           -> handleResourcesRead cfg req
  "prompts/list"             -> handlePromptsList cfg req
  "prompts/get"              -> handlePromptsGet cfg req
  "logging/setLevel"         -> handleSetLevel st req
  "completion/complete"      -> pure $ mkResponse (reqId req) (object ["completion" .= object ["values" .= ([] :: [Text])]])
  method                     -> pure $ mkErrorResponse (reqId req) methodNotFound
                                  ("Method not found: " <> method) Nothing

-- | Handle a JSON-RPC notification.
handleNotification :: ServerConfig -> ServerState -> JsonRpcNotification -> IO ()
handleNotification _cfg st notif = case notifMethod notif of
  "notifications/initialized" -> writeIORef (ssInitialized st) True
  "notifications/cancelled"   -> pure ()  -- acknowledge cancellation
  _                           -> pure ()  -- ignore unknown notifications

-- ---------------------------------------------------------------------------
-- Request handlers
-- ---------------------------------------------------------------------------

handleInitialize :: ServerConfig -> ServerState -> JsonRpcRequest -> IO JsonRpcResponse
handleInitialize cfg _st req = do
  let caps = ServerCapabilities
        { serverPrompts   = Just (PromptsCap (Just True))
        , serverResources = Just (ResourcesCap (Just False) (Just True))
        , serverTools     = Just (ToolsCap (Just True))
        , serverLogging   = Just LoggingCap
        , serverExperimental = Nothing
        }
      result = InitializeResult
        { initResProtocolVersion = protocolVersion
        , initResCapabilities    = caps
        , initResServerInfo      = Implementation (cfgServerName cfg) (cfgServerVersion cfg)
        , initResInstructions    = cfgInstructions cfg
        }
  pure $ mkResponse (reqId req) (toJSON result)

handleToolsList :: ServerConfig -> JsonRpcRequest -> IO JsonRpcResponse
handleToolsList cfg req =
  pure $ mkResponse (reqId req) $ object ["tools" .= cfgTools cfg]

handleToolsCall :: ServerConfig -> JsonRpcRequest -> IO JsonRpcResponse
handleToolsCall cfg req = case reqParams req of
  Nothing -> pure $ mkErrorResponse (reqId req) invalidParams "Missing params" Nothing
  Just params -> case fromJSON params of
    Error e   -> pure $ mkErrorResponse (reqId req) invalidParams (T.pack e) Nothing
    Success tc -> do
      let handler = cfgToolHandler cfg (tcName tc)
      result <- handler (maybe (object []) id (tcArguments tc))
      pure $ mkResponse (reqId req) (toJSON result)

handleResourcesList :: ServerConfig -> JsonRpcRequest -> IO JsonRpcResponse
handleResourcesList cfg req =
  pure $ mkResponse (reqId req) $ object ["resources" .= cfgResources cfg]

handleResourceTemplatesList :: ServerConfig -> JsonRpcRequest -> IO JsonRpcResponse
handleResourceTemplatesList cfg req =
  pure $ mkResponse (reqId req) $ object ["resourceTemplates" .= cfgResourceTemplates cfg]

handleResourcesRead :: ServerConfig -> JsonRpcRequest -> IO JsonRpcResponse
handleResourcesRead cfg req = case reqParams req of
  Nothing -> pure $ mkErrorResponse (reqId req) invalidParams "Missing params" Nothing
  Just params -> case fromJSON params of
    Error e   -> pure $ mkErrorResponse (reqId req) invalidParams (T.pack e) Nothing
    Success rp -> do
      result <- cfgResourceHandler cfg (rrUri rp)
      case result of
        Left err       -> pure $ mkErrorResponse (reqId req) (-32002) err Nothing
        Right contents -> pure $ mkResponse (reqId req) $ object ["contents" .= contents]

handlePromptsList :: ServerConfig -> JsonRpcRequest -> IO JsonRpcResponse
handlePromptsList cfg req =
  pure $ mkResponse (reqId req) $ object ["prompts" .= cfgPrompts cfg]

handlePromptsGet :: ServerConfig -> JsonRpcRequest -> IO JsonRpcResponse
handlePromptsGet cfg req = case reqParams req of
  Nothing -> pure $ mkErrorResponse (reqId req) invalidParams "Missing params" Nothing
  Just params -> case fromJSON params of
    Error e   -> pure $ mkErrorResponse (reqId req) invalidParams (T.pack e) Nothing
    Success gp -> do
      result <- cfgPromptHandler cfg (gpName gp) (gpArguments gp)
      case result of
        Left err  -> pure $ mkErrorResponse (reqId req) invalidParams err Nothing
        Right res -> pure $ mkResponse (reqId req) (toJSON res)

handleSetLevel :: ServerState -> JsonRpcRequest -> IO JsonRpcResponse
handleSetLevel st req = case reqParams req of
  Nothing -> pure $ mkErrorResponse (reqId req) invalidParams "Missing params" Nothing
  Just params -> case fromJSON params of
    Error e   -> pure $ mkErrorResponse (reqId req) invalidParams (T.pack e) Nothing
    Success sl -> do
      writeIORef (ssLogLevel st) (slLevel sl)
      pure $ mkResponse (reqId req) (object [])
