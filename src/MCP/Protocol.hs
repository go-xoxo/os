{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module MCP.Protocol
  ( -- * Protocol version
    protocolVersion
    -- * Capabilities
  , ClientCapabilities(..)
  , ServerCapabilities(..)
  , PromptsCap(..)
  , ResourcesCap(..)
  , ToolsCap(..)
  , LoggingCap(..)
    -- * Initialization
  , Implementation(..)
  , InitializeParams(..)
  , InitializeResult(..)
    -- * Tools
  , Tool(..)
  , ToolAnnotations(..)
  , ToolCallParams(..)
  , ToolResult(..)
  , ContentItem(..)
    -- * Resources
  , Resource(..)
  , ResourceTemplate(..)
  , ResourceContents(..)
  , ReadResourceParams(..)
    -- * Prompts
  , Prompt(..)
  , PromptArgument(..)
  , PromptMessage(..)
  , GetPromptParams(..)
  , GetPromptResult(..)
    -- * Pagination
  , PaginatedParams(..)
    -- * Logging
  , LoggingLevel(..)
  , SetLevelParams(..)
    -- * Progress
  , ProgressParams(..)
    -- * Cancellation
  , CancelledParams(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

-- | The protocol version this implementation supports.
protocolVersion :: Text
protocolVersion = "2025-03-26"

-- ---------------------------------------------------------------------------
-- Capabilities
-- ---------------------------------------------------------------------------

data ClientCapabilities = ClientCapabilities
  { clientRoots       :: Maybe Value
  , clientSampling    :: Maybe Value
  , clientExperimental :: Maybe Value
  } deriving (Show, Eq, Generic)

instance FromJSON ClientCapabilities where
  parseJSON = withObject "ClientCapabilities" $ \o ->
    ClientCapabilities
      <$> o .:? "roots"
      <*> o .:? "sampling"
      <*> o .:? "experimental"

instance ToJSON ClientCapabilities where
  toJSON c = object $ concat
    [ maybe [] (\v -> ["roots" .= v]) (clientRoots c)
    , maybe [] (\v -> ["sampling" .= v]) (clientSampling c)
    , maybe [] (\v -> ["experimental" .= v]) (clientExperimental c)
    ]

data PromptsCap = PromptsCap
  { promptsListChanged :: Maybe Bool
  } deriving (Show, Eq, Generic)

instance ToJSON PromptsCap where
  toJSON p = object $ maybe [] (\v -> ["listChanged" .= v]) (promptsListChanged p)

data ResourcesCap = ResourcesCap
  { resourcesSubscribe    :: Maybe Bool
  , resourcesListChanged  :: Maybe Bool
  } deriving (Show, Eq, Generic)

instance ToJSON ResourcesCap where
  toJSON r = object $ concat
    [ maybe [] (\v -> ["subscribe" .= v]) (resourcesSubscribe r)
    , maybe [] (\v -> ["listChanged" .= v]) (resourcesListChanged r)
    ]

data ToolsCap = ToolsCap
  { toolsListChanged :: Maybe Bool
  } deriving (Show, Eq, Generic)

instance ToJSON ToolsCap where
  toJSON t = object $ maybe [] (\v -> ["listChanged" .= v]) (toolsListChanged t)

data LoggingCap = LoggingCap
  deriving (Show, Eq, Generic)

instance ToJSON LoggingCap where
  toJSON _ = object []

data ServerCapabilities = ServerCapabilities
  { serverPrompts      :: Maybe PromptsCap
  , serverResources    :: Maybe ResourcesCap
  , serverTools        :: Maybe ToolsCap
  , serverLogging      :: Maybe LoggingCap
  , serverExperimental :: Maybe Value
  } deriving (Show, Eq, Generic)

instance ToJSON ServerCapabilities where
  toJSON s = object $ concat
    [ maybe [] (\v -> ["prompts" .= v]) (serverPrompts s)
    , maybe [] (\v -> ["resources" .= v]) (serverResources s)
    , maybe [] (\v -> ["tools" .= v]) (serverTools s)
    , maybe [] (\v -> ["logging" .= v]) (serverLogging s)
    , maybe [] (\v -> ["experimental" .= v]) (serverExperimental s)
    ]

-- ---------------------------------------------------------------------------
-- Initialization
-- ---------------------------------------------------------------------------

data Implementation = Implementation
  { implName    :: Text
  , implVersion :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON Implementation where
  parseJSON = withObject "Implementation" $ \o ->
    Implementation <$> o .: "name" <*> o .: "version"

instance ToJSON Implementation where
  toJSON i = object ["name" .= implName i, "version" .= implVersion i]

data InitializeParams = InitializeParams
  { initProtocolVersion :: Text
  , initCapabilities    :: ClientCapabilities
  , initClientInfo      :: Implementation
  } deriving (Show, Eq, Generic)

instance FromJSON InitializeParams where
  parseJSON = withObject "InitializeParams" $ \o ->
    InitializeParams
      <$> o .: "protocolVersion"
      <*> o .: "capabilities"
      <*> o .: "clientInfo"

data InitializeResult = InitializeResult
  { initResProtocolVersion :: Text
  , initResCapabilities    :: ServerCapabilities
  , initResServerInfo      :: Implementation
  , initResInstructions    :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON InitializeResult where
  toJSON r = object $ concat
    [ [ "protocolVersion" .= initResProtocolVersion r
      , "capabilities"    .= initResCapabilities r
      , "serverInfo"      .= initResServerInfo r
      ]
    , maybe [] (\v -> ["instructions" .= v]) (initResInstructions r)
    ]

-- ---------------------------------------------------------------------------
-- Tools
-- ---------------------------------------------------------------------------

data ToolAnnotations = ToolAnnotations
  { annTitle          :: Maybe Text
  , annReadOnlyHint   :: Maybe Bool
  , annDestructiveHint :: Maybe Bool
  , annIdempotentHint :: Maybe Bool
  , annOpenWorldHint  :: Maybe Bool
  } deriving (Show, Eq, Generic)

instance ToJSON ToolAnnotations where
  toJSON a = object $ concat
    [ maybe [] (\v -> ["title" .= v]) (annTitle a)
    , maybe [] (\v -> ["readOnlyHint" .= v]) (annReadOnlyHint a)
    , maybe [] (\v -> ["destructiveHint" .= v]) (annDestructiveHint a)
    , maybe [] (\v -> ["idempotentHint" .= v]) (annIdempotentHint a)
    , maybe [] (\v -> ["openWorldHint" .= v]) (annOpenWorldHint a)
    ]

data Tool = Tool
  { toolName        :: Text
  , toolDescription :: Maybe Text
  , toolInputSchema :: Value
  , toolAnnotations :: Maybe ToolAnnotations
  } deriving (Show, Eq, Generic)

instance ToJSON Tool where
  toJSON t = object $ concat
    [ [ "name"        .= toolName t
      , "inputSchema" .= toolInputSchema t
      ]
    , maybe [] (\v -> ["description" .= v]) (toolDescription t)
    , maybe [] (\v -> ["annotations" .= v]) (toolAnnotations t)
    ]

data ToolCallParams = ToolCallParams
  { tcName      :: Text
  , tcArguments :: Maybe Value
  } deriving (Show, Eq, Generic)

instance FromJSON ToolCallParams where
  parseJSON = withObject "ToolCallParams" $ \o ->
    ToolCallParams <$> o .: "name" <*> o .:? "arguments"

data ContentItem
  = TextContent Text
  | ImageContent Text Text  -- base64 data, mimeType
  | AudioContent Text Text  -- base64 data, mimeType
  | ResourceContent Resource Text  -- resource, text
  deriving (Show, Eq, Generic)

instance ToJSON ContentItem where
  toJSON (TextContent t) = object ["type" .= ("text" :: Text), "text" .= t]
  toJSON (ImageContent d m) = object ["type" .= ("image" :: Text), "data" .= d, "mimeType" .= m]
  toJSON (AudioContent d m) = object ["type" .= ("audio" :: Text), "data" .= d, "mimeType" .= m]
  toJSON (ResourceContent r t) = object
    [ "type" .= ("resource" :: Text)
    , "resource" .= object ["uri" .= resUri r, "mimeType" .= ("text/plain" :: Text), "text" .= t]
    ]

data ToolResult = ToolResult
  { trContent :: [ContentItem]
  , trIsError :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON ToolResult where
  toJSON r = object ["content" .= trContent r, "isError" .= trIsError r]

-- ---------------------------------------------------------------------------
-- Resources
-- ---------------------------------------------------------------------------

data Resource = Resource
  { resUri         :: Text
  , resName        :: Text
  , resDescription :: Maybe Text
  , resMimeType    :: Maybe Text
  , resSize        :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON Resource where
  toJSON r = object $ concat
    [ [ "uri"  .= resUri r
      , "name" .= resName r
      ]
    , maybe [] (\v -> ["description" .= v]) (resDescription r)
    , maybe [] (\v -> ["mimeType" .= v]) (resMimeType r)
    , maybe [] (\v -> ["size" .= v]) (resSize r)
    ]

data ResourceTemplate = ResourceTemplate
  { rtUriTemplate  :: Text
  , rtName         :: Text
  , rtDescription  :: Maybe Text
  , rtMimeType     :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON ResourceTemplate where
  toJSON r = object $ concat
    [ [ "uriTemplate" .= rtUriTemplate r
      , "name"        .= rtName r
      ]
    , maybe [] (\v -> ["description" .= v]) (rtDescription r)
    , maybe [] (\v -> ["mimeType" .= v]) (rtMimeType r)
    ]

data ResourceContents
  = TextResourceContents Text Text Text  -- uri, mimeType, text
  | BlobResourceContents Text Text Text  -- uri, mimeType, base64 blob
  deriving (Show, Eq, Generic)

instance ToJSON ResourceContents where
  toJSON (TextResourceContents uri mime txt) =
    object ["uri" .= uri, "mimeType" .= mime, "text" .= txt]
  toJSON (BlobResourceContents uri mime blob) =
    object ["uri" .= uri, "mimeType" .= mime, "blob" .= blob]

data ReadResourceParams = ReadResourceParams
  { rrUri :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON ReadResourceParams where
  parseJSON = withObject "ReadResourceParams" $ \o ->
    ReadResourceParams <$> o .: "uri"

-- ---------------------------------------------------------------------------
-- Prompts
-- ---------------------------------------------------------------------------

data PromptArgument = PromptArgument
  { paName        :: Text
  , paDescription :: Maybe Text
  , paRequired    :: Maybe Bool
  } deriving (Show, Eq, Generic)

instance ToJSON PromptArgument where
  toJSON a = object $ concat
    [ ["name" .= paName a]
    , maybe [] (\v -> ["description" .= v]) (paDescription a)
    , maybe [] (\v -> ["required" .= v]) (paRequired a)
    ]

data Prompt = Prompt
  { promptName        :: Text
  , promptDescription :: Maybe Text
  , promptArguments   :: Maybe [PromptArgument]
  } deriving (Show, Eq, Generic)

instance ToJSON Prompt where
  toJSON p = object $ concat
    [ ["name" .= promptName p]
    , maybe [] (\v -> ["description" .= v]) (promptDescription p)
    , maybe [] (\v -> ["arguments" .= v]) (promptArguments p)
    ]

data PromptMessage = PromptMessage
  { pmRole    :: Text
  , pmContent :: ContentItem
  } deriving (Show, Eq, Generic)

instance ToJSON PromptMessage where
  toJSON m = object ["role" .= pmRole m, "content" .= pmContent m]

data GetPromptParams = GetPromptParams
  { gpName      :: Text
  , gpArguments :: Maybe Value
  } deriving (Show, Eq, Generic)

instance FromJSON GetPromptParams where
  parseJSON = withObject "GetPromptParams" $ \o ->
    GetPromptParams <$> o .: "name" <*> o .:? "arguments"

data GetPromptResult = GetPromptResult
  { gprDescription :: Maybe Text
  , gprMessages    :: [PromptMessage]
  } deriving (Show, Eq, Generic)

instance ToJSON GetPromptResult where
  toJSON r = object $ concat
    [ ["messages" .= gprMessages r]
    , maybe [] (\v -> ["description" .= v]) (gprDescription r)
    ]

-- ---------------------------------------------------------------------------
-- Pagination
-- ---------------------------------------------------------------------------

data PaginatedParams = PaginatedParams
  { ppCursor :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON PaginatedParams where
  parseJSON = withObject "PaginatedParams" $ \o ->
    PaginatedParams <$> o .:? "cursor"

-- ---------------------------------------------------------------------------
-- Logging
-- ---------------------------------------------------------------------------

data LoggingLevel
  = LogDebug
  | LogInfo
  | LogNotice
  | LogWarning
  | LogError
  | LogCritical
  | LogAlert
  | LogEmergency
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON LoggingLevel where
  toJSON LogDebug     = "debug"
  toJSON LogInfo      = "info"
  toJSON LogNotice    = "notice"
  toJSON LogWarning   = "warning"
  toJSON LogError     = "error"
  toJSON LogCritical  = "critical"
  toJSON LogAlert     = "alert"
  toJSON LogEmergency = "emergency"

instance FromJSON LoggingLevel where
  parseJSON = withText "LoggingLevel" $ \case
    "debug"     -> pure LogDebug
    "info"      -> pure LogInfo
    "notice"    -> pure LogNotice
    "warning"   -> pure LogWarning
    "error"     -> pure LogError
    "critical"  -> pure LogCritical
    "alert"     -> pure LogAlert
    "emergency" -> pure LogEmergency
    _           -> fail "Unknown logging level"

data SetLevelParams = SetLevelParams
  { slLevel :: LoggingLevel
  } deriving (Show, Eq, Generic)

instance FromJSON SetLevelParams where
  parseJSON = withObject "SetLevelParams" $ \o ->
    SetLevelParams <$> o .: "level"

-- ---------------------------------------------------------------------------
-- Progress
-- ---------------------------------------------------------------------------

data ProgressParams = ProgressParams
  { ppProgressToken :: Value
  , ppProgress      :: Double
  , ppTotal         :: Maybe Double
  , ppMessage       :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON ProgressParams where
  toJSON p = object $ concat
    [ [ "progressToken" .= ppProgressToken p
      , "progress"      .= ppProgress p
      ]
    , maybe [] (\v -> ["total" .= v]) (ppTotal p)
    , maybe [] (\v -> ["message" .= v]) (ppMessage p)
    ]

-- ---------------------------------------------------------------------------
-- Cancellation
-- ---------------------------------------------------------------------------

data CancelledParams = CancelledParams
  { cpRequestId :: Value
  , cpReason    :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON CancelledParams where
  parseJSON = withObject "CancelledParams" $ \o ->
    CancelledParams <$> o .: "requestId" <*> o .:? "reason"
