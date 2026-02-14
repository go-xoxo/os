{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T

import MCP.Protocol
import MCP.Server
import MCP.Transport.Stdio

main :: IO ()
main = runStdioServer serverConfig

-- | Server configuration with example tools, resources, and prompts.
serverConfig :: ServerConfig
serverConfig = ServerConfig
  { cfgServerName    = "squirrel-os"
  , cfgServerVersion = "0.1.0"
  , cfgInstructions  = Just "Squirrel OS MCP server providing system tools and resources."
  , cfgTools         = serverTools
  , cfgToolHandler   = toolHandler
  , cfgResources     = serverResources
  , cfgResourceTemplates = serverResourceTemplates
  , cfgResourceHandler   = resourceHandler
  , cfgPrompts       = serverPrompts
  , cfgPromptHandler = promptHandler
  }

-- ---------------------------------------------------------------------------
-- Tools
-- ---------------------------------------------------------------------------

serverTools :: [Tool]
serverTools =
  [ Tool
      { toolName        = "echo"
      , toolDescription = Just "Echo back the provided message"
      , toolInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "message" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("The message to echo back" :: Text)
                  ]
              ]
          , "required" .= (["message"] :: [Text])
          ]
      , toolAnnotations = Just ToolAnnotations
          { annTitle          = Just "Echo"
          , annReadOnlyHint   = Just True
          , annDestructiveHint = Just False
          , annIdempotentHint = Just True
          , annOpenWorldHint  = Just False
          }
      }
  , Tool
      { toolName        = "get_system_info"
      , toolDescription = Just "Get information about the Squirrel OS system"
      , toolInputSchema = object ["type" .= ("object" :: Text), "properties" .= object []]
      , toolAnnotations = Just ToolAnnotations
          { annTitle          = Just "System Info"
          , annReadOnlyHint   = Just True
          , annDestructiveHint = Just False
          , annIdempotentHint = Just True
          , annOpenWorldHint  = Just False
          }
      }
  ]

toolHandler :: Text -> Value -> IO ToolResult
toolHandler name args = case name of
  "echo" -> do
    let msg = case args of
          Object o -> case KM.lookup "message" o of
            Just (String t) -> t
            _               -> "(no message)"
          _ -> "(invalid arguments)"
    pure $ ToolResult [TextContent msg] False
  "get_system_info" ->
    pure $ ToolResult
      [ TextContent $ T.unlines
          [ "Squirrel OS v0.1.0"
          , "Runtime: Haskell (GHC)"
          , "MCP Protocol: 2025-03-26"
          , "Transport: stdio"
          ]
      ] False
  _ ->
    pure $ ToolResult [TextContent ("Unknown tool: " <> name)] True

-- ---------------------------------------------------------------------------
-- Resources
-- ---------------------------------------------------------------------------

serverResources :: [Resource]
serverResources =
  [ Resource
      { resUri         = "squirrel://system/status"
      , resName        = "System Status"
      , resDescription = Just "Current system status information"
      , resMimeType    = Just "text/plain"
      , resSize        = Nothing
      }
  , Resource
      { resUri         = "squirrel://system/config"
      , resName        = "System Configuration"
      , resDescription = Just "Current system configuration"
      , resMimeType    = Just "application/json"
      , resSize        = Nothing
      }
  ]

serverResourceTemplates :: [ResourceTemplate]
serverResourceTemplates =
  [ ResourceTemplate
      { rtUriTemplate  = "squirrel://logs/{level}"
      , rtName         = "Log Files"
      , rtDescription  = Just "Access log files by level"
      , rtMimeType     = Just "text/plain"
      }
  ]

resourceHandler :: Text -> IO (Either Text [ResourceContents])
resourceHandler uri = case uri of
  "squirrel://system/status" ->
    pure $ Right [TextResourceContents uri "text/plain"
      "System: operational\nUptime: running\nMCP: connected"]
  "squirrel://system/config" ->
    pure $ Right [TextResourceContents uri "application/json"
      "{\"name\":\"squirrel-os\",\"version\":\"0.1.0\",\"mcp_protocol\":\"2025-03-26\"}"]
  _ ->
    pure $ Left ("Resource not found: " <> uri)

-- ---------------------------------------------------------------------------
-- Prompts
-- ---------------------------------------------------------------------------

serverPrompts :: [Prompt]
serverPrompts =
  [ Prompt
      { promptName        = "code_review"
      , promptDescription = Just "Review Haskell code for quality and suggest improvements"
      , promptArguments   = Just
          [ PromptArgument "code" (Just "The Haskell code to review") (Just True)
          ]
      }
  , Prompt
      { promptName        = "explain"
      , promptDescription = Just "Explain a concept in the context of Squirrel OS"
      , promptArguments   = Just
          [ PromptArgument "topic" (Just "The topic to explain") (Just True)
          ]
      }
  ]

promptHandler :: Text -> Maybe Value -> IO (Either Text GetPromptResult)
promptHandler name args = case name of
  "code_review" -> do
    let code = extractArg "code" args
    pure $ Right $ GetPromptResult
      (Just "Haskell code review prompt")
      [ PromptMessage "user" $ TextContent $
          "Please review the following Haskell code for quality, " <>
          "correctness, and idiomatic style. Suggest improvements:\n\n" <>
          code
      ]
  "explain" -> do
    let topic = extractArg "topic" args
    pure $ Right $ GetPromptResult
      (Just "Explanation prompt")
      [ PromptMessage "user" $ TextContent $
          "Explain the following concept in the context of " <>
          "Squirrel OS and its Haskell implementation:\n\n" <>
          topic
      ]
  _ ->
    pure $ Left ("Unknown prompt: " <> name)

extractArg :: Text -> Maybe Value -> Text
extractArg key (Just (Object o)) = case KM.lookup (Key.fromText key) o of
  Just (String t) -> t
  _               -> "(not provided)"
extractArg _ _ = "(not provided)"
