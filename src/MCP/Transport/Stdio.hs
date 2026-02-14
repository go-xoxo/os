{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Transport.Stdio
  ( runStdioServer
  ) where

import Control.Exception (IOException, catch)
import Data.Aeson (decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import System.IO
  ( hFlush, hSetBinaryMode, hSetBuffering
  , stdin, stdout, stderr
  , BufferMode(..), hIsEOF
  )

import MCP.JsonRpc
import MCP.Server

-- | Run the MCP server using stdio transport.
-- Reads newline-delimited JSON-RPC messages from stdin,
-- processes them, and writes responses to stdout.
-- Per the MCP spec: messages are delimited by newlines and MUST NOT
-- contain embedded newlines.
runStdioServer :: ServerConfig -> IO ()
runStdioServer cfg = do
  -- Configure I/O for protocol communication
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  st <- newServerState
  processLines cfg st

-- | Process lines from stdin until EOF.
processLines :: ServerConfig -> ServerState -> IO ()
processLines cfg st = do
  eof <- hIsEOF stdin `catch` (\(_ :: IOException) -> pure True)
  if eof
    then pure ()  -- clean shutdown
    else do
      line <- BS.hGetLine stdin `catch` (\(_ :: IOException) -> pure BS.empty)
      if BS.null line
        then processLines cfg st  -- skip empty lines
        else do
          let lazyLine = BL.fromStrict line
          case decode lazyLine of
            Nothing  -> do
              -- Send parse error for malformed JSON
              let errResp = JsonRpcResponse (IdNumber 0) Nothing
                    (Just (JsonRpcError parseError "Parse error" Nothing))
              sendResponse errResp
            Just msg -> do
              mResp <- handleMessage cfg st msg
              case mResp of
                Nothing   -> pure ()
                Just resp -> sendResponse resp
          processLines cfg st

-- | Send a JSON-RPC response to stdout (newline-delimited).
sendResponse :: JsonRpcResponse -> IO ()
sendResponse resp = do
  BLC.hPut stdout (encode resp)
  BLC.hPut stdout "\n"
  hFlush stdout
