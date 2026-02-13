{-# LANGUAGE OverloadedStrings #-}

module LLM.OpenAI where

import Data.Aeson
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header

data ChatRequest = ChatRequest
  { model :: Text
  , messages :: [Value]
  }

instance ToJSON ChatRequest where
  toJSON (ChatRequest m msgs) =
    object
      [ "model" .= m
      , "messages" .= msgs
      ]

callOpenAI :: Text -> IO ()
callOpenAI apiKey = do
  manager <- newManager tlsManagerSettings
  let req =
        (parseRequest_ "https://api.openai.com/v1/chat/completions")
          { method = "POST"
          , requestHeaders =
              [ (hAuthorization, "Bearer " <> encodeUtf8 apiKey)
              , (hContentType, "application/json")
              ]
          , requestBody =
              RequestBodyLBS $
                encode $
                  ChatRequest
                    "gpt-4o-mini"
                    [ object
                        [ "role" .= ("user" :: Text)
                        , "content" .= ("Hello from Haskell" :: Text)
                        ]
                    ]
          }

  response <- httpLbs req manager
  print (responseBody response)
