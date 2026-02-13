{-# LANGUAGE OverloadedStrings #-}

module LLM.OpenAI
  ( callOpenAI
  , transcribeImage
  , transcribeImages
  , TranscriptionResult (..)
  )
where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
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

data TranscriptionResult = TranscriptionResult
  { filePath :: FilePath
  , transcription :: Text
  }
  deriving (Show)

instance FromJSON TranscriptionResult where
  parseJSON = withObject "TranscriptionResult" $ \v ->
    TranscriptionResult
      <$> v .: "filePath"
      <*> v .: "transcription"

instance ToJSON TranscriptionResult where
  toJSON (TranscriptionResult fp t) =
    object
      [ "filePath" .= fp
      , "transcription" .= t
      ]

-- | Send a simple text message to OpenAI Chat Completions API.
callOpenAI :: Text -> IO LBS.ByteString
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
  return (responseBody response)

-- | Build an image content block for the Vision API using a base64-encoded image.
imageContent :: Text -> Text -> Value
imageContent base64Data mimeType =
  object
    [ "type" .= ("image_url" :: Text)
    , "image_url"
        .= object
          [ "url" .= ("data:" <> mimeType <> ";base64," <> base64Data)
          ]
    ]

-- | Build a text content block for the Vision API.
textContent :: Text -> Value
textContent t =
  object
    [ "type" .= ("text" :: Text)
    , "text" .= t
    ]

-- | Transcribe a single image by sending it to the OpenAI Vision API.
-- Takes an API key, a prompt describing what to transcribe, and the base64-encoded
-- image data along with its MIME type.
transcribeImage :: Text -> Text -> Text -> Text -> IO LBS.ByteString
transcribeImage apiKey prompt base64Data mimeType = do
  manager <- newManager tlsManagerSettings
  let contentParts =
        [ textContent prompt
        , imageContent base64Data mimeType
        ]
      req =
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
                    "gpt-4o"
                    [ object
                        [ "role" .= ("user" :: Text)
                        , "content" .= contentParts
                        ]
                    ]
          }
  response <- httpLbs req manager
  return (responseBody response)

-- | Transcribe multiple images by sending them all in a single Vision API request.
-- Each image is provided as a (base64Data, mimeType) pair.
transcribeImages :: Text -> Text -> [(Text, Text)] -> IO LBS.ByteString
transcribeImages apiKey prompt images = do
  manager <- newManager tlsManagerSettings
  let imageParts = map (uncurry imageContent) images
      contentParts = textContent prompt : imageParts
      req =
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
                    "gpt-4o"
                    [ object
                        [ "role" .= ("user" :: Text)
                        , "content" .= contentParts
                        ]
                    ]
          }
  response <- httpLbs req manager
  return (responseBody response)
