{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text, pack)
import Image.Loader
import LLM.OpenAI
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs, lookupEnv)
import System.FilePath (takeFileName)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> printUsage
    ("--help" : _) -> printUsage
    _ -> do
      mApiKey <- lookupEnv "OPENAI_API_KEY"
      case mApiKey of
        Nothing -> putStrLn "Error: OPENAI_API_KEY environment variable not set."
        Just key -> processArgs (pack key) args

printUsage :: IO ()
printUsage = do
  putStrLn "squirrel-os: Transcribe multiple images using OpenAI Vision API"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  squirrel-os <image1> [image2] [image3] ..."
  putStrLn "  squirrel-os <directory>"
  putStrLn ""
  putStrLn "Environment:"
  putStrLn "  OPENAI_API_KEY  Required. Your OpenAI API key."
  putStrLn ""
  putStrLn "Supported formats: PNG, JPG, JPEG, GIF, WebP, BMP"

processArgs :: Text -> [String] -> IO ()
processArgs apiKey args = do
  -- Check if the single argument is a directory
  images <- case args of
    [single] -> do
      isDir <- doesDirectoryExist single
      if isDir
        then do
          putStrLn $ "Loading images from directory: " <> single
          loadImagesFromDirectory single
        else loadImages args
    _ -> loadImages args

  let (errors, loaded) = partitionResults images
  mapM_ (\e -> putStrLn $ "Warning: " <> e) errors

  case loaded of
    [] -> putStrLn "No valid images found to transcribe."
    imgs -> transcribeAll apiKey imgs

partitionResults :: [Either String ImageData] -> ([String], [ImageData])
partitionResults = foldr go ([], [])
 where
  go (Left e) (es, ds) = (e : es, ds)
  go (Right d) (es, ds) = (es, d : ds)

transcribeAll :: Text -> [ImageData] -> IO ()
transcribeAll apiKey imgs = do
  let count = length imgs
  putStrLn $ "Transcribing " <> show count <> " image(s)..."
  putStrLn ""

  -- Process each image individually to get per-image transcriptions
  mapM_ (transcribeOne apiKey) imgs

transcribeOne :: Text -> ImageData -> IO ()
transcribeOne apiKey img = do
  let name = takeFileName (imageFilePath img)
      prompt =
        "Please transcribe all text visible in this image. "
          <> "If there is no text, describe the image content. "
          <> "Be thorough and accurate."
  putStrLn $ "--- " <> name <> " ---"
  result <- transcribeImage apiKey prompt (imageBase64 img) (imageMimeType img)
  LBS.putStrLn result
  putStrLn ""

