{-# LANGUAGE OverloadedStrings #-}

module Image.Loader
  ( loadImage
  , loadImages
  , loadImagesFromDirectory
  , ImageData (..)
  , supportedExtensions
  )
where

import qualified Data.ByteString as BS
import Data.ByteString.Base64 (encode)
import Data.Text (Text, pack, toLower)
import Data.Text.Encoding (decodeUtf8)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))

-- | Represents a loaded image with its base64 data and MIME type.
data ImageData = ImageData
  { imageFilePath :: FilePath
  , imageBase64 :: Text
  , imageMimeType :: Text
  }
  deriving (Show)

-- | File extensions we support for image transcription.
supportedExtensions :: [String]
supportedExtensions = [".png", ".jpg", ".jpeg", ".gif", ".webp", ".bmp"]

-- | Determine the MIME type from a file extension.
mimeTypeFromExtension :: String -> Maybe Text
mimeTypeFromExtension ext = case toLower (pack ext) of
  ".png" -> Just "image/png"
  ".jpg" -> Just "image/jpeg"
  ".jpeg" -> Just "image/jpeg"
  ".gif" -> Just "image/gif"
  ".webp" -> Just "image/webp"
  ".bmp" -> Just "image/bmp"
  _ -> Nothing

-- | Load a single image file, returning its base64-encoded data and MIME type.
loadImage :: FilePath -> IO (Either String ImageData)
loadImage path = do
  exists <- doesFileExist path
  if not exists
    then return $ Left $ "File not found: " <> path
    else case mimeTypeFromExtension (takeExtension path) of
      Nothing ->
        return $ Left $ "Unsupported image format: " <> takeExtension path
      Just mime -> do
        raw <- BS.readFile path
        let b64 = decodeUtf8 (encode raw)
        return $ Right $ ImageData path b64 mime

-- | Load multiple image files from a list of file paths.
loadImages :: [FilePath] -> IO [Either String ImageData]
loadImages = mapM loadImage

-- | Load all supported images from a directory.
loadImagesFromDirectory :: FilePath -> IO [Either String ImageData]
loadImagesFromDirectory dir = do
  entries <- listDirectory dir
  let imageFiles =
        filter
          (\f -> takeExtension f `elem` supportedExtensions)
          entries
      fullPaths = map (dir </>) imageFiles
  loadImages fullPaths
