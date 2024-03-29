{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main where
import System.Console.CmdArgs
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Error
import qualified Crypto.Random.Types as CRT
import qualified Data.ByteString as BS
import qualified Data.ByteArray as BA
import Data.Word
import Data.ByteArray.Encoding (convertFromBase, Base(Base16))
import Codec.Picture
import qualified Data.Vector.Storable as V

data Args = EncodeArgs {
  textInput :: FilePath
  , imageToEncode :: FilePath
  , outputImage :: FilePath
  , authFile :: FilePath
} | DecodeArgs {
  imageToDecode :: FilePath
  , authFile :: FilePath
  , output :: FilePath
} deriving (Show, Data, Typeable)

encodeArgs = EncodeArgs{
  textInput = def &= typFile &= help "Path to text file to encode"
  , imageToEncode = def &= typFile &= help "Path to image to encode"
  , outputImage = def &= typFile &= help "Path to output image"
  , authFile = def &= typ "KEY" &= help "Path to your auth file, see auth.example in samples"
}

decodeArgs = DecodeArgs {
  imageToDecode = def &= typFile &= help "Path to image to decode"
  , authFile = def &= typ "KEY" &= help "Path to your auth file, see auth.example in samples"
  , output = def &= typFile &= help "Path to output file"
}

hexToByteString :: String -> BS.ByteString
hexToByteString hex = case convertFromBase Base16 (BS.pack $ map (fromIntegral . fromEnum) hex) of
  (Left err) -> error $ "Invalid hex: " ++ err
  (Right bs) -> bs

-- Your provided key and nonce as hexadecimal strings
keyToBS :: String -> BS.ByteString
keyToBS = hexToByteString

nonceToIv :: String -> IV AES128
nonceToIv nonce = case makeIV $ hexToByteString nonce of
  Nothing -> error "Invalid nonce"
  Just iv -> iv

encryptMessage :: BS.ByteString -> BS.ByteString -> IV AES128 -> BS.ByteString
encryptMessage plaintext key nonce  = do
  let cipher = throwCryptoError $ cipherInit key :: AES128
  let ciphertext = ctrCombine cipher nonce plaintext
  ciphertext

decryptMessage :: BS.ByteString -> BS.ByteString -> IV AES128 -> BS.ByteString
decryptMessage ciphertext key nonce = do
  let cipher = throwCryptoError $ cipherInit key :: AES128
  let plaintext = ctrCombine cipher nonce ciphertext
  plaintext

stringToEncodedAscii :: String -> [Int]
stringToEncodedAscii = map fromEnum

parseAuthFile :: FilePath -> IO (BS.ByteString, IV AES128)
parseAuthFile path = do
  contents <- readFile path
  let [_, key, nonce] = lines contents
  let key' = hexToByteString key
  let nonce' = case makeIV $ hexToByteString nonce of
        Nothing -> error ("Invalid nonce, expect a 32 char string, but provided" ++ nonce)
        Just iv -> iv
  return (key', nonce')

modifyPixel :: [Word8] -> PixelRGBA8 -> PixelRGBA8
modifyPixel [] pixel = pixel
modifyPixel (x:xs) (PixelRGBA8 r g b a) = PixelRGBA8 r g b x

encode :: FilePath -> FilePath -> FilePath -> String -> IO ()
encode text image output authPath = do
  print "encoding... \n"
  plaintext <- BS.readFile text
  (key, iv) <- parseAuthFile authPath
  let encryptedMessage = encryptMessage plaintext key iv
  -- write to output file
  imgResult <- readImage image
  case imgResult of
    Left err -> putStrLn ("Error loading image: " ++ err)
    Right dynamicImage ->
      let img = convertRGBA8 dynamicImage
          encryptedByteList = BS.unpack encryptedMessage
          processedImage = pixelMap (modifyPixel encryptedByteList) img
      in savePngImage output (ImageRGBA8 processedImage)
  print ("finished encoding, see: " ++ output)


toPixels :: [Word8] -> [PixelRGBA8]
toPixels [] = []
toPixels (r:g:b:a:rest) = PixelRGBA8 r g b a : toPixels rest
toPixels _ = error "Incomplete pixel data"

decode :: FilePath -> FilePath -> String -> IO ()
decode image output key = do
  print "decoding... \n"
  (key, iv) <- parseAuthFile key
  -- read image
  imgResult <- readImage image
  case imgResult of
    Left err -> putStrLn ("Error loading image: " ++ err)
    Right dynamicImage -> do
      let img = convertRGBA8 dynamicImage
          -- extract data from image
          pixelList = toPixels $ V.toList(imageData img)
          encryptedByteList = map (\(PixelRGBA8 _ _ _ a) -> a) pixelList
          encryptedMessage = BS.pack encryptedByteList
          -- decrypt
          plaintext = decryptMessage encryptedMessage key iv
      -- write to output file
      BS.writeFile output plaintext
  print ("decoded, see: " ++ output)

main :: IO ()
main = do
  commands <- cmdArgs (modes [encodeArgs, decodeArgs])
  case commands of
    EncodeArgs { textInput = t, imageToEncode = i, outputImage = o, authFile = k } -> do
      encode t i o k
    DecodeArgs { imageToDecode = i, authFile = k, output = o } -> do
      decode i o k

