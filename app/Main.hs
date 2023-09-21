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
import Data.ByteArray.Encoding (convertFromBase, Base(Base16))

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
  print key
  print nonce
  let key' = hexToByteString key
  let nonce' = case makeIV $ hexToByteString nonce of
        Nothing -> error ("Invalid nonce, expect a 32 char string, but provided" ++ nonce)
        Just iv -> iv
  return (key', nonce')

encode :: FilePath -> FilePath -> FilePath -> String -> IO ()
encode text image output authPath = do
  print "encoding... \n"
  (key, iv) <- parseAuthFile authPath
  print (encryptMessage "Hello world" key iv)

decode :: FilePath -> FilePath -> String -> IO ()
decode image output key = do
  print "decoding... \n"

main :: IO ()
main = do
  commands <- cmdArgs (modes [encodeArgs, decodeArgs])
  case commands of
    EncodeArgs { textInput = t, imageToEncode = i, outputImage = o, authFile = k } -> do
      encode t i o k
    DecodeArgs { imageToDecode = i, authFile = k, output = o } -> do
      decode i o k
  print "finished"

