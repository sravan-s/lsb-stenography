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
yourHexKey :: String
yourHexKey = "0123456789abcdef0123456789abcdef"  -- Replace with your 32-character hex key

yourHexNonce :: String
yourHexNonce = "0123456789abcdef0123456789abcdef"  -- Replace with your 16-character hex nonce

defaultKey :: BS.ByteString
defaultKey = hexToByteString yourHexKey

defaultNonce :: IV AES128
defaultNonce = case makeIV $ hexToByteString yourHexNonce of
  Nothing -> error "Invalid nonce"
  Just iv -> iv

encryptMessage :: BS.ByteString -> BS.ByteString
encryptMessage plaintext = do
  let cipher = throwCryptoError $ cipherInit defaultKey :: AES128
  let ciphertext = ctrCombine cipher defaultNonce plaintext
  ciphertext

decryptMessage :: BS.ByteString -> BS.ByteString
decryptMessage ciphertext = do
  let cipher = throwCryptoError $ cipherInit defaultKey :: AES128
  let plaintext = ctrCombine cipher defaultNonce ciphertext
  plaintext

stringToEncodedAscii :: String -> [Int]
stringToEncodedAscii = map fromEnum 

encode :: FilePath -> FilePath -> FilePath -> String -> IO ()
encode text image output key = do
  print "encoding... \n"
  print text
  print image
  print output
  print key

decode :: FilePath -> FilePath -> String -> IO ()
decode image output key = do
  print "decoding... \n"
  print image
  print output
  print key

main :: IO ()
main = do
  commands <- cmdArgs (modes [encodeArgs, decodeArgs])
  case commands of
    EncodeArgs { textInput = t, imageToEncode = i, outputImage = o, authFile = k } -> do
      encode t i o k
    DecodeArgs { imageToDecode = i, authFile = k, output = o } -> do
      decode i o k
  print (encryptMessage "Hello, World!")
  print (decryptMessage $ encryptMessage "Hello, World!")
  print "finished"
