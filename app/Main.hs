{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main where
import System.Console.CmdArgs

data Args = EncodeArgs {
  textInput :: FilePath
  , imageToEncode :: FilePath
  , outputImage :: FilePath
  , encodekey :: String
} | DecodeArgs {
  imageToDecode :: FilePath
  , decodeKey :: String
  , output :: FilePath
} deriving (Show, Data, Typeable)

encodeArgs = EncodeArgs{
  textInput = def &= typFile &= help "Path to text file to encode"
  , imageToEncode = def &= typFile &= help "Path to image to encode"
  , outputImage = def &= typFile &= help "Path to output image"
  , encodekey = def &= typ "KEY" &= help "Key to encode with"
}

decodeArgs = DecodeArgs {
  imageToDecode = def &= typFile &= help "Path to image to decode"
  , decodeKey = def &= typ "KEY" &= help "Key to decode with"
  , output = def &= typFile &= help "Path to output file"
}

main :: IO ()
main = print =<< cmdArgs (modes [encodeArgs, decodeArgs])
