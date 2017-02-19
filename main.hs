import System.IO as IO
import Data.ByteString.Lazy as BS

main = do
  IO.putStrLn "Give path to data file"
  dataFilePath <- IO.getLine
  IO.putStrLn "Give path to image file"
  imgFilePath <- IO.getLine

  dataContent <- BS.readFile dataFilePath
  imgContent <- BS.readFile imgFilePath

  print dataContent
  print imgContent
