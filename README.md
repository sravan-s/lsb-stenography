# lsb-stenography
Image Steganography using LSB

The executable works in two modes


```
a. Encode mode
  textInput :: FilePath
  imageToEncode :: FilePath
  outputImage :: FilePath
  encodekey :: String

b. Decode Mode
  imageToDecode :: FilePath
  decodekey :: String
  output :: FilePath
```

## How to run?

```
$ git clone

cabal run . -- encodeargs --textinput="./samples/text.txt" --imagetoencode="./samples/image.jpg" --outputimage="./samples/outputImage.jpg" --encodekey="MY_KEY"

cabal run . -- decodeargs --imagetodecode="./samples/outputImage.jpg" --decodekey="MY_KEY" --output="output.txt" 
```



