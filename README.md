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
```

## How to run?

```
$ git clone
cabal run . -- --textInput="./samples/text.txt" --imageToEncode="./samples/image.jpg" outputImage="./samples/outputImage.jpg" encodekey="MY_KEY"
cabal run . -- --imageToDecode="./samples/outputImage.jpg" --decodekey="MY_KEY"
```



