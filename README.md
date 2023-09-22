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

-- copy authfile.example to .authfile and fill necessary fields

cabal run . -- encodeargs --textinput="./samples/text.txt" --imagetoencode="./samples/image.jpg" --outputimage="./samples/outputImage.jpg" --authfile="./samples/.authfile"

cabal run . -- decodeargs --imagetodecode="./samples/outputImage.jpg" --authfile="./samples/.authfile" --output="./samples/output.txt"
```



