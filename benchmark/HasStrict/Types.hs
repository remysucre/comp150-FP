{-# LANGUAGE CPP #-}
module BCM.Visualize.Internal.Types where

#if !MIN_VERSION_base(4,8,0)
import Foreign.ForeignPtr.Safe( ForeignPtr, castForeignPtr )
#else
import Foreign.ForeignPtr( ForeignPtr, castForeignPtr )
#endif

import Control.Monad (when)
import Data.Serialize
import Data.Bits( xor, (.&.), unsafeShiftR )
import qualified Data.Vector.Unboxed as U
import Data.Word
import Data.List (foldl')
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import Data.Vector.Storable (Vector)

-- | Value used to identify a png chunk, must be 4 bytes long.
type ChunkSignature = B.ByteString

type Palette = Vector Word8

-- | Generic header used in PNG images.
data PngIHdr = PngIHdr
    { width             :: !Word32       -- ^ Image width in number of pixel
    , height            :: !Word32       -- ^ Image height in number of pixel
    , bitDepth          :: !Word8        -- ^ Number of bit per sample
    , colourType        :: !PngImageType -- ^ Kind of png image (greyscale, true color, indexed...)
    , compressionMethod :: !Word8        -- ^ Compression method used
    , filterMethod      :: !Word8        -- ^ Must be 0
    , interlaceMethod   :: !PngInterlaceMethod   -- ^ If the image is interlaced (for progressive rendering)
    }
    deriving Show

-- | Data structure during real png loading/parsing
data PngRawChunk = PngRawChunk
    { chunkLength :: Word32
    , chunkType   :: ChunkSignature
    , chunkCRC    :: Word32
    , chunkData   :: B.ByteString
    }

-- | What kind of information is encoded in the IDAT section
-- of the PngFile
data PngImageType =
      PngGreyscale
    | PngTrueColour
    | PngIndexedColor
    | PngGreyscaleWithAlpha
    | PngTrueColourWithAlpha
    deriving Show

-- | Different known interlace methods for PNG image
data PngInterlaceMethod =
      -- | No interlacing, basic data ordering, line by line
      -- from left to right.
      PngNoInterlace

      -- | Use the Adam7 ordering, see `adam7Reordering`
    | PngInterlaceAdam7
    deriving (Enum, Show)

instance Serialize PngRawChunk where
    put chunk = do
        putWord32be $ chunkLength chunk
        putByteString $ chunkType chunk
        when (chunkLength chunk /= 0)
             (putByteString $ chunkData chunk)
        putWord32be $ chunkCRC chunk

    get = do
        size <- getWord32be
        chunkSig <- getByteString (fromIntegral $ B.length iHDRSignature)
        imgData <- if size == 0
            then return B.empty
            else getByteString (fromIntegral size)
        crc <- getWord32be

        let computedCrc = pngComputeCrc [chunkSig, imgData]
        when (computedCrc `xor` crc /= 0)
             (fail $ "Invalid CRC : " ++ show computedCrc ++ ", "
                                      ++ show crc)
        return PngRawChunk {
            chunkLength = size,
            chunkData = imgData,
            chunkCRC = crc,
            chunkType = chunkSig
        }

instance Serialize PngImageType where
    put PngGreyscale = putWord8 0
    put PngTrueColour = putWord8 2
    put PngIndexedColor = putWord8 3
    put PngGreyscaleWithAlpha = putWord8 4
    put PngTrueColourWithAlpha = putWord8 6

    get = get >>= imageTypeOfCode

imageTypeOfCode :: Word8 -> Get PngImageType
imageTypeOfCode 0 = return PngGreyscale
imageTypeOfCode 2 = return PngTrueColour
imageTypeOfCode 3 = return PngIndexedColor
imageTypeOfCode 4 = return PngGreyscaleWithAlpha
imageTypeOfCode 6 = return PngTrueColourWithAlpha
imageTypeOfCode _ = fail "Invalid png color code"

instance Serialize PngIHdr where
    put hdr = do
        putWord32be 13
        let inner = runPut $ do
                putByteString iHDRSignature
                putWord32be $ width hdr
                putWord32be $ height hdr
                putWord8    $ bitDepth hdr
                put $ colourType hdr
                put $ compressionMethod hdr
                put $ filterMethod hdr
                put $ interlaceMethod hdr
            crc = pngComputeCrc [inner]
        putByteString inner
        putWord32be crc

    get = do
        _size <- getWord32be
        ihdrSig <- getByteString (B.length iHDRSignature)
        when (ihdrSig /= iHDRSignature)
             (fail "Invalid PNG file, wrong ihdr")
        w <- getWord32be
        h <- getWord32be
        depth <- get
        colorType <- get
        compression <- get
        filtermethod <- get
        interlace <- get
        _crc <- getWord32be
        return PngIHdr {
            width = w,
            height = h,
            bitDepth = depth,
            colourType = colorType,
            compressionMethod = compression,
            filterMethod = filtermethod,
            interlaceMethod = interlace
        }

instance Serialize PngInterlaceMethod where
    get = getWord8 >>= \w -> case w of
        0 -> return PngNoInterlace
        1 -> return PngInterlaceAdam7
        _ -> fail "Invalid interlace method"

    put PngNoInterlace    = putWord8 0
    put PngInterlaceAdam7 = putWord8 1

-- signature

-- | Signature signalling that the following data will be a png image
-- in the png bit stream
pngSignature :: ChunkSignature
pngSignature = B.pack [137, 80, 78, 71, 13, 10, 26, 10]

-- | Helper function to help pack signatures.
signature :: String -> ChunkSignature
signature = BS.pack 

-- | Signature for the header chunk of png (must be the first)
iHDRSignature :: ChunkSignature 
iHDRSignature = signature "IHDR"

-- | Signature for a palette chunk in the pgn file. Must
-- occure before iDAT.
pLTESignature :: ChunkSignature
pLTESignature = signature "PLTE"

-- | Signature for a data chuck (with image parts in it)
iDATSignature :: ChunkSignature
iDATSignature = signature "IDAT"

-- | Signature for the last chunk of a png image, telling
-- the end.
iENDSignature :: ChunkSignature
iENDSignature = signature "IEND"

----------------------------------------------------------------------------

-- | Compute the CRC of a raw buffer, as described in annex D of the PNG
-- specification.
pngComputeCrc :: [B.ByteString] -> Word32
pngComputeCrc = (0xFFFFFFFF `xor`) . B.foldl' updateCrc 0xFFFFFFFF . B.concat
    where updateCrc crc val =
              let u32Val = fromIntegral val
                  lutVal = pngCrcTable U.! fromIntegral ((crc `xor` u32Val) .&. 0xFF)
              in lutVal `xor` (crc `unsafeShiftR` 8)

-- | From the Annex D of the png specification.
pngCrcTable :: U.Vector Word32
pngCrcTable = U.fromListN 256 [ foldl' updateCrcConstant c [zero .. 7] | c <- [0 .. 255] ]
    where zero = 0 :: Int -- To avoid defaulting to Integer
          updateCrcConstant c _ | c .&. 1 /= 0 = magicConstant `xor` (c `unsafeShiftR` 1)
                                | otherwise = c `unsafeShiftR` 1
          magicConstant = 0xedb88320 :: Word32
