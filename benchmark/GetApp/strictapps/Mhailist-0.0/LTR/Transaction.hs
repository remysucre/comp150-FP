{-# LANGUAGE CPP, NamedFieldPuns, OverloadedStrings #-}

module LTR.Transaction (

    Transaction (..),
    Originator, mkOriginator,
    mkTransaction,
    putTransactionWithoutDigest,
    appendTransaction, getTransactions,

    Word32, Word16,
#ifdef __TEST__
    _testTransaction,
#endif

) where

import Control.Monad (unless)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Digest.Pure.MD5
import Data.Time.Clock (UTCTime, diffUTCTime, NominalDiffTime)
import Data.Time.Format (readTime)
import Data.Word (Word32, Word16)
import Util.String ()
import System.IO (IOMode (..), withBinaryFile)
import System.Locale (defaultTimeLocale)

#ifdef __TEST__
import UnitTest
#endif

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS

{- Transaction Binary Format

  |---------------------------------------------------------------|
  |   magic   | v |                length                         |
  |---------------------------------------------------------------|
  |          application          |     flags     |   t-type      |
  |---------------------------------------------------------------|
  |                          originator                           |
  |---------------------------------------------------------------|
  |                          timestamp                            |
  |---------------------------------------------------------------|
  .                                                               .
  .                            data                               .
  .                                                               .
  |---------------------------------------------------------------|
  |                          MD5 Digest                           |
  |                                                               |
  |---------------------------------------------------------------|

  Bits  Name            Description
  ----- --------------- ---------------------------------------------------
   24   magic           Identifies the start of a transaction; always "cjs".
   08   version         Transaction format version: currently 0.
   48   length          The length of the entire transaction, including
                        the header and MD5 digest.
   32   application     The number assigned to the particular program or
                        system that generates and uses this transaction.
                        These numbers are assigned by a central authority.
                        However, the range 0xff000000 through 0xfffeffff is
                        desginated for "experimental" use, and will never
                        be assigned.
   16   flags           Currently reserved and must be set to all zeros.
                        (A "compressed data" flag will be added soon.)
   16   t-type          Transaction type; assigned by the application.
   64   originator      A globally-unique node ID for a particular generator
                        of transactions. How this is assigned is yet to be
                        determined.
   64   timestamp       Signed number of milliseconds since the Unix epoch,
                        1970-01-01 00:00:00 UTC.
    n   data            A variable amount of data.
  128   digest          An MD5 digest of the header (inc. length) and data.

-}

----------------------------------------------------------------------

-- | A transaction and associated data, if valid. Note that because the
-- constructors are exposed for pattern matching, it's possible to build
-- an invalid 'Transaction'; expect writing such things to fail. Always
-- use 'mkTransaction' to create a transaction.
--
-- 'transRawData' does not include the digest at the end.
--
data Transaction
    = InvalidTransaction String
    | Transaction
        { transApp              :: Word32
        , transType             :: Word16
        , transOriginator       :: Originator
        , transTS               :: NominalDiffTime
        , transData             :: ByteString
        , transRawData          :: ByteString
        , transID               :: MD5Digest
        }
    deriving (Show, Eq)

newtype Originator = Originator ByteString
    deriving (Show, Eq)

----------------------------------------------------------------------

-- XXX Temporarially, originators are always zero.
mkOriginator :: Int -> Originator
mkOriginator 0 = Originator "\0\0\0\0\0\0\0\0"
mkOriginator _ = error "Transaction.mkOriginator: write me"

epoch = readTime defaultTimeLocale "" "" :: UTCTime

milliseconds :: NominalDiffTime -> Word64
milliseconds t = round $ 1000 * toRational t

----------------------------------------------------------------------

-- | Create a valid transaction. Transactions created using the data
-- constructors directly are likely to be invalid, and be silently
-- corrupted when written.
--
mkTransaction :: Word32 -> Word16 -> Originator
                 -> NominalDiffTime -> ByteString -> Transaction
mkTransaction app ttype orig ts tdata =
    Transaction { transApp              = app
                , transType             = ttype
                , transOriginator       = orig
                , transTS               = ts
                , transData             = tdata
                , transRawData          = rawData
                , transID               = digest
                }
    where
        rawData = runPut $ putTransactionWithoutDigest app ttype orig ts tdata
        digest  = md5 rawData

instance Binary Transaction where

    get = do magic <- getLazyByteString 3
             unless (magic == "cjs")
                 $ error $ "XXX Bad transaction magic number"
             version <- getWord8
             unless (version == 0)
                 $ error $ "XXX Bad transaction version: " ++ show version
             lengthHi <- getWord16be
             unless (lengthHi == 0)
                 $ error $ "XXX Packet too long for this implementation: "
                           ++ "lengthHi=" ++ show lengthHi
             lengthLo <- getWord32be
             let dataLen = (fromIntegral lengthLo) - (8 * 6)
             app    <- getWord32be
             flags  <- getWord16be
             ttype  <- getWord16be
             orig   <- get
             ts     <- get
             tdata  <- getLazyByteString dataLen
             let rawData = B.concat [magic, encode version, encode lengthHi,
                                    encode lengthLo, encode app, encode flags,
                                    encode ttype, encode orig, encode ts,
                                    tdata]
             digest <- get :: Get MD5Digest
             unless (digest == md5 rawData)
                 $ error.concat $ [ "XXX transaction digest does not match: "
                                  , "expected=", show digest, ", "
                                  , "computed=", show (md5 rawData) ]
             return $! Transaction 
                           { transApp              = app
                           , transType             = ttype
                           , transOriginator       = orig
                           , transTS               = ts
                           , transData             = tdata
                           , transRawData          = rawData
                           , transID               = digest
                           }

    put InvalidTransaction{} = fail "Cannot put an InvalidTransaction"
    put Transaction{transRawData,transID} = do putLazyByteString transRawData
                                               put transID
    

putTransactionWithoutDigest
    :: Word32 -> Word16 -> Originator -> NominalDiffTime -> ByteString -> Put
putTransactionWithoutDigest app ttype orig ts tdata =
    do putLazyByteString    magic
       putWord8             version
       putWord16be          lengthHi
       -- XXX Should check length
       let lengthLo = (fromIntegral $ B.length tdata) + (8 * 6)
       putWord32be          lengthLo
       putWord32be          app
       putWord16be          flags
       putWord16be          ttype
       put                  orig
       put                  ts
       putLazyByteString    tdata
    where
        magic = "cjs" :: ByteString
        version = 0
        lengthHi = 0
        flags = 0

instance Binary Originator where
    get                 = getLazyByteString 8 >>= return . Originator
    put (Originator o)  = putLazyByteString o

instance Binary NominalDiffTime where
    get = do ms <- getWord64be
             return $ fromIntegral ms / 1000
    put = put . milliseconds 

----------------------------------------------------------------------

-- |Safely appends the transaction to the given file, ensuring that no
-- other process using this will write the file at the same time.
--
appendTransaction :: FilePath -> Transaction -> IO ()
appendTransaction path trans =
    withBinaryFile path AppendMode (\h -> B.hPut h $ encode trans)

getTransactions :: FilePath -> IO [Transaction]
getTransactions path = withBinaryFile path ReadMode (\h ->
                            -- get contents not lazily because after this block
                            -- the handle will be closed
                            do bs <- BS.hGetContents h
                               (return . getList) (B.pack $ BS.unpack bs))
    where getList bs | B.null bs = []
                     | otherwise = let (trans, rest, _) = runGetState get bs 0
                                     in trans:(getList rest)

----------------------------------------------------------------------

#ifdef __TEST__
testdataOriginator = Originator "\0\1\2\3\4\5\6\7"

testdataTime = oneMinute `diffUTCTime` epoch
    where oneMinute = readTime defaultTimeLocale "%M" "01" :: UTCTime

testdataApp  = 0xffeeddcc
testdataType = 0xfedc
testdataTransaction =
    mkTransaction testdataApp
                  testdataType
                  testdataOriginator
                  testdataTime
                  "Hello, world."

testdataBinaryTransaction = B.concat
    [ "cjs", "\0", "\0\0\0\0\0\61"              -- magic, ver, len
    , "\xff\xee\xdd\xcc", "\0\0", "\xfe\xdc"    -- app, flags, type
    , "\0\1\2\3\4\5\6\7"                        -- originator
    , "\0\0\0\0\0\0\234\96"                     -- timestamp, 1 min.
    , "Hello, world."                           -- data
    , "_\166\201OjT\a\194a\ESC%o\DC3\148_\236"  -- md5
    ]

_testTransaction = runTests $ "LTR.Transaction" ~: test 
    [ show epoch ~?= "1970-01-01 00:00:00 UTC"
    , mkOriginator 0 ~?= Originator "\0\0\0\0\0\0\0\0"
    , encode testdataTransaction ~?= testdataBinaryTransaction
    , decode testdataBinaryTransaction ~?= testdataTransaction
    ]
#endif
