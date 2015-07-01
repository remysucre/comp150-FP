{-# OPTIONS_GHC -XOverloadedStrings #-}

{-
    Read or write a transaction to or from a disk file.

    Usage:
        ttool append TFILE APP TYPE (-d DATA | -f FILE) [-t TS]
        ttool dump TFILE 
        ttool undump FILE

    Commands:
        append  Append a transaction to a file. If the file does not
                exist, it will be created.
        dump    Dump, in human-readable format, the transactions from
                a transaction file.
        undump  Read dump output (though many input fields are optional)
                and convert it to binary format, sending it to stdout.

    Arguments:
        TFILE   a transaction log file.
        APP     application number (32-bit unsigned decimal)
        TYPE    type number (16-bit unsigned decimal)

    Options:
        Options may be interspersed anywhere within the command line.
        Files that start with a hyphen should be specified as "./-name".

        -t TS       Specify a timestamp, in milliseconds since the epoch.
                    If not specified, the current time is used.
        -d DATA     Specify as a string data to be written to the file.
        -f FILE     Specify a file containing the transaction data.

    Notes:

        Application Numbers: 1 testing, 2 mhailist

        Add an option for lock testing? This could write part of the
        transaction, delay, and then write the other part. While that's
        running in the background, you can run another one on the same
        file in the foreground and ensure that a valid transaction log
        is produced.

        Until we get the originator stuff worked out, the originator is
        always zero. Probably we would have some way of generating a
        default originator (host info plus PID?) and a -o <originator>
        option to let us override it.

        We don't do stdin/out or network at the moment, because those can't
        be locked. Should we try to add some support for this? Or just leave
        that to other programs?

        In the long run, we should probably add an interactive mode (-i)
        which will let us do various kinds of manipulations.

-}

module LTR.Ttool (main) where

import Data.Binary (encode)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Time.Format (readTime)
import LTR.TextTrans (transToText, textTransactionsParser)
import LTR.Transaction
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Locale (defaultTimeLocale)
import Text.ParserCombinators.Parsec (parse)
import qualified Data.ByteString.Lazy as B

----------------------------------------------------------------------

data Command
    = AppendCommand FilePath Int Int [Flag] -- file app type flags
    | DumpCommand FilePath [Flag]
    | UndumpCommand FilePath
    | CommandError String
    deriving Show

data Flag
    = TransData String
    | TransDataFile String
    | TransTimestamp String
    deriving (Show)

options :: [OptDescr Flag]
options =
    [ Option ['d'] [] (ReqArg (\s -> TransData s) "DATA")
        "transaction data"
    , Option ['f'] [] (ReqArg (\s -> TransDataFile s) "FILE")
        "FILE containing transaction data"
    , Option ['t'] [] (ReqArg (\s -> TransTimestamp s) "TS")
        "timestamp, in milliseconds since the epoch."
    ]

parseCommandLine :: [String] -> Command
parseCommandLine argv =
    case getOpt Permute options argv of
         (flags, "append":path:app:ttype:[], [])
             -> AppendCommand path (read app) (read ttype) flags
         (flags, "dump":path:[], [])    -> DumpCommand path flags
         (_, "undump":path:[], [])      -> UndumpCommand path
         err                            -> CommandError (show err)

append path app ttype writeData =
    do
        putStrLn $ "writing :" ++ show trans ++ "\n\n" 
        appendTransaction path trans
    where 
        tempOriginator = mkOriginator 0 
        oneMinute = readTime defaultTimeLocale "%M" "01" :: UTCTime 
        epoch = readTime defaultTimeLocale "" "" :: UTCTime
        tempTimeStamp = oneMinute `diffUTCTime` epoch
        trans = mkTransaction 
            (fromIntegral app) 
            (fromIntegral ttype) 
            tempOriginator 
            tempTimeStamp 
            writeData

dump :: FilePath -> IO ()
dump path = getTransactions path >>= mapM_ (putStr.transToText)

undump :: FilePath -> IO ()
undump path =
    do input <- readFile path
       case parse textTransactionsParser path input of
            Left err -> error $ show err
            Right ts -> B.putStr $ B.concat $ map encode ts

usage :: String -> IO ()
usage msg = 
    error $ concat
        [ "Invalid arguments: ", msg, "\n\n"
        , "Usage: ttool append TFILE APP TYPE "
        , "(-d DATA | -f FILE) [-t TS] \n"
        , "       ttool dump TFILE\n"
        ]

main :: IO ()
main =
    do argv <- getArgs
       case parseCommandLine argv of
            CommandError s                     -> usage s
            DumpCommand path _flags            -> dump path 
            UndumpCommand path                 -> undump path
            AppendCommand path app ttype
                [TransData writeData] -> append path app ttype (pack writeData)
            AppendCommand path app ttype
                [TransDataFile file]  -> B.readFile file >>=
                                         append path app ttype
            AppendCommand _ _ _ _              ->
                    error "No transaction data or file given"
