{-# LANGUAGE CPP #-}
module Application.FileHandling (detectFiletype, renderHtml) where

    import Application.Types

    import Text.Pandoc
    import GHC.IO.Handle
    import System.IO.Temp

#ifdef CABAL
    import Paths_markup_preview
#endif

    import Control.Applicative
    import Data.List
    import Data.List.Utils
    import Data.Maybe
    import qualified Data.Text.IO as T (readFile)
    import Data.Text (Text)


    detectFiletype :: FilePath -> Maybe FileType
    detectFiletype filepath = fst <$> find (any (`isSuffixOf` filepath) . snd)
                            [ (Markdown, [".markdown", ".md"])
                            , (Textile, [".textile"])
                            , (ReStructuredText, [".rst", ".rest", ".restx"]) ]


    readResource :: FilePath -> IO Text
    readResource filepath =
#ifdef CABAL
        getDataFileName filepath >>= \filepath' -> T.readFile filepath'
#endif
#ifndef CABAL
        T.readFile filepath
#endif


    renderHtml :: (FileType, FilePath) -> IO String
    renderHtml (format, filepath) = readFile filepath >>= writeHtmlFile where
        readerF = fromJust $ lookup format [(Markdown, readMarkdown), (ReStructuredText, readRST), (Textile, readTextile)]
        writer = writeHtmlString def
        reader = readerF (def { readerStandalone = True })
        writeHtmlFile content = do
            -- make this cross OS path style handling
            let tempDirectory = (++ "/") . join "/" . init . split "/" $ filepath
            contents <- readResource "Resources/layout.html"
            let layout = compileTemplate contents
            case layout of
              Left s -> putStrLn s >> return "" -- return error file
              Right t -> do
                  let htmlContent = renderTemplate t $ varListToJSON [("htmlContent", writer $ reader content)]
                  (tempFilePath, tempHandle) <- openTempFile tempDirectory "markup-preview.html"
                  hPutStr tempHandle htmlContent >> hFlush tempHandle
                  return ("file://" ++ tempFilePath)
