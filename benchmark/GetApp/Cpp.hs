import Language.Preprocessor.Cpphs
import System.Environment
import Control.Applicative
import Data.Functor

opts = CpphsOptions { infiles = [], outfiles = []
                    , defines = map asTrue oplist, includes = []
                    , preInclude = []
                    , boolopts = defaultBoolOptions }

asTrue x = (x, "True")

oplist = ["MIN_VERSION_Cabal(a,b,c)"
        , "MIN_VERSION_OpenGL(a,b,c)"
        , "MIN_VERSION_accelerate(a,b,c)"
        , "MIN_VERSION_bytestring(a,b,c)"
        , "MIN_VERSION_cprng_aes(a,b,c)"
        , "MIN_VERSION_directory(a,b,c)"
        , "MIN_VERSION_exceptions(a,b,c)"
        , "MIN_VERSION_ghc(a,b,c)"
        , "MIN_VERSION_haskell_src_exts(a,b,c)"
        , "MIN_VERSION_http_conduit(a,b,c)"
        , "MIN_VERSION_mtl(a,b,c)"
        , "MIN_VERSION_optparse_applicative(a,b,c)"
        , "MIN_VERSION_pandoc(a,b,c)"
        , "MIN_VERSION_process(a,b,c)"
        , "MIN_VERSION_stm(a,b,c)"
        , "MIN_VERSION_time(a,b,c)"
        , "MIN_VERSION_transformers(a,b,c)"
        , "MIN_VERSION_utf8_string(a,b,c)"
        , "MIN_VERSION_template_haskell(2,10,0)"
        , "MIN_VERSION_base(a,b,c)"]


main = do 
    fp <- head <$> getArgs
    fc <- readFile fp
    out <- runCpphs opts fp fc
    putStrLn out
