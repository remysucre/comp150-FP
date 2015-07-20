import Data.List
import Data.Word
import Data.ByteString

sum' :: Word8 -> Word8 -> (Word8, Word8)
sum' a b = (a + b, a * b)

ws = Data.List.replicate 1000000 75 :: [Word8]
bs = pack ws
res = Data.ByteString.mapAccumL sum' 1 bs

main = do
    --Prelude.putStrLn $ show res
    Prelude.putStrLn $ show bs
