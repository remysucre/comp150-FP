import Data.List
import Data.Word
import Data.ByteString

ws = Data.List.replicate 1000000 255 :: [Word8]
bs = pack ws
res = Data.ByteString.map (\b -> b ^ b) bs

main = do
    Prelude.putStr $ show res
