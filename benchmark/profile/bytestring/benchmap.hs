import Data.List
import Data.Word
import Data.ByteString

ws = [1..1000000] :: [Word8]
bs = pack ws
res = Data.ByteString.map (\b -> b ^ b) bs

main = do
    Prelude.putStr $ show bs
