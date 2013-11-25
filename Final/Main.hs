module Main where
test :: String -> String
test str = str ++ "0"

test2 :: String -> String
test2 str = newStr ++ newStr2
            where
               newStr = test "hello"
               newStr2 = test str

test3 :: a -> b -> (a,b)
test3 x y = (x,y)

test4 :: String -> String
test4 str = let str2 = str ++ str
            in test str2

test5 :: Int -> String -> [String]
test5 0 str = [str]
test5 i str = (:) str $ test5 (i-1) (test str)

main :: IO ()
main = print $ test5 10 "Hello, world"
