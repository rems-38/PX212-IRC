{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

import Cipher
import InvCipher
import Data.Char (ord, intToDigit, chr, digitToInt, isHexDigit)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP
import Text.Read.Lex (lexChar)


-----------------------------------------------------------------
----------------------------- Main ------------------------------
-----------------------------------------------------------------
-- Commande pour "make" : `ghc --make AES.hs -o ./AES`
-- Usage: `./AES encode/decode` OR `./AES -e/-d key msg`
-- Quand utilisé avec -d/-e le msg doit être entre double guillemets (pas forcément nécessaire pour la key)
-- Pas utile quand utilisé avec encode/decode
-- Ex: ./AES -d 2b7e151628aed2a6abf7158809cf4f3c "?\CANX\137\DC4\STX5\DC3\v\204\ETB\198\138\254c\145" -> "bonjour"
--     ./AES -e "2b7e151628aed2a6abf7158809cf4f3c" "bonjour" -> "?\CANX\137\DC4\STX5\DC3\v\204\ETB\198\138\254c\145"

main :: IO ()
main = 
    do
        args <- getArgs
        case args of
            ["encode"] -> do
                (key, msg) <- askKeyMsg
                putStrLn "\nEncoded message : "
                print $ encode key msg
            ["decode"] -> do
                (key, msg) <- askKeyMsg
                putStrLn "\nDecoded message : "
                print $ decode key msg
            ["-e", key, msg] -> do
                putStrLn "\nEncoded message : "
                print $ encode key msg
            ["-d", key, msg] -> do
                putStrLn "\nDecoded message : "
                print $ decode key (fst $ head $ readP_to_S (manyTill lexChar eof) msg)
            _ -> putStrLn "Usage: encode/decode OR -e/-d key msg"

askKeyMsg :: IO (String, String)
askKeyMsg = do
    putStrLn "Enter key : "
    key <- getLine
    putStrLn "Enter message : "
    msg <- getLine
    return (key, fst $ head $ readP_to_S (manyTill lexChar eof) msg)
-----------------------------------------------------------------



-----------------------------------------------------------------
---------------------------- Parser -----------------------------
-----------------------------------------------------------------
stringHex :: String -> Block
stringHex = map (hexPol . decToHex . ord)

hexString :: Block -> String
hexString = map (chr . hexToDec . polHex)

decToHex :: Int -> String
decToHex 0 = "0"
decToHex n = reverse (hexChars n)
   where
      hexChars 0 = ""
      hexChars x = intToDigit (x `mod` 16) : hexChars (x `div` 16)

hexToDec :: String -> Int
hexToDec = foldl (\acc x -> acc * 16 + digitToInt x) 0
-----------------------------------------------------------------



-----------------------------------------------------------------
---------------------------- Encode -----------------------------
-----------------------------------------------------------------
-- Pour la clé : utiliser une string héxadécimale sans espaces
-- Ex : "2b7e151628aed2a6abf7158809cf4f3c" (pour du 128 bits)
encode :: String -> String -> String
encode _ [] = ""
encode key msg | length msg `mod` 16 == 0 = hexString (cipher n (take 16 (stringHex msg)) (toBlock $ space key)) ++ encode key (hexString (drop 16 (stringHex msg)))
               | otherwise = encode key (msg ++ [' '])
               where n | length key == 32 || length key == 48 || length key == 64 = length (toBlock $ space key) * 8
                       | otherwise = error "Key must be 128, 192 or 256 bits long"

space :: String -> String
space [] = []
space [x] = [x]
space (x:y:xs) = x:y:' ':space xs
-----------------------------------------------------------------



-----------------------------------------------------------------
---------------------------- Decode -----------------------------
-----------------------------------------------------------------
decode :: String -> String -> String
decode _ [] = ""
decode key msg = removeEndSpace (hexString (invCipher n (take 16 (stringHex msg)) (toBlock $ space key)) ++ decode key (hexString (drop 16 (stringHex msg))))
                   where n | length key == 32 || length key == 48 || length key == 64 = length (toBlock $ space key) * 8
                           | otherwise = error "Key must be 128, 192 or 256 bits long"
                         removeEndSpace x = reverse $ dropWhile (== ' ') $ reverse x
-----------------------------------------------------------------