{-# LANGUAGE LambdaCase #-}

module Main where

import System.IO.Unsafe
import System.Random

randomRangeInt :: Int -> Int -> Int -> Int -> [IO Int]
randomRangeInt begin end min max
             | begin >= end = [(curry randomRIO) min max]
             | otherwise    =  (curry randomRIO) min max : randomRangeInt (begin + 1) end min max

generateGrid :: Int -> Int -> [[IO Int]]
generateGrid width 1      = [randomRangeInt 1 width 0 1]
generateGrid width height =  randomRangeInt 1 width 0 1 : generateGrid width (height - 1)

intListToStringList :: [IO Int] -> [IO String]
intListToStringList = map (>>= (\case 1 -> return "#"
                                      0 -> return "."))

generationToString :: [[IO Int]] -> [[IO String]]
generationToString = map intListToStringList

stringifyList :: [IO String] -> IO String
stringifyList []               = return ""
stringifyList (charIO:charIOs) = charIO >>= (\char -> (<> char) <$> (stringifyList charIOs))

stringifyMatrix :: [[IO String]] -> IO String
stringifyMatrix []         = return ""
stringifyMatrix [[]]       = return ""
stringifyMatrix (lIO:lIOs) = (stringifyList lIO) >>= (\string -> (<> (string <> "\n")) <$> (stringifyMatrix lIOs))

instance Show a => Show (IO a) where
    show io = show (unsafePerformIO io) -- Just to show things, please don't be mad at me :')

main :: IO ()
main = putStr . unsafePerformIO . stringifyMatrix . generationToString $ generateGrid 25 25
