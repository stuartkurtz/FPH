module Main where

import Data.Char ( isAlpha,  isAscii, toLower )
import Data.Map ( Map )
import qualified Data.Map as M
import Text.Printf ( printf )

frequencies :: Ord a => [a] -> Map a Int
frequencies = M.unionsWith (+) . map (`M.singleton` 1)

main :: IO ()
main = do
  input <- getContents
  let freqs = frequencies . map toLower . filter isAlpha . filter isAscii $ input
  putStr . unlines $ [ printf "%c: %d" c n 
                     | c <- ['a'..'z']
                     , let n = M.findWithDefault 0 c freqs
                     ]
  
