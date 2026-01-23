module Main where

import Data.Char ( isAlpha, isAscii, toLower )
import qualified Data.List as L
import Data.Map ( Map )
import qualified Data.Map as M
import Text.Printf ( printf )

frequencies :: Ord a => [a] -> Map a Int
frequencies = M.unionsWith (+) . map (`M.singleton` 1)

invertMap :: Ord b => Map a b -> Map b [a]
invertMap m = M.unionsWith (++) [ M.singleton b [a] | (a,b) <- M.assocs m ]

main :: IO ()
main = do
  input <- getContents
  let freqs = frequencies . map toLower . filter isAlpha . filter isAscii $ input
      inv = invertMap freqs
  putStr . unlines $ [ printf "%c: %d" c n 
                     | c <- ['a'..'z']
                     , let n = M.findWithDefault 0 c freqs
                     ]
  putStrLn ""
  putStr . unlines $ [ printf "%d: %s" n chars
                     | (n,cs) <- M.toDescList inv
                     , let chars = unwords $ map L.singleton cs
                     ]
