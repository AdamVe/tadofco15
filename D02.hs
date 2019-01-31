{-# LANGUAGE UnicodeSyntax #-}

module Main where
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List

part1 (w:h:l:[]) = 3*w*h + 2*w*l + 2*h*l
part2 (w:h:l:[]) = w + w + h + h + w*h*l

readDim ∷ L.ByteString → Int
readDim str =
  case L.readInt str of
    Nothing → 0
    Just (value, _) → value

readDims ∷ [L.ByteString] → [Int]
readDims (x:y:z:[]) = sort [readDim x, readDim y, readDim z]

countResult part =Data.List.foldl1' (+) . map ( part . readDims . L.split 'x' ) . L.lines
  
main :: IO ()
main = do
        input ← L.getContents
        print $ "Part 1: " ++ show (countResult part1 input)
        print $ "Part 2: " ++ show (countResult part2 input)

