{-# LANGUAGE UnicodeSyntax #-}

module Main where
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.List

data Dimension = Dimension Int Int Int

part1 (Dimension w h l) = 3*w*h + 2*w*l + 2*h*l
part2 (Dimension w h l) = w + w + h + h + w*h*l

readDim ∷ L.ByteString → Int
readDim str =
  case L.readInt str of
    Nothing         → 0
    Just (value, _) → value

readDimension ∷ [L.ByteString] → Dimension
readDimension [x,y,z] = Dimension w h l
  where
    w = sorted!!0
    h = sorted!!1
    l = sorted!!2
    sorted = sort [readDim x, readDim y, readDim z]
readDimension _ = error "Invalid"

countResult part = Data.List.foldl1' (+) . map ( part . readDimension . L.split 'x' ) . L.lines

main :: IO ()
main = do
        input ← L.getContents
        print $ "Part 1: " ++ show (countResult part1 input)
        print $ "Part 2: " ++ show (countResult part2 input)

