{-# LANGUAGE UnicodeSyntax #-}

module Main where
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.List
import           Data.Maybe
import           Data.ByteString.Conversion

data Dimension = Dimension Int Int Int

part1 (Dimension w h l) = 3*w*h + 2*w*l + 2*h*l
part2 (Dimension w h l) = w + w + h + h + w*h*l

readDimension ∷ [L.ByteString] → Dimension
readDimension strings = Dimension w h l
  where [w, h, l] = sort $ map (fst . fromJust . L.readInt) strings

countResult part = Data.List.foldl1' (+) . map ( part . readDimension . L.split 'x' ) . L.lines

main :: IO ()
main = do
        input ← L.getContents
        print $ "Part 1: " ++ show (countResult part1 input)
        print $ "Part 2: " ++ show (countResult part2 input)
