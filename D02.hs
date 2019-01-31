module Main where
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.List
import           Data.Maybe

p1 (w,h,l) = 3*w*h + 2*w*l + 2*h*l
p2 (w,h,l) = w + w + h + h + w*h*l

c s = (w,h,l)
  where [w, h, l] = sort $ map (fst . fromJust . L.readInt) s

run part = foldl1' (+) . map ( part . c . L.split 'x' ) . L.lines

main = do
        input <- L.getContents
        print $ show (run p1 input) ++ "," ++ show (run p2 input)
