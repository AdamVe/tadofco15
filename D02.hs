module Main where
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import Data.Maybe

p1 (w,h,l)=3*w*h+2*w*l+2*h*l
p2 (w,h,l)=2*w+2*h+w*h*l

c s=(w,h,l)
  where [w,h,l]=sort$map(fst.fromJust.L.readInt)s

r p=foldl1'(+).map(p.c.L.split 'x').L.lines

main=do
  i<-L.getContents
  print$r p1 i
  print$r p2 i
