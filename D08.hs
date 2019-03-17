import qualified Data.ByteString.Lazy as BL
tupleAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
tupleAdd (a,b) (c,d) = (a+c, b+d)

count :: BL.ByteString -> (Int,Int)
count bs = if BL.length bs == 0
              then (0,0)
           else let tbs = BL.tail bs
                    hbs = BL.head bs
                    nextVal = count tbs in
                  case hbs of
                    0x5c -> tupleAdd (2,1) nextVal
                    0x22 -> tupleAdd (2,1) nextVal
                    0x0a -> tupleAdd (2,0) nextVal
                    _    -> tupleAdd (1,1) nextVal

main :: IO ()
main = do
  i <- BL.getContents
  let r = count i
  print $ "counts: " ++ show r ++ " result: " ++ show (fst r - snd r)

