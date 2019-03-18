import qualified Data.ByteString.Lazy as BL
ta :: (Int, Int) -> (Int, Int) -> (Int, Int)
ta (a,b) (c,d) = (a+c, b+d)

cnt :: BL.ByteString -> (Int,Int)
cnt bs = if BL.length bs == 0
              then (0,0)
           else let tbs = BL.tail bs
                    hbs = BL.head bs
                    nextVal = cnt tbs in
                  case hbs of
                    0x5c -> ta (2,1) nextVal
                    0x22 -> ta (2,1) nextVal
                    0x0a -> ta (2,0) nextVal
                    _    -> ta (1,1) nextVal

main :: IO ()
main = do
  i <- BL.getContents
  print (uncurry (-) (cnt i))

