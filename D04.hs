module Main where
import qualified Crypto.Hash.MD5            as MD5
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.List
import           Data.Monoid
import           Numeric                    (showHex)

-- challenge = C.pack "yzbqklnj"
challenge = C.pack "abcdef"

getmd5 :: Int -> C.ByteString
getmd5 i = MD5.hash $ challenge <> C.pack (show i)
--getmd5 i = MD5.hash $ C.pack (show i)

convertToHex :: C.ByteString -> String
convertToHex = LC.unpack . toLazyByteString . byteStringHex

showBytes :: C.ByteString -> IO ()
showBytes bs =
  case C.uncons bs of
    Nothing -> return ()
    Just (w8, rest) -> do
      print w8
      showBytes rest

check :: C.ByteString -> Bool
check bs = C.take 2 bs == C.pack [toEnum 0, toEnum 0]

-- toLazyByteString . byteStringHex
main :: IO ()
main = do
  let testMD5 = getmd5 609043
  print $ "MD5 abcdef609043 = " ++ (convertToHex testMD5)
  showBytes testMD5
  print $ show $ check testMD5
  print $ findIndex check [getmd5 i | i <- [0..]]
  print "done"
