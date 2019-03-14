
tupleAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
tupleAdd (a,b) (c,d) = (a+c, b+d)

count :: String -> (Int,Int)
count []        = (0,0)
count ('\\':xs) = tupleAdd (2,1) (count xs)
count ('"':xs)  = tupleAdd (2,1) (count xs)
count ('\n':xs) = tupleAdd (2,0) (count xs)
count (_:xs)    = tupleAdd (1,1) (count xs)

main :: IO ()
main = do
  input <- getContents
  let result = count input
  print $ "counts: " ++ show result ++ " result: " ++ show (uncurry (-) result)

