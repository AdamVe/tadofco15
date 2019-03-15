ta (a,b)(c,d) = (a + c, b + d)

cnt []        = (0,0)
cnt ('\\':xs) = ta (2,1) (cnt xs)
cnt ('"':xs)  = ta (2,1) (cnt xs)
cnt ('\n':xs) = ta (2,0) (cnt xs)
cnt (_:xs)    = ta (1,1) (cnt xs)

main = do
  i <- getContents
  print (uncurry (-) (cnt i))

