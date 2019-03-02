import           Data.Array
import           Data.Bits
import           Text.ParserCombinators.Parsec

type Index = (Int, Int)
type Grid = Array Index Bool
type GridT = Grid -> Grid
type Step = (Bool->Bool) -> (Index, Index) -> Grid -> Grid

pInputP1 :: GenParser Char st [GridT]
pInputP1 = (pLineP1 <* string "\n") `manyTill` eof

pIndexP1 :: CharParser st Index
pIndexP1 = many digit >>= (\i1 -> char ',' >> many digit >>= (\i2 -> return (read i1, read i2)))

pBoundsP1 :: CharParser st (Index, Index)
pBoundsP1 = pIndexP1 >>= (\i1 -> string " through " >> pIndexP1 >>= (\i2 -> return (i1,i2)))

pTurnOnP1 :: CharParser st GridT
pTurnOnP1 = changeLamps (True .|.) <$> (string "turn on " >> pBoundsP1)

pTurnOffP1 :: CharParser st GridT
pTurnOffP1 = changeLamps (False .&.) <$> (string "turn off " >> pBoundsP1)

pToggleP1 :: CharParser st GridT
pToggleP1 = changeLamps not <$> (string "toggle " >> pBoundsP1)

pLineP1 :: CharParser st GridT
pLineP1 =  try pToggleP1 <|> try pTurnOffP1 <|> pTurnOnP1

createGrid :: Int -> Int -> Grid
createGrid w h = array ((0 :: Int, 0 :: Int), (w-1, h-1)) [((x,y), False) | x <- [0..w-1], y <- [0..h-1]]

changeLamps :: Step
changeLamps f ((sx,sy), (ex,ey)) m = m//[ ((x,y), f (m!(x,y)) ) | x <- [sx..ex], y <- [sy..ey]]

main :: IO ()
main = do
  let g = createGrid 1000 1000

  input <- getContents
  print "Starting!"
  case parse pInputP1 "xx" input of
    Left e -> print $ "Parser err" ++ show e
    Right list -> do
      let g' = foldl (\g'' f -> f g'') g list
      print "Lights up:"
      print $ length $ filter (==True) $ elems g'

  putStrLn "Done!"
