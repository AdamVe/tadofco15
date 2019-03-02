import           Control.Monad                 (liftM)
import           Data.Array
import           Data.Bits
import           Text.ParserCombinators.Parsec

type Index = (Int, Int)
type Grid = Array Index Bool
type GridT = Grid -> Grid
type Step = (Bool->Bool) -> (Index, Index) -> Grid -> Grid

pInput :: GenParser Char st [GridT]
pInput = (pLine <* pEol) `manyTill` eof

pIndex :: CharParser st Index
pIndex = do
  sx <- many digit
  _ <- string ","
  sy <- many digit
  return (read sx, read sy)

pBounds :: CharParser st (Index, Index)
pBounds = pIndex >>= (\i1 -> string " through " >> pIndex >>= (\i2 -> return (i1,i2)))

pTurnOn :: CharParser st GridT
pTurnOn = turnOn <$> (string "turn on " >> pBounds)

pTurnOff :: CharParser st GridT
pTurnOff = turnOff <$> (string "turn off " >> pBounds)

pToggle :: CharParser st GridT
pToggle = toggle <$> (string "toggle " >> pBounds)


pLine :: CharParser st GridT
pLine =  try pToggle <|> try pTurnOff <|> pTurnOn

pEol :: CharParser st String
pEol = try (string "\n\r")
        <|> try (string "\r\n")
        <|> string "\n"
        <|> string "\r"


createGrid :: Int -> Int -> Grid
createGrid w h = array ((0 :: Int, 0 :: Int), (w-1, h-1)) [((x,y), False) | x <- [0..w-1], y <- [0..h-1]]

changeLamps :: Step
changeLamps f ((sx,sy), (ex,ey)) m = m//[ ((x,y), f (m!(x,y)) ) | x <- [sx..ex], y <- [sy..ey]]

turnOn :: (Index, Index) -> Grid -> Grid
turnOn = changeLamps (True .|.)
turnOff = changeLamps (False .&.)
toggle = changeLamps not

main :: IO ()
main = do
  let g = createGrid 1000 1000

  input <- getContents
  print "Starting!"
  case parse pInput "xx" input of
    Left e -> print $ "Parser err" ++ show e
    Right list -> do
      let g' = foldl (\g'' f -> f g'') g list
      print "Lights up:"
      print $ length $ filter (==True) $ elems g'

  putStrLn "Done!"
