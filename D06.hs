import           Data.Array
import           Data.Bits
import qualified Data.ByteString.Lazy        as BL
import           Data.List
import           Text.Parsec
import           Text.Parsec.ByteString.Lazy

type Index = (Int, Int)
type Grid = Array Index Bool
type GridT = Grid -> Grid
type Step = (Bool->Bool) -> (Index, Index) -> Grid -> Grid

pInput :: Parser [GridT]
pInput = (pLine <* string "\n") `manyTill` eof

pIndex :: Parser Index
pIndex = many digit >>= (\i1 -> char ',' >> many digit >>= (\i2 -> return (read i1, read i2)))

pBounds :: Parser (Index, Index)
pBounds = pIndex >>= (\i1 -> string " through " >> pIndex >>= (\i2 -> return (i1,i2)))

pTurnOn :: Parser GridT
pTurnOn = changeLamps (True .|.) <$> (string "turn on " >> pBounds)

pTurnOff :: Parser GridT
pTurnOff = changeLamps (False .&.) <$> (string "turn off " >> pBounds)

pToggle :: Parser GridT
pToggle = changeLamps not <$> (string "toggle " >> pBounds)

pLine :: Parser GridT
pLine =  try pToggle <|> try pTurnOff <|> pTurnOn

createGrid :: Int -> Int -> Grid
createGrid w h = array ((0 :: Int, 0 :: Int), (w-1, h-1)) [((x,y), False) | x <- [0..w-1], y <- [0..h-1]]

changeLamps :: Step
changeLamps f ((sx,sy), (ex,ey)) m = m//[ ((x,y), f (m!(x,y)) ) | x <- [sx..ex], y <- [sy..ey]]

main :: IO ()
main = do
  let g = createGrid 1000 1000

  input <- BL.getContents
  case parse pInput "stdin" input of
    Left e -> print $ "Parser err" ++ show e
    Right list -> do
      let g' = foldl' (\g'' f -> f g'') g list
      print $ "# of lights up: " ++ show (length $ filter (==True) $ elems g')
