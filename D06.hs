import qualified Data.ByteString as BL hiding (foldl')
import           Data.List       hiding (length)
import           Data.Set        as S hiding (foldl')
import           Text.Parsec

pInput = (pLine <* string "\n") `manyTill` eof
pIndex = many digit >>= (\i1 -> char ',' >> many digit >>= (\i2 -> return (read i1 :: Int, read i2 :: Int)))
pBounds = pIndex >>= (\i1 -> string " through " >> pIndex >>= (\i2 -> return (i1,i2)))
pLine =  try (toggleLamps <$> (string "toggle " >> pBounds))
  <|> try (turnLampsOff <$> (string "turn off " >> pBounds))
  <|> toggleLamps <$> (string "toggle " >> pBounds)

lamps ((sx, sy), (ex, ey)) = fromAscList [(x,y)| x <- [sx..ex], y <- [sy..ey]]

turnLampsOn ((sx,sy), (ex,ey)) s = S.union s $ lamps ((sx,sy),(ex,ey))
turnLampsOff ((sx,sy), (ex,ey)) s = S.difference s $ lamps ((sx,sy),(ex,ey))
toggleLamps ((sx,sy), (ex,ey)) s = S.union (S.difference s isection) notIn
  where isection = S.intersection s queryLamps
        notIn = S.difference queryLamps isection
        queryLamps = lamps ((sx,sy),(ex,ey))

main :: IO ()
main = do
  input <- BL.getContents
  case parse pInput "stdin" input of
    Left e -> print $ "Parser err" ++ show e
    Right list -> do
      let g' = foldl' (\g'' f -> f g'') S.empty list
      print $ "# of lights up: " ++ show (length $ elems g')
