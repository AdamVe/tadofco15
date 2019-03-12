{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
import qualified Data.ByteString.Lazy        as BL
import           Data.List
import           Data.Map
import           Text.Parsec
import           Text.Parsec.ByteString.Lazy

type Gate = String
data Signal = Value Int
            | Reference Gate

data Combi
  = And Signal Signal
  | Or Signal Signal
  | RShift Signal Signal
  | LShift Signal Signal
  | Not Signal
  | Var Signal

type Instruction = Combi

instance Show Signal where
  show (Value v)     = show v
  show (Reference r) = r

instance Show Combi where
  show (And v1 v2)    = show v1 ++ " AND " ++ show v2
  show (Or v1 v2)     = show v1 ++ " OR " ++ show v2
  show (RShift v1 v2) = show v1 ++ " RSHIFT " ++ show v2
  show (LShift v1 v2) = show v1 ++ " LSHIFT " ++ show v2
  show (Not v)        = "NOT " ++ show v
  show (Var v)        = show v

pInput :: Parser [(Gate, Instruction)]
pInput = (pExp <* char '\n') `manyTill` eof

pVal :: Parser Int
pVal = do
  pWspace
  x <- try (string "0") <|> many1 (oneOf "0123456789")
  pWspace
  return $ read x

pVar :: Parser String
pVar = pWspace *> many (oneOf ['a'..'z']) <* pWspace

pGate :: Parser String
pGate = pWspace *> string "->" *> pWspace *> pVar

pWspace :: Parser String
pWspace = many $ oneOf " "

pValue :: Parser Signal
pValue = try (Value <$> pVal) <|> (Reference <$> pVar)

pExp :: Parser (Gate, Instruction)
pExp =
  try (pValue >>= (\ v1 -> string "AND" >> pValue >>= (\ v2 -> pGate >>= (\ gate -> return (gate, And v1 v2))))) <|>
  try (pValue >>= (\ v1 -> string "OR" >> pValue >>= (\ v2 -> pGate >>= (\ gate -> return (gate, Or v1 v2))))) <|>
  try (pValue >>= (\ v1 -> string "RSHIFT" >> pValue >>= (\ v2 -> pGate >>= (\ gate -> return (gate, RShift v1 v2))))) <|>
  try (pValue >>= (\ v1 -> string "LSHIFT" >> pValue >>= (\ v2 -> pGate >>= (\ gate -> return (gate, LShift v1 v2))))) <|>
  try (string "NOT" >> pValue >>= (\ v1 -> pGate >>= (\ gate -> return (gate, Not v1)))) <|>
  try (pValue >>= (\ v1 -> pGate >>= (\gate -> return (gate, Var v1))))

main :: IO ()
main = do
  input <- BL.getContents
  case parse pInput "stdin" input of
    Left e -> print $ "Parser err" ++ show e
    Right list -> do
      let mp = fromList list
      mapM_ print list
