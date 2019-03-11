{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
import qualified Data.ByteString.Lazy        as BL
import           Data.List
import           Text.Parsec
import           Text.Parsec.ByteString.Lazy

type Gate = String
data Signal = Value Int
            | Reference String

data Instruction
  = And Gate Signal Signal
  | Or Gate Signal Signal
  | RShift Gate Signal Signal
  | LShift Gate Signal Signal
  | Not Gate Signal
  | Var Gate Signal

instance Show Signal where
  show (Value v)     = show v
  show (Reference r) = r

instance Show Instruction where
  show (And r v1 v2)    = show v1 ++ " AND " ++ show v2 ++ " -> " ++ r
  show (Or r v1 v2)     = show v1 ++ " OR " ++ show v2 ++ " -> " ++ r
  show (RShift r v1 v2) = show v1 ++ " RSHIFT " ++ show v2 ++ " -> " ++ r
  show (LShift r v1 v2) = show v1 ++ " LSHIFT " ++ show v2 ++ " -> " ++ r
  show (Not r v)        = "NOT " ++ show v ++ " -> " ++ r
  show (Var r v)        = show v ++ " -> " ++ show r

pInput :: Parser [Instruction]
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

pExp :: Parser Instruction
pExp =
  try (pValue >>= (\ v1 -> string "AND" >> pValue >>= (\ v2 -> pGate >>= (\ gate -> return (And gate v1 v2))))) <|>
  try (pValue >>= (\ v1 -> string "OR" >> pValue >>= (\ v2 -> pGate >>= (\ gate -> return (Or gate v1 v2))))) <|>
  try (pValue >>= (\ v1 -> string "RSHIFT" >> pValue >>= (\ v2 -> pGate >>= (\ gate -> return (RShift gate v1 v2))))) <|>
  try (pValue >>= (\ v1 -> string "LSHIFT" >> pValue >>= (\ v2 -> pGate >>= (\ gate -> return (LShift gate v1 v2))))) <|>
  try (string "NOT" >> pValue >>= (\ v1 -> pGate >>= (\ gate -> return (Not gate v1 )))) <|>
  try (pValue >>= (\ v1 -> pGate >>= (\gate -> return $ Var gate v1)))

main :: IO ()
main = do
  input <- BL.getContents
  case parse pInput "stdin" input of
    Left e -> print $ "Parser err" ++ show e
    Right list ->
      mapM_ print list
