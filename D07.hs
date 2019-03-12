{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
import           Data.Bits
import qualified Data.ByteString.Lazy        as BL
import           Data.List
import           Data.Map
import           Data.Word
import           Debug.Trace
import           Text.Parsec
import           Text.Parsec.ByteString.Lazy

type Gate = String
data Signal = Value Word16
            | Reference Gate

data Instruction
  = And Signal Signal
  | Or Signal Signal
  | RShift Signal Int
  | LShift Signal Int
  | Not Signal
  | Var Signal

instance Show Signal where
  show (Value v)     = show v
  show (Reference r) = r

instance Show Instruction where
  show (And v1 v2)    = show v1 ++ " AND " ++ show v2
  show (Or v1 v2)     = show v1 ++ " OR " ++ show v2
  show (RShift v1 v2) = show v1 ++ " RSHIFT " ++ show v2
  show (LShift v1 v2) = show v1 ++ " LSHIFT " ++ show v2
  show (Not v)        = "NOT " ++ show v
  show (Var v)        = show v

pInput :: Parser [(Gate, Instruction)]
pInput = (pExp <* char '\n') `manyTill` eof

pVal :: Parser Word16
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
  try (pValue >>= (\ v1 -> string "RSHIFT" >> many (oneOf " 0123456789") >>= (\ v2 -> pGate >>= (\ gate -> return (gate, RShift v1 (read v2)))))) <|>
  try (pValue >>= (\ v1 -> string "LSHIFT" >> many (oneOf " 0123456789") >>= (\ v2 -> pGate >>= (\ gate -> return (gate, LShift v1 (read v2)))))) <|>
  try (string "NOT" >> pValue >>= (\ v1 -> pGate >>= (\ gate -> return (gate, Not v1)))) <|>
  try (pValue >>= (\ v1 -> pGate >>= (\gate -> return (gate, Var v1))))

evalSignal :: Signal -> Map String Instruction -> Word16
evalSignal s m = case s of
    Value i      -> i --trace ("Evaluating value " ++ show i) i
    Reference g' -> (eval m g') -- trace ("Evaluating ref " ++ g') (eval m g')

eval :: Map String Instruction -> Gate -> Word16
eval m g = case m!g of
  Var v      -> trace ("Var " ++ show v) (evalSignal v m)
  And v1 v2  -> trace ("And " ++ show v1 ++ " " ++ show v2) (evalSignal v1 m .&. evalSignal v2 m)
  Or v1 v2   -> trace ("Or " ++ show v1 ++ " " ++ show v2) (evalSignal v1 m .|. evalSignal v2 m)
  LShift v s -> trace ("Lshift " ++ show v ++ " " ++ show s) (shift (evalSignal v m) s)
  RShift v s -> trace ("Rshift " ++ show v ++ " " ++ show (negate s)) (shift (evalSignal v m) (negate s))
  Not v      -> trace ("Not " ++ show v) (complement $ evalSignal v m)

main :: IO ()
main = do
  input <- BL.getContents
  case parse pInput "stdin" input of
    Left e -> print $ "Parser err" ++ show e
    Right list -> do
      let mp = fromList list
      let result = eval mp "a"
      mapM_ print list
      print $ "a -> " ++ show result
      -- print $ "x -> " ++ show (eval mp "x")
      -- print $ "y -> " ++ show (eval mp "y")
      -- print $ "d -> " ++ show (eval mp "d")
      -- print $ "e -> " ++ show (eval mp "e")
      -- print $ "f -> " ++ show (eval mp "f")
      -- print $ "g -> " ++ show (eval mp "g")
      -- print $ "h -> " ++ show (eval mp "h")
      -- print $ "i -> " ++ show (eval mp "i")
