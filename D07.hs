import           Control.Monad.State
import           Data.Bits
import qualified Data.ByteString.Lazy        as BL
import           Data.Map
import           Data.Word
import           Text.Parsec                 hiding (State)
import           Text.Parsec.ByteString.Lazy

type WireId = String
type Signal = Word16
type WireSourceMap = Map WireId SignalSource

data SignalSource
  = AndGate SignalSource SignalSource
  | OrGate SignalSource SignalSource
  | RShiftGate SignalSource Int
  | LShiftGate SignalSource Int
  | NotGate SignalSource
  | SpecificValue Signal
  | Wire WireId deriving (Show)

pInput :: Parser [(WireId, SignalSource)]
pInput = (pLine <* endOfLine) `manyTill` eof

pSignal :: Parser Signal
pSignal =  read <$> many1 digit

pWireId :: Parser WireId
pWireId = many1 lower

pOutputWire :: Parser WireId
pOutputWire = string " -> " *> pWireId

pValue :: Parser SignalSource
pValue = (SpecificValue <$> pSignal) <|> (Wire <$> pWireId)

pLine :: Parser (WireId, SignalSource)
pLine =
  (string "NOT " >> pValue >>= (\ v1 -> pOutputWire >>= (\ wire -> return (wire, NotGate v1)))) <|>
  (pValue >>=
   \ v1 ->
     try (string " AND " >> pValue >>= (\ v2 -> pOutputWire >>= (\ wire -> return (wire, AndGate v1 v2)))) <|>
     try (string " OR " >> pValue >>= (\ v2 -> pOutputWire >>= (\ wire -> return (wire, OrGate v1 v2)))) <|>
     try (string " RSHIFT " >> read <$> many digit >>= (\ v2 -> pOutputWire >>= (\ wire -> return (wire, RShiftGate v1 v2)))) <|>
     try (string " LSHIFT " >> read <$> many digit >>= (\ v2 -> pOutputWire >>= (\ wire -> return (wire, LShiftGate v1 v2)))) <|>
     (pOutputWire >>= (\wire -> return (wire, v1))))

getWireSignalSource :: WireId -> State WireSourceMap SignalSource
getWireSignalSource g = state $ \m -> let i = m!g
                                      in (i, m)

setWireValue :: WireId -> Signal -> State WireSourceMap ()
setWireValue g v = state $ \m -> ((), insert g (SpecificValue v) m)

evalSignal :: SignalSource -> State WireSourceMap Signal
evalSignal (SpecificValue v) = return v
evalSignal (Wire w) = do
  signalSource <- getWireSignalSource w
  s <- evalSignal signalSource
  setWireValue w s
  return s
evalSignal (AndGate s1 s2) = do
  r1 <- evalSignal s1
  r2 <- evalSignal s2
  return (r1 .&. r2)
evalSignal (OrGate s1 s2) = do
  r1 <- evalSignal s1
  r2 <- evalSignal s2
  return (r1 .|. r2)
evalSignal (RShiftGate v b) = do
  s <- evalSignal v
  return (shift s (negate b))
evalSignal (LShiftGate v b) = do
  s <- evalSignal v
  return (shift s b)
evalSignal (NotGate v) = do
  r <- evalSignal v
  return (complement r)

main :: IO ()
main = do
  input <- BL.getContents
  case parse pInput "stdin" input of
    Left e -> print $ "Parser err" ++ show e
    Right list -> do
      let a = runState (evalSignal (Wire "a")) $ fromList list
      print a
