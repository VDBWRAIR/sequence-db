module CSV where
import Prelude
import Unsafe.Coerce
import Data.Generic
import Data.Either
import Data.Array as A
import Data.Array.Unsafe as Unsafe
import Data.Eulalie.Parser as P
import Data.Eulalie.String as S
import App.Seq (Serotype, serotypes, Segment, segments)
import Data.Int as Int
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Data.Eulalie.Parser (Parser(Parser))
import Data.Eulalie.Result (ParseResult(Success))
import Data.Eulalie.Stream (stream)
import Data.Eulalie.Success (ParseSuccess)
import Data.Foldable (foldr)
import Data.Generic (toSpine, GenericSpine(..), class Generic, fromSpine)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Type.Proxy (Proxy(..))
type Pairs = Array (Tuple String String)

force :: forall a. (Unit -> a) -> a
force = (_ $ unit)

class Encode a where
    encode :: a -> Array (Tuple String String)

genericEncode :: GenericSpine -> Maybe (Array (Tuple String String))
genericEncode (SProd s arr) = if ((A.length arr) /= 1) then Nothing else
                                case (force <$> (A.head arr)) of
                                  (Just (SRecord xs)) -> genericEncode (SRecord xs)
                                  _ -> Nothing
genericEncode (SRecord xs) = sequence $ map encodeRecPair xs
  where
    encodeRecPair x = (Tuple x.recLabel) <$> (encodeSingle $ force x.recValue)
        
genericEncode _ = Nothing

gEncode :: forall a. (Generic a) => a -> Maybe Pairs
gEncode = genericEncode <<< toSpine

encodeSingle :: GenericSpine -> Maybe String
encodeSingle (SProd "Data.Maybe.Just" arr) = encodeSingle $ force (Unsafe.head arr)
encodeSingle (SProd "Data.Maybe.Nothing" arr) = Just ""
encodeSingle (SProd s arr) = if (A.null arr) then (Just s) else Nothing
encodeSingle (SArray _) = Nothing
encodeSingle (SRecord _) = Nothing 
encodeSingle (SBoolean x) = Just $ show x
encodeSingle (SInt x)     = Just $ show x
encodeSingle (SNumber x)  = Just $ show x
encodeSingle (SString x)  = Just $ show x
encodeSingle (SChar x)    = Just $ show x

class Decode a where
    decode :: Array (Tuple String String) -> a

import Data.Eulalie.Char as C

--decodeSingle :: forall a. GenericSpine -> Either String (Parser a)
--decodeSingle (SProd "Data.Maybe.Just" arr) = encodeSingle $ force (Unsafe.head arr)
--decodeSingle (SProd "Data.Maybe.Nothing" arr) = Just ""
--decodeSingle (SProd s arr) = if (A.null arr) then (Just s) else Nothing

---- convert these to typeclass instances
--decodeSingle (SBoolean _) = Right ((fromShow true) <|> (fromShow false))
--decodeSingle (SInt _)     = Right $ (C.many1 C.digit) <$> Int.fromString
--decodeSingle (SString _)  = Right $ C.many1 C.char
--decodeSingle (SChar _)    = Right P.item
--decodeSingle (SArray _) =  Left "Array not supported"
--decodeSingle (SRecord _) = Left "Record not supported"
--decodeSingle (SNumber _)  = Left "number not supported"
-- create a Parseable typeclass. If all members can implement that -- should work
-- instance Parseable Serotype where
--   parse = segment
parseFoo' s = P.parse (S.string "Foo" *> (P.succeed Foo) <|>
                       S.string "Bar" *> (P.succeed Bar))
              (stream s)
-- use seqeunce $ lookup functions

fromShow :: forall a. (Show a) => a -> Parser a
fromShow a = S.string (show a) *> (P.succeed a)

segment :: Parser Segment
segment = foldr (<|>) P.fail $ map fromShow segments

serotype :: Parser Serotype
serotype = foldr (<|>) P.fail $ map fromShow serotypes


toMaybe x = case x of
  (Success ({value : value})) -> Just value
  _                           -> Nothing
  
parseFoo s = case (parseFoo' s) of
  (Success ({value : value})) -> Right value
  _                           -> Left "Bad"

--convertF :: GenericSpine -> P.Parser a

--convertF (SProd "Data.Maybe.Just" arr) = encodeSingle $ force (Unsafe.head arr)
--convertF (SProd "Data.Maybe.Nothing" arr) = Just ""
--convertF (SProd s arr) = if (A.null arr) then (Just s) else Nothing
--convertF (SArray _) = Nothing
--convertF (SRecord _) = Nothing 
--convertF (SBoolean x) = Just $ show x
--convertF (SInt x)     = Just $ show x
--convertF (SNumber x)  = Just $ show x
--convertF (SString x)  = Just $ show x
--convertF (SChar x)    = Just $ show x

--read :: forall a. (Generic a) => Proxy a -> String -> Maybe a
--read p s = fromSpine $ (SProd s [])

foreign import read :: forall a. String -> a
readFoo :: String -> Foo
readFoo s = unsafeCoerce $ read s 
data Foo = Foo | Bar
derive instance gerericFoo :: Generic Foo
instance eqFoo :: Eq Foo where
  eq = gEq 
instance showFoo :: Show Foo where
  show = gShow
