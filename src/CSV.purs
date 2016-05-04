module CSV where
import Data.Array.Unsafe as Unsafe
import Data.Array as A
import Data.Maybe
import Prelude
import Data.Generic (toSpine, g)
type Pairs = Array (Tuple String String)

class Encode a where
    encode :: a -> Array (Tuple String String)

geneircEncode :: GenericSpine -> Maybe (Array (Tuple String String))
genericEncode (SProd s arr) = if ((length s) /= 1) then Nothing else
                                case (A.head arr) of
                                  (Just (SRecord xs)) -> genericEncode (SRecord xs)
                                  _ -> Nothing
genericEncode (SRecord xs) = sequence $ map encodeRecPair xs
  where encodeRecPair x = ((,) (x.recLabel)) <$> encodeSingle (force x.recValue)
genericEncode _ = Nothing

gEncode :: forall a. (Generic a) => a -> Maybe Pairs
gEncode = genericEncode <<< toSpine

encodeSingle :: GenericSpine -> Maybe String
encodeSingle (SProd "Data.Maybe.Just" arr) = encodeSingle (Unsafe.head arr)
encodeSingle (SProd "Data.Maybe.Nothing" arr) = Just ""
encodeSingle (SProd s arr) = if (null arr) then (Just s) else Nothing
encodeSingle (SArray _) = Nothing
encodeSingle (SRecord _) = Nothing 
encodeSingle (SBoolean x) = Just $ show x
encodeSingle (SInt x)     = Just $ show x
encodeSingle (SNumber x)  = Just $ show x
encodeSingle (SString x)  = Just $ show x
encodeSingle (SChar x)    = Just $ show x 

class Decode a where
    decode :: Array (Tuple String String) -> a
