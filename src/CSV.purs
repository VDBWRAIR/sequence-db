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
import Data.CSVGeneric 
type Pairs = Array (Tuple String String)

force :: forall a. (Unit -> a) -> a
force = (_ $ unit)


--gEncode :: forall a. (Generic a) => a -> Maybe Pairs
--gEncode = genericEncode <<< toSpine

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
class Encode a where
    encode :: a -> Array (Tuple String String)


