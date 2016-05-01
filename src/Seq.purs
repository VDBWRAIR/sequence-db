module App.Seq where
import Data.StrMap as StrMap
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Array.Unsafe (last)
import Data.Array (zip)
import Data.String (split)
import App.Routes (Route(Home, NotFound))
import Pux.Html (Html, div, p, text, table, tr, td, input)
import Pux.Html.Attributes (className, checked, value, type_)
import Prelude --(id, const, ($), show, (<>), (<$>), Eq, (==), (&&), not, (<<<), map, Show)
import Pux.Html.Events (onClick)
import Data.Foldable (Foldable)
import Data.Generic
import Data.Maybe (Maybe, fromMaybe) 
--import Text.Parsing.CSV (defaultParsers, makeParsers)
--import Text.Parsing.Parser (runParser)
import Data.Array.Unsafe (unsafeIndex)
import Data.Array as A
import Data.String as S
import Data.Maybe
import Data.Int as Int 
--import Data.Eulalie.Parser as P
--import Data.Eulalie.String as S


data Format = Fasta | CSV
readFormat = makeRead [Fasta, CSV]

startsWith :: String -> String -> Boolean
startsWith sub s = S.take (S.length sub) s == sub

autoComplete :: (State -> String) -> Array State -> String -> Array String
autoComplete attr recs sub = A.filter (startsWith sub) $ map attr recs

derive instance genericFormat :: Generic Format
instance showFormat :: Show Format where
  show = undot <<< gShow
instance eqFormat :: Eq Format where 
    eq = gEq
    
readHost :: String -> Maybe Host
readHost = makeRead [Human, Mosquito]

data Host = Human | Mosquito
derive instance genericHost :: Generic Host
instance showHost :: Show Host where
    show = undot <<< gShow 
instance eqHost :: Eq Host where
    eq = gEq


undot :: String -> String    
undot s = last $ S.split "." s

readSerotype = makeRead serotypes
serotypes = [DENV1 , DENV2 , DENV3 , DENV4 , HN1 , H1N1 , H5N1 , H3N2 , H7N9]
makeRead :: forall a. (Show a) => Array a -> (String -> Maybe a)
makeRead xs = f 
  where
    f x = StrMap.lookup x m 
      where
        msg = "Could not coerce " <> x <> " to one of " <> (show m)
    m = StrMap.fromFoldable $ zip (map show xs) xs
    
data Serotype = DENV1 | DENV2 | DENV3 | DENV4 | HN1 | H1N1 | H5N1 | H3N2 | H7N9
derive instance genericSerotype :: Generic Serotype 
instance showSerotype :: Show Serotype where
    show = undot <<< gShow 
instance eqSerotype :: Eq Serotype where
    eq = gEq
    
segments = [PB1 , PB2 , PA , HA , NP , NA , M1 , NS1]
readSegment = makeRead [PB1 , PB2 , PA , HA , NP , NA , M1 , NS1]
data Segment = PB1 | PB2 | PA | HA | NP | NA | M1 | NS1
derive instance genericSegment :: Generic Segment
instance showSegment :: Show Segment where
  show = undot <<< gShow
instance eqSegment :: Eq Segment where
  eq = gEq

type Year = Int 
type State = {
       name     :: String
     , acc      :: String
     , year     :: Year 
     , country  :: String 
     , host     :: Host
     , serotype :: Serotype
     , sequence :: String
     , segment  :: Maybe Segment
     , genotype :: Maybe Genotype
     , checked  :: Boolean
     }
             
--columns = map fst [("name", id') ,  ("acc", id') ,  ("country", id') ,  ("year", Int.fromString) ,  ("host", readHost) ,  ("seortype", readSegment) ,  ("segment", maybe' readSegment) ,  ("genotype", maybe' readGenotype)]

-- TODO: include Dates
columns :: Array String 
columns = ["name", "acc", "country", "year", "host", "serotype", "segment", "genotype"]
--    applyRow row = zipWith ($) funcs row
--    funcs = map snd $ A.sortBy headerOrder columns
readCSV :: String -> String -> Maybe (Array State)
readCSV sep s = process <$> (A.head lines') <*> (A.tail lines')
  where
    lines' = map (S.split sep) $ lines s
    lines  = S.split "\n"
        
process :: Array String -> Array (Array String) -> Array State
process header rows = map process' rows
  where
    process' :: Array String -> State
    process' row = {
      name : (row `at` 0)
    , acc  : (row `at` 1)
    , country : (row `at` 2)
    , year : fromMaybe 0 $ Int.fromString (row `at` 3)
    , host : fromMaybe Human $ readHost (row `at` 3)
    , serotype : fromMaybe DENV4 $ readSerotype (row `at` 4)
    , segment : readSegment (row `at` 5)
    , genotype : readGenotype (row `at` 6)
    , checked : false
    , sequence : "DummySeq"
    }
      where at = unsafeIndex
    order row = A.sortBy headerOrder row
    headerOrder x y  = compare (fromMaybe 9999 (A.elemIndex  x header)) (fromMaybe 9999 (A.elemIndex y header))
    
maybe' _ "" = Nothing
maybe' f  x = Just $ f x

stateEq :: State -> State -> Boolean
stateEq x y = x.name == y.name &&
              x.acc  == y.acc  && 
              x.year  == y.year  && 
              x.country  == y.country  && 
              x.host  == y.host  && 
              x.serotype  == y.serotype  && 
              x.segment  == y.segment &&
              x.genotype == x.genotype
              
type Acc = String              
data Action = PageView Route | ToggleCheck
--TODO: add action to delete/check (check for deletion / download)
view :: State -> Html Action
view state = table []
            [tr []
              [ td  [] 
                   [input [type_ "checkbox", checked state.checked, value "selected" , onClick $ const ToggleCheck] [] ]
              , td [className "name"]    [ text $ "Name:  " <> state.name ]
              , td [className "acc"]     [ text $ "Accession:  " <> state.acc ]
              , td [className "year"]    [ text $ "Year:  " <> show state.year ]
              , td [className "country"] [ text $ "Country:  " <> state.country ] ]
          , tr []   [ td []  [ text $ "Host:  " <> show state.host ] 
                 ,  td  [] [ text $ "Serotype:  " <> show state.serotype ] 
                 ,  td  [] [ text $ "Segment:  "  <> (fromMaybe "n/a" $ show <$> state.segment)] 
                 ,  td  [] [ text $ "Genotype:  "  <> (fromMaybe "n/a" $ show <$> state.genotype)] ]]

update :: Action -> State -> State
update ToggleCheck state = state { checked = not state.checked }
update _ state = state 

init :: State -> State
init = id

data Genotype = Genotype1
readGenotype = makeRead genotypes
genotypes = [Genotype1]
derive instance genericGenotype :: Generic Genotype
instance showGenotype :: Show Genotype where
  show = undot <<< gShow
instance eqGenotype :: Eq Genotype where
  eq = gEq
