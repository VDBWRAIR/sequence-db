module App.Seq where
import Data.Either
import Test.QuickCheck.Gen as Gen
import Prelude
import Data.Generic
import Data.Maybe
import Data.Array as A
import Data.Date as Date
import Data.Int as Int
import Data.StrMap as StrMap
import Data.String as S
import App.Routes (Route(Home, NotFound))
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Array (zip)
import Data.Array.Unsafe (last)
import Data.Array.Unsafe (unsafeIndex)
import Data.Foldable (Foldable, intercalate)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (split)
import Data.Traversable (sequence)
import Pux.Html (Html, div, p, text, table, tr, td, input)
import Pux.Html.Attributes (className, checked, value, type_)
import Pux.Html.Events (onClick)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Data.CSVGeneric 
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

makeRead :: forall a. (Show a) => Array a -> (String -> Maybe a)
makeRead xs = f 
  where
    f x = StrMap.lookup x m 
      where
        msg = "Could not coerce " <> x <> " to one of " <> (show m)
    m = StrMap.fromFoldable $ zip (map show xs) xs
    
derive instance genericSerotype :: Generic Serotype 
instance showSerotype :: Show Serotype where
    show = undot <<< gShow 
instance eqSerotype :: Eq Serotype where
    eq = gEq
    
data Serotype = DENV1 | DENV2 | DENV3 | DENV4 | HN1 | H1N1 | H5N1 | H3N2 | H7N9
serotypes = [DENV1 , DENV2 , DENV3 , DENV4 , HN1 , H1N1 , H5N1 , H3N2 , H7N9]

    
instance arbitrarySerotype :: Arbitrary Serotype where
  arbitrary = Gen.elements DENV1 serotypes
    
segments = [PB1 , PB2 , PA , HA , NP , NA , M1 , NS1]
readSegment = makeRead [PB1 , PB2 , PA , HA , NP , NA , M1 , NS1]
data Segment = PB1 | PB2 | PA | HA | NP | NA | M1 | NS1
derive instance genericSegment :: Generic Segment
instance showSegment :: Show Segment where
  show = undot <<< gShow
instance eqSegment :: Eq Segment where
  eq = gEq

newtype Row = Row {
                  name :: String
                , year :: Int
                , date :: Maybe String
                , continent :: Maybe String
                , subtype :: Maybe String
                , databaseName :: Maybe String 
                , accHA     :: String
                , segmentHA     :: String
                , accMP     :: String
                , segementMP     :: String
                , accNA     :: String
                , segmentNA     :: String
                , accNP     :: String
                , segmentNP     :: String
                , accNS     :: String
                , segmentNS     :: String
                , accPA     :: String
                , segmentPA     :: String
                , accPB1     :: String
                , segmentPB1     :: String
                , accPB2     :: String
                , segmentPB2     :: String
                }
derive instance genericRow :: Generic Row
instance showRow :: Show Row where
  show = undot <<< gShow
instance eqRow :: Eq Row where
  eq = gEq
  
newtype Entry = Entry {
       name     :: String
     , acc      :: String
     , country  :: String 
     , serotype :: Serotype
     , segment  :: Maybe Segment
     , genotype :: Maybe Genotype
     , sequence :: String
     , hostString :: String
     , month :: Maybe Int
     , year :: Int
     , day :: Maybe Int
     --, date     :: Date.Date
}
derive instance genericEntry :: Generic Entry
instance showEntry :: Show Entry where
  show = undot <<< gShow
instance eqEntry :: Eq Entry where
  eq = gEq

type Year = Int 
type State = {
       name     :: String
     , acc      :: String
     , country  :: String 
     , host     :: Host
     , serotype :: Serotype
     , segment  :: Maybe Segment
     , genotype :: Maybe Genotype
     , sequence :: String
     , hostString :: String
     , checked  :: Boolean
     , date     :: Date.Date
     , month :: Maybe Int
     , year :: Int
     , day :: Maybe Int
     }
data Separator = Comma | Tab
--columns = map fst [("name", id') ,  ("acc", id') ,  ("country", id') ,  ("year", Int.fromString) ,  ("host", readHost) ,  ("seortype", readSegment) ,  ("segment", maybe' readSegment) ,  ("genotype", maybe' readGenotype)]

-- TODO: include Dates
toEither _ (Just x) = Right x
toEither z Nothing  = Left z

columns :: Array String 
columns = ["name", "acc", "country",  "year", "host", "serotype", "segment", "genotype", "month", "day", "sequence"]
--    applyRow row = zipWith ($) funcs row
--    funcs = map snd $ A.sortBy headerOrder columns
--readCSV :: String -> String -> Maybe (Array State)
readCSV :: Separator -> String -> Either Error (Array State)
--readCSV sep s = process <$> (toEither "no head" $ A.head lines') <*> (toEither "no tail" $ A.tail lines')
readCSV sep s = do
                  head <- (toEither "no head" $ A.head lines')
                  rows <- (toEither "no tail" $ A.tail lines')
                  process head rows
  where
    sep' = case sep of
      Comma -> ","
      Tab -> "\t"
    lines' = map (S.split sep') $ lines s
    lines  = S.split "\n"
    
type Error = String        
process :: Array String -> Array (Array String) -> Either Error (Array State)
process header rows = sequence $ map process' $ A.filter (not <<< A.null) rows
  where
    --process' row | (A.length row) < (A.length columns) = Left ("Row not have expected length " ++ (show $ A.length columns) ++ " found length " ++ (show $ A.length row) )
    process' :: Array String -> Either Error State
    process' row = do
     name <- Right $ row `at` "name"
     acc  <- Right $ row `at` "acc"
     country <- Right $ row `at` "country"
     year <- toField Int.fromString  "year" row
     host <- toField readHost "host" row
     serotype <-  toField readSerotype  "serotype" row
     let hostString = (row `at` "host")
     let segment = readSegment $ (row `at` "segement")
     let genotype = readGenotype $ (row `at` "genotype")
     let month = (Int.fromString (row `at` "month"))
     let day = Int.fromString (row `at` "day") 
     sequence' <-  Right $ row `at` "sequence"
     date <- toEither "bad date parse" $ Date.fromString ((show year) ++ "/" ++ (fromMaybe "6" $ show <$> month) ++ "/" ++ (fromMaybe "15" $ show <$> day))
     pure {   name : name
            , acc : acc
            , country : country
            , date : date
            , host : host
            , serotype : serotype
            , segment : segment
            , genotype : genotype
            , sequence : sequence'
            , checked : false
            , month : month
            , year : year
            , day : day
            , hostString : hostString
           }
      where
        at xs col = fromMaybe ("Bad column "  <> col <> " in header " <> (intercalate "," header ) <> "\n" <> "row: " <> (intercalate ","xs))  $ do
          i <- A.elemIndex col header
          xs A.!! i
        toField :: forall a. (String -> Maybe a) -> String -> Array String -> Either Error a
        toField f col row = toEither msg $ f x
          where
            msg = x ++ " is not a valid " ++ col
            x = row `at` col
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
              , td [className "date"]    [ text $ "Date:  " <> dateString state] 
              , td [className "country"] [ text $ "Country:  " <> state.country ] ]
          , tr []   [ td []  [ text $ "Host:  " <> show state.host ] 
                 ,  td  [] [ text $ "Serotype:  " <> show state.serotype ] 
                 ,  td  [] [ text $ "Segment:  "  <> (fromMaybe "n/a" $ show <$> state.segment)] 
                 ,  td  [] [ text $ "Genotype:  "  <> (fromMaybe "n/a" $ show <$> state.genotype)] ]]

update :: Action -> State -> State
update ToggleCheck state = state { checked = not state.checked }
update _ state = state 
dateString state = intercalate "/" [(fromMaybe "?" $ show <$> state.month), (fromMaybe "?" $ show <$> state.day), (show state.year)]
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
