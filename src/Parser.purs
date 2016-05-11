module Parser where
import Data.Array as A
import Data.List as L
import Data.String as S
import App.Layout (safeReadAscii)
import App.Seq (Genotype, Segment, Serotype)
import Control.Bind (join)
import Data.Date (fromString, DayOfMonth(DayOfMonth))
import Data.Date.UTC (month, dayOfMonth)
import Data.Enum (fromEnum)
import Data.Foldable (find)
import Data.Generic (gEq, gShow, class Generic)
import Data.List ((:), List(Nil, Cons), null, takeWhile)
import Data.List.WordsLines (lines)
import Data.Maybe (fromMaybe, Maybe(Just))
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (unfoldr)
import Prelude (Unit, pure, (==), map, not, (<<<), class Eq, class Show, (<$>), ($), bind)
newtype Row = Row {
                  name :: String
                , year :: Int
                , country :: String
                , genomeNumber :: Maybe String
                , genotype :: Maybe Genotype
                , host :: String
                , date :: Maybe String
                , continent :: Maybe String
                , type_ :: Maybe String
                , subtype :: Maybe String
                , serotype :: Maybe Serotype
                , disease :: Maybe String
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
              
newtype Entry = Entry {
       name     :: String
     , acc      :: String
     , country  :: String 
     , serotype :: Maybe Serotype
     , segment  :: Maybe Segment
     , genotype :: Maybe Genotype
     , genomeNumber :: Maybe String
     , type_ :: Maybe String
     , subtype :: Maybe String
     , disease :: Maybe String
     , sequence :: String
     , hostString :: String
     , month :: Maybe Int
     , year :: Int
     , day :: Maybe Int
      
     --, date     :: Date.Date
}

splitAt :: forall t12. Int -> List t12 -> Tuple (List t12) (List t12)
splitAt n xs = Tuple (L.take n xs) (L.drop n xs)

chunksOf :: forall t512. Int -> List t512 -> List (List t512)
chunksOf n = L.takeWhile (not <<< L.null) <<< unfoldr (Just <<< splitAt n)

derive instance genericRow :: Generic Row
instance showRow :: Show Row where
  show = undot <<< gShow
instance eqRow :: Eq Row where
  eq = gEq

undot :: String -> String    
undot s = fromMaybe "" $ A.last $ S.split "." s 

pairs = chunksOf 2

readFasta fp = ((map f) <<< pairs <<< lines) <$> safeReadAscii fp
  where f (Cons a (Cons b Nil)) = {id: a, sequence: b}

--type Seq = forall r. Object (id :: String , sequence :: String | r) 
type Seq r = {id :: String , sequence :: String | r}
type Empty = {foo :: String}
makeEntry :: forall r. Segment -> String -> List (Seq r)  -> Row -> Maybe Entry
makeEntry seg acc fasta (Row row) = do
  --entry <- find (( == row.name) <<< _.id) fasta
  entry <- find (\x -> x.id == row.name) fasta 
  let seq = entry.sequence
  let date = join $ fromString <$> row.date
  let month' = (fromEnum <<< month) <$> date
  let day = (( \(DayOfMonth x) -> x) <<< dayOfMonth) <$> date
    -- get date from row
  pure $ Entry {name: row.name, acc: acc, country: row.country, genomeNumber: row.genomeNumber
            , segment: Just seg, genotype: row.genotype, sequence: seq
            , hostString: row.host, month: month', day: day, year: row.year
            , serotype: row.serotype, type_: row.type_, subtype: row.subtype
            , disease: row.disease}

--do
--  fasta <- readFasta 
--  segments 
--
--                HA     :: String
--                MP     :: String
--                NA     :: String
--                NP     :: String
--                NS     :: String
--                PA     :: String
--                PB1     :: String
--                PB2     :: String
