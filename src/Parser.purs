module Parser where
import Data.Array as A
import Data.String as S
import Data.Generic (class Generic, gShow, gEq)
import Data.List (List, takeWhile, null, take, drop)
import Data.Maybe (fromMaybe, Maybe(Just))
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (unfoldr)
import Prelude (not, class Show, class Eq, (<<<), ($))
import Data.List.WordsLines (lines)
import App.Layout (safeReadAscii)

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



                    
newtype Entry = Entry {
       name     :: String
     , acc      :: String
     , country  :: String 
     , serotype :: Serotype
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
type Seq = forall e. ({ id :: String , sequence :: String } | e)

splitAt :: forall t12. Int -> List t12 -> Tuple (List t12) (List t12)
splitAt n xs = Tuple (take n xs) (drop n xs)

chunksOf :: forall t512. Int -> List t512 -> List (List t512)
chunksOf n = takeWhile (not <<< null) <<< unfoldr (Just <<< splitAt n)

derive instance genericRow :: Generic Row
instance showRow :: Show Row where
  show = undot <<< gShow
instance eqRow :: Eq Row where
  eq = gEq

undot :: String -> String    
undot s = fromMaybe "" $ A.last $ S.split "." s 

pairs = chunksOf 2
s = "Month/Day/Year"
readFasta fp = (map f <<< pairs) <$> safeReadAscii fp
  where f (Tuple a b) = {id: a, sequence: b}
{ segment :: Segment
, fasta :: List Seq
, acc :: String}        
makeEntry :: Segment -> String -> List Seq -> Row -> Entry
makeEntry seg acc fasta row = do
  entry <- find (_.id == row.name) fasta
  let seq = entry.sequence 
  let month = (monthFromEnum <<< month) <$> row.date
  let day = (( \(DayOfMonth x) -> x) <<< dayOfMonth) <$> row.date
    -- get date from row
pure $ Entry {name: row.name, acc: acc, country: row.country, genomeNumber: row.genomeNumber
            , segment: Just segment, genotype: row.genotype, sequence: seq
            , hostString: row.host, month: month, day: day, year: row.year}



