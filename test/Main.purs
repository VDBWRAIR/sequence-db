module Test.Main where
import Prelude
import Data.Either
import Data.Maybe
import Data.Generic
import CSV
import Data.CSVGeneric
import App.Seq
import App.Seq as Seq
import Data.Array as A
import Data.Eulalie.Parser as P
import Data.String as S
import Test.QuickCheck.Gen as Gen
import Test.Unit.Assert as Assert
import App.Seq (serotypes, Serotype, Segment(PB1, M1))
import Data.Either.Unsafe (fromRight)
import Data.Eulalie.Result (ParseResult(Success, Error))
import Data.Eulalie.Result (ParseResult(Success))
import Data.Eulalie.Stream (stream)
import Test.QuickCheck (Result, (===))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.Unit (test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)
import Type.Proxy (Proxy(..))

theCommutativeProperty :: Int -> Int -> Result
theCommutativeProperty a b = (a + b) === (b + a)

main = runTest do
  test "the commutative property" do
    quickCheck theCommutativeProperty
  test "read" do
    -- no proper error if the headers don't match the type
    let header = S.split " " "name acc country serotype segment genotype sequence hostString month year day"
    let p = (fullParse header) (Proxy :: Proxy Entry)
    --(A.length $ S.split "," s) `equal` A.length header
    --(S.split "," s) `equal` []
    let res = P.parse p $ stream s
    -- this test gets truncated
    let expected = Entry {name : "harold", acc: "anacc", country: "USA", serotype: DENV1, segment: Just PB1
                                  , genotype: Just Genotype1, sequence: "AAA", hostString: "hostString"
                                  , month: Just 2, year: 3, day: Just 12}
    header `equal` fs
    
    (fromRight $ getSuccess res) `equal` expected
getSuccess (Success x) = x.value
fs = [ "name" , "acc" , "country" , "serotype" , "segment" , "genotype" , "sequence" , "hostString" , "month" , "year" , "day"] 
s = "harold,anacc,USA," <> show DENV1 <> "," <> show PB1 <> "," <> show Genotype1 <> ",AAA,hostString,2,3,12"
showResult (Success r) = show r.value
showResult (Error e) = "Expected one of:" <> show e.expected <> " at " <> show e.input    

    
--    (Foo) `equal` (readFoo "Foo")
--    (Bar) `equal` (readFoo "Bar")
--    dengueExampleHead 
--
--dengueExampleHead = (A.head <$> (Seq.readCSV Seq.Comma csv)) `equal` expected
--  where 
--     expected = Right $ Just { name : "foo", acc : "bar",  year : 1989
--               , month : (Just 8), day : (Just 1), host : Just "Moss"
--               , segment : (Just "n/a"), sequence : "ATCG", checked : true }
--     csv = "genotype,segment,name,acc,year,month,day,country,host,serotype,sequence\n" ++ 
--           ",,foo,bar,1989,8,1,USA,Moss,n/a,ATCG\n" ++ 
--           ",,11/1666,KR922405,2011,,,Thailand,Human,DENV4,ATGAACCAACGAAAGAAGGTGG\n" ++ 
--           ",,Br246RR/10,JN983813,2010,9,8,Brazil,Human,DENV4,ATGAACCAACGAAAAAAGGT\n" ++ 
--           ",,D4/Pakistan/150/2009,KF041260,2009,,,Pakistan,Human,DENV4,ATGAACCAACG"
