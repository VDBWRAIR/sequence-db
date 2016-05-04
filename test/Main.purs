module Test.Main where
import Prelude
import Data.Either
import Data.Maybe
import Data.Generic
import CSV
import App.Seq as Seq
import Data.Array as A
import Data.Eulalie.Parser as P
import Test.QuickCheck.Gen as Gen
import Test.Unit.Assert as Assert
import App.Seq (serotypes, Serotype, Segment(PB1, M1))
import Data.Eulalie.Result (ParseResult(Success))
import Data.Eulalie.Stream (stream)
import Test.QuickCheck (Result, (===))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.Unit (test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)
import Type.Proxy (Proxy(..))


newtype SingleEnum = SingleEnum { foo :: Foo } 
derive instance genericSingleEnum :: Generic SingleEnum
--instance encodeSingleEnum :: Encode SingleEnum
--  encode = gEncode
--instance decodeSingleEnum :: Decode SingleEnum
--  decode = gDecode --
--newtype Primitives { int :: Int
--                   , str :: String }
--                   
--derive instance genericPrimitives :: Generic Primitives
--instance encodePrimitives :: Encode Primitives
--  encode = gEncode 
--instance decodePrimitives :: Decode Primitives
--  decode = gDecode
--
--decodeEncodeEquals :: (Encode a, Decode a) => a -> Result
--decodeEncodeEquals x = decode $ encode x == x

theCommutativeProperty :: Int -> Int -> Result
theCommutativeProperty a b = (a + b) === (b + a)

toFromParseSerotype :: Serotype -> Result
toFromParseSerotype a = Just a === run serotype $ show a

main = runTest do
--  test "enum shows encode" do
--    encode $ SingleEnum { foo : Foo } `equal` "foo\n" <> "Foo"
  test "the commutative property" do
    quickCheck theCommutativeProperty
  test "read" do
    (Just Foo)  `equal` (Just Foo)
    (Right Foo) `equal` (parseFoo "Foo")
    (Right Bar) `equal` (parseFoo "Bar")
    assert "oughta be left" $ isLeft $ parseFoo "Baz"
    (Just PB1) `equal` run segment "PB1"
    (Just M1) `equal` run segment "M1"
    quickCheck toFromParseSerotype
    
run p s = toMaybe $ P.parse p (stream s)

    
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
