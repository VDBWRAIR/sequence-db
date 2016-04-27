module App.Form where
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Generic
import Global (encodeURIComponent)
import App.Routes (Route)
import Prelude --(($), map, (<>), show, const, (<<<), (&&), (<=), (>=), (<$>), (==), Eq, not)
import Pux.Html (Html, text, form, button, input, span, ul, div, label, a, br, p, select, option)
import Pux.Html.Attributes (type_, value, name, download, href, checked, disabled)
import Pux.Html.Events (FormEvent, onChange, onSubmit, onClick, SelectionEvent, onSelect)
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import App.Seq as Seq
import App.Seq (Format(Fasta,CSV), Host(..), readFormat) 
import Data.Array (filter, nubBy, length)
import Data.Foldable (intercalate)
type Year = Int

type State = {    name      :: Maybe String
                 , acc      :: Maybe String
                 , minYear  :: Year 
                 , maxYear  :: Year
                 , country  :: Maybe String
                 , segment  :: Maybe Seq.Segment
                 , host     :: Maybe Seq.Host
                 , serotype :: Maybe Seq.Serotype
                 , result   :: Array Seq.State
                 , random   :: Boolean
                 , sampleSize :: Maybe Int
                 , format   :: Format
              }
type Acc = String
data Action =
   NameChange     FormEvent
 | MinYearChange  FormEvent
 | MaxYearChange  FormEvent
 | CountryChange  FormEvent
 | HostChange     FormEvent
 | SerotypeChange FormEvent
 | SegmentChange  FormEvent
 | SampleSizeChange  FormEvent
 | FormatChange SelectionEvent
 | RunQuery
 | DelteChecked
 | ToggleRandom
 | PageView Route
 | Child Seq.Acc Seq.Action
  
init :: State
init = { name: Nothing, country: Nothing
       , host: Nothing, serotype: Nothing
       , segment: Nothing
       , minYear : 0, maxYear : 3000
       , acc : Nothing, result : []
       , random : false, sampleSize : Nothing
       , format : Fasta}
-- In order to give Seq.State an Eq instance, it must be wrapped in NewType
update :: Action -> State -> State
update (RunQuery) state = state { result = nubBy Seq.stateEq $ state.result <> (query state) }
--update (RunState) state =   state { result = ((show state.minYear) <> (show state.maxYear)) } 
update (NameChange ev)    state = state { name =    Just ev.target.value }
update (CountryChange ev) state = state { country = Just ev.target.value }
update (MinYearChange ev) state = state { minYear = (unsafeCoerce ev.target.value) :: Int }
update (MaxYearChange ev) state = state { maxYear = (unsafeCoerce ev.target.value) :: Int }
update (HostChange ev)    state = state { host = Seq.readHost ev.target.value }
update (SerotypeChange ev)    state = state { serotype = Seq.readSerotype ev.target.value }
update (SegmentChange ev)    state = state { segment = Seq.readSegment ev.target.value }
update (SampleSizeChange ev)    state = state { sampleSize = Just (unsafeCoerce ev.target.value :: Int ) }
update DelteChecked     state = state { result = (filter (not <<< _.checked) state.result )}
update ToggleRandom     state = state { random = not state.random }
update (Child acc Seq.ToggleCheck) state = state { result = map f state.result }
  where f x = if (x.acc == acc) then (Seq.update Seq.ToggleCheck x) else x -- (x {checked = not x.checked} ) else x
update (FormatChange ev)    state = state { format = fromMaybe CSV $ readFormat ev.target.value  }

view :: State -> Html Action
view state = div []
  [form
  [ name "Search"
  , onSubmit (const RunQuery)
    ]
  [ label [] [ text "Name:"], input [type_ "text", value $ fromMaybe "" state.name,    onChange NameChange ] [] 
  , label [] [ text "Country:"], input [type_ "text", value $ fromMaybe "" state.country, onChange CountryChange ] [] , br [] []
  , label [] [ text "Host Species:"], select [value $ fromMaybe "Any" $ show <$> state.host, onChange HostChange ] (toOptions [Seq.Human, Seq.Mosquito])
  , label [] [ text "Segment (optional):"], select [value $ fromMaybe "Any" $ show <$> state.segment, onChange SegmentChange ] (toOptions Seq.segments)
  , label [] [ text "Serotype:"], select [value $ fromMaybe "Any" $ show <$> state.serotype, onChange SerotypeChange] (toOptions Seq.serotypes), br [] []
  , label [] [ text "Minimum Year"], input [type_ "text", value $ show state.minYear, onChange MinYearChange ] []
  , label [] [ text "Maximum Year"], input [type_ "text", value $ show state.maxYear, onChange MaxYearChange ] []
  , button [ type_ "submit" ] [ text "Search" ]
  --, ul [] $  map ((map Child) <<< Seq.view) state.result
  , ul [] $  map (\s -> map (Child s.acc) $ Seq.view s) state.result
    ]
  , button [ onClick (const DelteChecked)] [ text "Delete" ],
    p [] [text ("Results: " ++ (show $ length state.result))], br [] [] 
  , span [] [label [] [text "Random subset"], br [] []
           , input [type_ "checkbox", checked state.random, value "random" , onClick (const ToggleRandom)] []
           , label [] [text "Sample Size"]
           , input [disabled $ not state.random, type_ "text", value $ fromMaybe "" $ show <$> state.sampleSize, onChange SampleSizeChange] []
           , br [] 
    []
  , a [href ("data:text/plain;charset=utf-8," <> (encodeURIComponent $ toFormat state.format state.result)) ] [text "Download"]
  , label [] [text "     Format"]]
  , select [value $ show state.format, onChange FormatChange]
     [option [value "CSV"] [text "CSV"]
    , option [value "Fasta"] [text "Fasta"]]]

                                   
toOptions xs = [(option [value "Any"] [text "Any"])] <> (map (\x -> option [value $ show x] [ text $ show x])  xs)

toFormat :: Format ->  Array Seq.State  -> String
toFormat fmt xs = case fmt of
  Fasta -> intercalate "\n" $ map toFasta xs
  CSV   -> header <> "\n" <> (intercalate "\n" $ map toRow xs)
  where
    toFasta x = x.name <> "_" <> x.acc <> "\n" <> x.sequence
    header = "name,acc,year,segment"
    toRow x = intercalate "," [x.name, x.acc, (show x.year), (show x.segment)]

toCSV :: Array Seq.State -> String
toCSV xs = header <> "\n" <> (intercalate "\n" $ map toRow xs)
  where
    header = "name,acc,year,segment"
    toRow x = intercalate "," [x.name, x.acc, (show x.year), (show x.segment)]
    

query :: State -> Array Seq.State
query q = filter match seqs
  where
    match x = (q.acc ==? x.acc) && 
      (q.name ==? x.name) &&
      (x.year >= q.minYear && x.year <= q.maxYear)
      --(q.serotype ==? x.serotype) &&
    (==?) :: forall a. (Eq a) => Maybe a -> a -> Boolean
    (==?) a b = fromMaybe true ((== b) <$> a)

seqs  = [example, example2, example3]

example :: Seq.State
example =  {
       name     : "NameFoo"
     , acc      : "AccFoo"
     , year     : 1989
     , country  : "USA"
     , host     : Seq.Mosquito
     , serotype : Seq.DENV1
     , sequence : "ACTG"
     , segment  : Nothing
     , checked : false
       }

example2 :: Seq.State
example2 =  {
       name     : "NameToo"
     , acc      : "AccToo"
     , year     : 2012
     , country  : "Jamaica"
     , host     : Seq.Human
     , serotype : Seq.DENV2
     , sequence : "GGGGG"
     , segment  : Nothing
     , checked : false
       }
example3 =  {
       name     : "Influenza99"
     , acc      : "Acc"
     , year     : 1999
     , country  : "USA"
     , host     : Seq.Human
     , serotype : Seq.HN1
     , sequence : "GGGGG"
     , segment  : Just Seq.PB1
     , checked : false
       }
