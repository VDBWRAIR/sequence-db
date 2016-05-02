module App.Form where 
import Control.Monad.Eff (Eff())
import DOM (DOM)
import FileReader (FS)
import  Control.Monad.Eff.Exception (EXCEPTION)
import Global (encodeURIComponent) --, readInt)
import Data.Int as Int
import Data.Set as Set
import Data.List.Lazy as List
import Data.Maybe.Unsafe (fromJust)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Control.Monad.Eff.Class (liftEff)
import Data.Either.Unsafe (fromRight)

--import Data.Unfoldable (replicateA)
import Control.Monad.Eff.Random as Rand
import App.Routes (Route)
import Data.Traversable (sequence) 
import Prelude --(($), map, (<>), show, const, (<<<), (&&), (<=), (>=), (<$>), (==), Eq, not)
import Pux (EffModel, noEffects)
import Pux.Html (Html, text, form, button, input, span, ul, div, label, a, br, p, select, option, font)
import Pux.Html as H
import Pux.Html.Attributes as At
import Data.StrMap as M
import Pux.Html.Attributes (type_, value, name, download, href, checked, disabled, color, size)
import Pux.Html.Events (FormEvent, onChange, onSubmit, onClick, SelectionEvent, onSelect)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe, maybe)
import App.Seq as Seq
import App.Seq (Format(Fasta,CSV), Host(..), readFormat) 
import Data.Array (filter, nubBy, length, (:), (!!))
import Data.Array as A
import Data.String as S
import Data.Foldable (intercalate, foldr)
import Data.Either (Either(Left,Right))
import Data.Date as Date  -- https://github.com/purescript/purescript-datetime


type Year = Int
type Error = String
type State = {    name      :: Maybe String
                 , acc      :: Maybe String
                 , minYear  :: Year 
                 , maxYear  :: Year
                 , minDate  :: Maybe Date.Date
                 , maxDate  :: Maybe Date.Date
                 , country  :: Maybe String
                 , segment  :: Maybe Seq.Segment
                 , host     :: Maybe Seq.Host
                 , hostString :: Maybe String
                 , serotype :: Maybe Seq.Serotype
                 , result   :: Array Seq.State
                 , genotype :: Maybe Seq.Genotype
                 , random   :: Boolean
                 , sampleSize :: Maybe Int
                 , format   :: Format
                 , errors :: M.StrMap Error
                 , db :: Array Seq.State
                 --, db :: Array Seq.State
              }
type Acc = String
data Action =
   NameChange     FormEvent
 | MinYearChange  FormEvent
 | MaxYearChange  FormEvent
 | MinDateChange  FormEvent
 | MaxDateChange  FormEvent
 | CountryChange  FormEvent
 | HostChange     FormEvent
 | HostStringChange     FormEvent
 | SerotypeChange FormEvent
 | SegmentChange  FormEvent
 | GenotypeChange  FormEvent
 | SampleSizeChange  FormEvent
 | FormatChange SelectionEvent
 | RandomState State
 | ToggleAllChecked
 | RandomClicked
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
       , format : Fasta, genotype : Nothing
       , minDate : Nothing, maxDate : Nothing
       , errors : M.empty
       , db : fromRightOrError $ Seq.readCSV "," csv
       , hostString : Nothing }
-- In order to give Seq.State an Eq instance, it must be wrapped in NewType
       

type AppEffects = (fs :: FS, random :: Rand.RANDOM, dom :: DOM )

strToMaybe xs = if (S.null $ S.trim xs) then Nothing else Just xs

update :: forall e. Action -> State -> EffModel State Action (AppEffects )
update (RunQuery) state             = noEffects $ state { result = nubBy Seq.stateEq $ state.result <> (query state) }
update (NameChange ev)    state     = noEffects $ state { name =    strToMaybe ev.target.value }
update (CountryChange ev) state     = noEffects $ state { country = strToMaybe ev.target.value }
update (HostChange ev)    state     = noEffects $ state { host = Seq.readHost ev.target.value }
update (HostStringChange ev)    state     = noEffects $ state { hostString = strToMaybe ev.target.value } 
update (SerotypeChange ev) state    = noEffects $ state { serotype = Seq.readSerotype ev.target.value }
update (GenotypeChange ev) state    = noEffects $ state { genotype = Seq.readGenotype ev.target.value }
update (SegmentChange ev)  state    = noEffects $ state { segment = Seq.readSegment ev.target.value }
update (MinYearChange ev) state     = noEffects $ strInt "Min Year" state ev (\x -> state { minYear = x }) id 0
update (MaxYearChange ev) state     = noEffects $ strInt "Max Year" state ev (\x -> state { maxYear = x }) id 0
update (SampleSizeChange ev) state  = noEffects $ strInt "Sample Size" state ev (\x -> state { sampleSize = x }) Just Nothing
update DelteChecked     state       = noEffects $ state { result = (filter (not <<< _.checked) state.result )}
update ToggleRandom     state       = noEffects $ state { random = not state.random }
update (FormatChange ev)  state     = noEffects $ state { format = fromMaybe CSV $ readFormat ev.target.value  }
update (MinDateChange ev) state     = noEffects $ state { minDate = Date.fromString ev.target.value  }
update (MaxDateChange ev) state     = noEffects $ state { maxDate = Date.fromString ev.target.value  }
update (RandomState state') state   = noEffects state'
update ToggleAllChecked     state    = noEffects $ state { result = map (\s -> s { checked = not s.checked} ) state.result } 
update RandomClicked     state      = { state : state, effects :  [do
                                                                      res <- liftEff $ handleRandom state
                                                                      return $ RandomState res] }
update (Child acc Seq.ToggleCheck) state = noEffects $ state { result = map f state.result }
  where f x = if (x.acc == acc) then (Seq.update Seq.ToggleCheck x) else x 

handleRandom :: forall e. State -> Eff (random :: Rand.RANDOM | e) State
handleRandom state = if ((fromMaybe 2147483647 state.sampleSize) >= (length state.result)) then
                        return $ state { errors = M.insert "Sample Size" "Sample size must be less than total result count." state.errors }
                      else do
                            res <- subsample (fromJust state.sampleSize) state.result
                            return $ state { errors = M.delete "Sample Size" state.errors,
                                result = res }
                              
--shuffleArray :: forall f a. (Monad f) => Array a -> GenT f (Array a)
shuffleArray = shuffle0 [] where
    shuffle0 acc [] = pure $ acc
    shuffle0 acc xs = do i <- Rand.randomInt 0 (A.length xs - 1)
                         let acc' = acc <> (maybe [] A.singleton (xs A.!! i))
                             xs' = fromMaybe xs $ A.deleteAt i xs
                         shuffle0 acc' xs'

subsample :: forall a e. Int -> Array a -> Eff (random :: Rand.RANDOM | e) (Array a)
subsample n xs = A.take n <$> shuffleArray xs 

strInt k state ev f g z = if (ev.target.value == "") then (f z) else withError (f <<< g) k  (Int.fromString ev.target.value) state
  where
    withError :: forall a. (a -> State) -> String -> Maybe a -> State -> State
    withError f k Nothing state = state  { errors = (M.insert k (k <> " must be a number.") state.errors ) }
    withError f k (Just x) state = (f x) { errors = (M.delete k state.errors) }

makeInput :: forall a. String -> (State -> Maybe String) -> (Seq.State -> String) ->
             (FormEvent -> Action) -> State -> (Html Action)
makeInput name stateAttr dbAttr action state = label [] [ text (name ++ ":")
                                , input [ type_ "text"
                                    , value $ fromMaybe "" (stateAttr state)
                                    , onChange action
                                    , At.list name ] []
                                , autoCompleteList name $ map dbAttr state.db ]
view :: State -> Html Action
view state = div []
  [form
  [ name "Search"
  , onSubmit (const RunQuery)
    ]
  [makeInput "Name" _.name _.name NameChange state
 , makeInput "Country" _.country _.country CountryChange state
  , br [] [] 
 , makeInput "Host" _.hostString _.hostString HostStringChange state
  , label [] [ text "Host Species:"], select [value $ fromMaybe "Any" $ show <$> state.host, onChange HostChange ] (toOptions [Seq.Human, Seq.Mosquito])
  , label [] [ text "Segment (optional):"], select [value $ fromMaybe "Any" $ show <$> state.segment, onChange SegmentChange ] (toOptions Seq.segments)
  , label [] [ text "Serotype:"], select [value $ fromMaybe "Any" $ show <$> state.serotype, onChange SerotypeChange] (toOptions Seq.serotypes), br [] []
  , label [] [ text "Genotype:"], select [value $ fromMaybe "Any" $ show <$> state.genotype, onChange GenotypeChange] (toOptions Seq.genotypes), br [] []
  , label [] [ text "Minimum Year"], input [type_ "text", value $ show state.minYear, onChange MinYearChange ] []
  , label [] [ text "Maximum Year"], input [type_ "text", value $ show state.maxYear, onChange MaxYearChange ] []
  , label [] [text "Min date"], input [type_ "date", onChange MinDateChange ] []
  , label [] [text "Max date"], input [type_ "date", onChange MaxDateChange ] []
  , button [ type_ "submit" ] [ text "Search" ]
  , br [] [] 
  , input [type_ "checkbox",value "random" , onClick (const ToggleAllChecked)] [] 
  , ul [] $  map (\s -> map (Child s.acc) $ Seq.view s) state.result
    ]
  , button [ onClick (const DelteChecked)] [ text "Delete" ],
    p [] [text ("Results: " ++ (show $ length state.result))], br [] [] 
  , span [] [label [] [text "Random subset"], br [] []
           , input [type_ "checkbox", checked state.random, value "random" , onClick (const ToggleRandom)] []
           , label [] [text "Sample Size"]
           , input [disabled $ not state.random, type_ "text", value $ fromMaybe "" $ show <$> state.sampleSize, onChange SampleSizeChange] []
           , button [ onClick (const RandomClicked)] [ text "Subset" ]
           , br [] []
  , a [href ("data:text/plain;charset=utf-8," <> (encodeURIComponent $ toFormat state.format state.result)) ] [text "Download"]
  , label [] [text "     Format"]]
  , select [value $ show state.format, onChange FormatChange]
     [option [value "CSV"] [text "CSV"]
    , option [value "Fasta"] [text "Fasta"]], br [] []
    , div []  $ toArray $ map (\x -> font [size 3, color "red"] [text $ x, br [] [] ]) (M.values state.errors) ]
             
autoCompleteList id' xs = H.datalist [At.id_ id'] $ toArray $ map (\x -> H.option [At.label x, At.value x] []) $ A.nub xs
                  
toArray xs = foldr (:) [] xs 
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
    toRow x = intercalate "," [x.name, x.acc, (show x.year), (fromMaybe "" $ show <$> x.segment)]
    

query :: State -> Array Seq.State
query q = filter match q.db
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
     , genotype : Nothing
     , month : Nothing
     , day : Nothing
     , hostString : "Foohhost"
     , date : fromJust $ Date.fromString "08/12/12"
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
     , genotype : Nothing
     , month : Nothing
     , day : Nothing
     , hostString : "Foohhost"
     , date : fromJust $ Date.fromString "08/12/12"
       }
            
example3 :: Seq.State
example3 =  {
  name     : "Influenza99"
     , acc      : "Acc"
     , year     : 1999
     , month : Nothing
     , day : Nothing
     , date : fromJust  $ Date.fromString "08/12/12"
     , country  : "USA"
     , host     : Seq.Human
     , hostString : "Foohhost"
     , serotype : Seq.HN1
     , sequence : "GGGGG"
     , segment  : Just Seq.PB1
     , checked : false
     , genotype : Nothing
       }
            
  -- [ option [label x, value x] []]
fromRightOrError (Right s) = s
fromRightOrError (Left s) = unsafeThrow s
-- TODO: non-normal Host is not okay apparently
csv = "genotype,segment,name,acc,year,month,day,country,host,serotype,sequence\n" ++ 
",,11/1666,KR922405,2011,,,Thailand,Human,DENV4,ATGAACCAACGAAAGAAGGTGG\n" ++ 
",,Br246RR/10,JN983813,2010,9,8,Brazil,Human,DENV4,ATGAACCAACGAAAAAAGGT\n" ++ 
",,D4/Pakistan/150/2009,KF041260,2009,,,Pakistan,Human,DENV4,ATGAACCAACG"
