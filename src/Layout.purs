module App.Layout where

import Prelude
import Unsafe.Coerce
import Data.Either
import Data.Maybe
import App.Form as Form
import App.Seq as Seq
import App.Routes (Route(Home, NotFound))
import App.Seq (Separator(Comma))
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (catchException)
import Data.Foreign (readInt)
import Data.HTTP.Method (Method(..))
import FileReader (readFile', readFileBlocking)
import Network.HTTP.Affjax (affjax, defaultRequest)
import Pux (EffModel, noEffects, mapEffects, mapState)
import Pux.Html (Html, div, h1, p, text, form, button, input, span)
import Pux.Html.Attributes (type_, value, name)
import Pux.Html.Events (FormEvent, onChange, onSubmit)

data Action
  = Child (Form.Action)
  | PageView Route
  | LoadFile (Either String (Array Seq.State))
  | DoChildAction Form.Action 


type State =
  { route :: Route
  , form :: Form.State }

init :: State
init =
  { route: NotFound
  , form: Form.init }

--safeReadAscii fp = catchException (const (liftEff $ pure "This is an error!" :: Eff (fs :: FS) String))  $ Node.readTextFile Encoding.ASCII fp
update :: forall e. Action -> State -> EffModel State Action (Form.AppEffects )
--update (PageView route) state = noEffects $ state { route = route } 
-- return $ LoadFile $ Seq.readCSV "," s]}

--csv must be placed in `static/` along with app.css
update (PageView route) state = { state : state
                                , effects : [do
                                               let fn = "https://gist.githubusercontent.com/averagehat/27baa334b0a3b2e90f691db6bbbdcab0/raw/064328b87eb1b6f1fa6d3fc9450a25514bf107c4/foo.csv"
                                                   
                                               --s <- liftEff $ safeReadAscii "./foo.csv"
                                               --s <- liftEff $ readFileBlocking fn 
                                               res <- affjax $ defaultRequest { url = "./foo.csv", method = Left GET }
                                               return $ LoadFile $ Seq.readCSV Comma res.response
                                              -- let x = res.response ++ "foo"
                                              -- return $ LoadFile $ Right Form.seqs
                                            ]}
update (LoadFile (Right recs))  state = noEffects $ state { form = state.form  { db = recs }}
update (DoChildAction (Form.RandomState state')) state = noEffects state { form = state' }
-- simply pass along the child From's actions 
update (Child action) state = mapEffects DoChildAction
          (mapState (\s -> state { form = s }) (Form.update action state.form))


view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Search for Dengue Sequences" ]
    , case state.route of
        Home -> map Child $ Form.view state.form
        _ -> map Child $ Form.view state.form
        --NotFound -> App.NotFound.view state
    ]
