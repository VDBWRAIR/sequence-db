module App.Layout where
import App.Form as Form
import App.Routes (Route(Home, NotFound))
import Prelude --(($), map, (<>), show, const, pure)
import Pux.Html (Html, div, h1, p, text, form, button, input, span)
import Pux.Html.Attributes (type_, value, name)
import Pux.Html.Events (FormEvent, onChange, onSubmit)
import Data.Foreign (readInt)
import Pux (EffModel, noEffects,  mapEffects, mapState)
import Unsafe.Coerce
import Data.Either
import Data.Maybe
import FileReader (readFileBlocking)
import App.Seq as Seq
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff.Class (liftAff)


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


update :: forall e. Action -> State -> EffModel State Action (Form.AppEffects )
--update (PageView route) state = noEffects $ state { route = route } 
update (PageView route) state = { state : state
                                , effects : [do
                                               s <- liftEff $ readFileBlocking "foo.csv"
                                               return $ LoadFile $ Seq.readCSV "," s]}
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
