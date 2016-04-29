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


data Action
  = Child (Form.Action)
  | PageView Route
  | NewTodoAction Form.Action  

type State =
  { route :: Route
  , count :: Form.State }

init :: State
init =
  { route: NotFound
  , count: Form.init }

--update :: Action -> State -> Form.RandomState

update :: forall e. Action -> State -> EffModel State Action (Form.AppEffects )
update (PageView route) state = noEffects $ state { route = route }
--update (Child action)   state = state { count = Form.update action state.count }

update (NewTodoAction _) state = noEffects state
update (Child action) state =
    mapEffects NewTodoAction
        (mapState (\s -> state { count = s }) (Form.update action state.count))
--update (Child action) state = do
--                                new <- Form.update action state.count
--                                return $ { state : state  { count = new }
--                                         , effects : [] }

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Search for sequences" ]
    , p [] [ text "Change src/Layout.purs and watch me hot-reload." ]
    , case state.route of
        Home -> map Child $ Form.view state.count
        NotFound -> App.NotFound.view state
    ]
