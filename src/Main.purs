module Main where

import App.Routes (match)
import App.Layout (Action(PageView), State, view, update)
import Control.Bind ((=<<))
import Control.Monad.Eff (Eff)
import Prelude (bind, return, ($))
import Pux (App, Config, CoreEffects, fromSimple, renderToDOM)
import Pux.Router (sampleUrl)
import Signal ((~>))
import App.Form (AppEffects)
  -- AppEffects must be defined as a closed record with DOM and RANDOM

-- | Entry point for the browser.--
main :: State -> Eff (CoreEffects AppEffects) (App State Action)
main state = do
  app <- Pux.start =<< config state
  renderToDOM "#app" app.html
  -- | Used by hot-reloading code in support/index.js
  return app 
config :: State ->
          Eff (CoreEffects (AppEffects ))
            (Config State Action AppEffects)
config state = do
  -- | Create a signal of URL changes.
  urlSignal <- sampleUrl

  -- | Map a signal of URL changes to PageView actions.
  let routeSignal = urlSignal ~> \r -> PageView (match r)
  return { initialState : state
           , update : update
           , view : view
           , inputs : [routeSignal]}

-- | Entry point for the browser with pux-devtool injected.
debug :: State -> Eff (CoreEffects AppEffects) (App State (Pux.Devtool.Action Action))
debug state = do
  app <- Pux.Devtool.start =<< config state
  renderToDOM "#app" app.html
  -- | Used by hot-reloading code in support/index.js
  return app 
