module Concur.VDom.Widgets where

import Concur.Core (Widget)
import Concur.VDom.DOM as D
import Concur.VDom.Props as P
import Concur.VDom.Types (HTML)
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.MultiAlternative (orr)
import Data.Function (($))
import Data.Functor (void)
import Data.Monoid (mempty)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Class.Console (log)


-- | A Sample widget for testing
sample :: Widget HTML Unit
sample = do
  void $ D.node "button" [P.handle "click"] [D.text "HELLO TO me"]
  -- log "FIRED!!!"

{-
foreign import setTimeout :: Int -> Effect Unit -> Effect Unit

timeout :: Int -> Widget HTML Unit
timeout duration = asyncAction mempty \cb -> do
  setTimeout duration (cb unit)
  pure (pure unit)

testWidget :: forall a. Widget HTML a
testWidget = do
  result <- orr
       [ uithing
       , affAction
       ]
  D.text result
  where
    uithing = do
      _ <- D.node "button" [ P.handle "click" ] [ D.text "cancel" ]
      pure "CANCELLED"
    -- long taking action
    affAction = do
      timeout 5000
      log  "done"
      pure "TIMEOUT"
-}
