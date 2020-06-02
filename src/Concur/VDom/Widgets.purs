module Concur.VDom.Widgets where

import Concur.Core (Widget)
import Concur.VDom (HTML)
import Concur.VDom.DOM as D
import Concur.VDom.Props as P
import Control.Bind (bind, discard)
import Effect.Class.Console (log)

-- | A Sample widget for testing
sample :: forall a. Widget HTML a
sample = do
  e <- D.node "button" [P.handle "click"] [D.text "Click me"]
  log "FIRED!!!"
  D.node_ "div" [] (D.text "Hello World!")
