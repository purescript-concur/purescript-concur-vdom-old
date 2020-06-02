module Main where

import Concur.VDom (runWidgetInDom)
import Concur.VDom.Widgets (sample)
import Data.Unit (Unit)
import Effect (Effect)

main :: Effect Unit
main = runWidgetInDom "root" sample
