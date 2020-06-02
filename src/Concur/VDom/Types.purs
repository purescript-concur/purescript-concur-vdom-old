module Concur.VDom.Types where

import Concur.VDom.Props.Internal as P
import Data.Void (Void)
import Halogen.VDom.Types (VDom)

-- Void implies we don't support some embedded widget type
-- TODO: Investigate w == Thunk MyVDOM Void where
--   newtype MyVDom a = MyVDom (VDom (Array P.Prop) (Thunk MyVDom a))
-- newtype HTMLNode = HTMLNode (VDom (Array P.Prop) (Thunk HTMLNode Void))
type HTMLNode = VDom (Array P.Prop) Void

type HTML
  = Array HTMLNode
