module Concur.VDom.Types where

import Concur.Thunk.Internal (Thunk)
import Concur.VDom.Props.Internal as P
import Data.Tuple (Tuple)
import Halogen.VDom.Types (VDom)
import Unsafe.Coerce (unsafeCoerce)

newtype HTMLNode = HTMLNode (VDom (Array P.Prop) (Thunk HTMLNode))

unHTMLNode :: HTMLNode -> VDom (Array P.Prop) (Thunk HTMLNode)
unHTMLNode (HTMLNode x) = x

mkHTMLNode :: VDom (Array P.Prop) (Thunk HTMLNode) -> HTMLNode
mkHTMLNode = HTMLNode

type HTML
  = Array HTMLNode

mkHTML :: Array (VDom (Array P.Prop) (Thunk HTMLNode)) -> HTML
mkHTML x = unsafeCoerce x

unHTML :: HTML -> Array (VDom (Array P.Prop) (Thunk HTMLNode))
unHTML x = unsafeCoerce x

mkKeyedHTML :: Array (Tuple String (VDom (Array P.Prop) (Thunk HTMLNode))) -> Array (Tuple String HTMLNode)
mkKeyedHTML x = unsafeCoerce x

unKeyedHTML :: Array (Tuple String HTMLNode) -> Array (Tuple String (VDom (Array P.Prop) (Thunk HTMLNode)))
unKeyedHTML x = unsafeCoerce x
