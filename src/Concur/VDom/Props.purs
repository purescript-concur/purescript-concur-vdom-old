module Concur.VDom.Props where

import Concur.Core.Props (Props(..))
import Concur.VDom.Props.Internal as P
import Effect.Uncurried (mkEffectFn1)
import Web.Event.Event as DOM

type VDomProps a = Props P.Prop a

-- | Construct a custom prop handler
unsafeMkPropHandler :: DOM.EventType -> VDomProps DOM.Event
unsafeMkPropHandler s = Handler \f -> P.Handler s (mkEffectFn1 f)

-- | Construct a custom key value prop
unsafeMkProp :: forall a b. String -> a -> VDomProps b
unsafeMkProp s v = PrimProp (P.Property s (P.propValue v))
