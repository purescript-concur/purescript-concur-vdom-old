module Concur.VDom.Props where

import Concur.Core.Props (Props(..))
import Concur.VDom.Props.Internal as P
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect.Uncurried (mkEffectFn1)
import Web.Event.Event as DOM

type VDomProps a = Props P.Prop a

-- | Construct a custom prop handler
handle :: String -> VDomProps DOM.Event
handle s = Handler \f -> P.Handler (wrap s) (mkEffectFn1 f)

-- | Construct a custom key value prop
prop :: forall a b. String -> a -> VDomProps b
prop s v = PrimProp (P.Property s (P.propValue v))

-- | Construct a custom key value attribute
attr :: forall a. String -> String -> VDomProps a
attr s v = PrimProp (P.Attribute Nothing s v)
