module Concur.VDom.Run where

import Prelude

import Concur.Core.Discharge (discharge, dischargePartialEffect)
import Concur.Core.Types (Widget)
import Concur.Thunk.Internal (Thunk, buildThunk)
import Concur.VDom.DOM (nodeBuilder)
import Concur.VDom.Props.Internal (buildProp)
import Concur.VDom.Props.Internal as P
import Concur.VDom.Types (HTML, HTMLNode, unHTMLNode)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, error, makeAff, nonCanceler, runAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throwException)
import Effect.Ref as Ref
import Effect.Uncurried as EFn
import Halogen.VDom as V
import Web.DOM.Document (Document) as DOM
import Web.DOM.Node (Node, appendChild) as DOM
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (HTMLElement, window)
import Web.HTML.Event.EventTypes as ET
import Web.HTML.HTMLDocument (readyState)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLDocument.ReadyState (ReadyState(..))
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

-- | Run a Concur Widget inside a dom element with the specified id
runWidgetInDom :: forall a. String -> Widget HTML a -> Effect Unit
runWidgetInDom elemId = runWidgetInSelector ("#" <> elemId)

-- | Run a Concur Widget inside a dom element with the specified selector
runWidgetInSelector :: forall a. String -> Widget HTML a -> Effect Unit
runWidgetInSelector elemId = renderWidgetInto (QuerySelector elemId)

-- | Run a Concur Widget inside a dom element with the specified QuerySelector
renderWidgetInto :: forall a. QuerySelector -> Widget HTML a -> Effect Unit
renderWidgetInto query w = runAffX do
  awaitLoad
  doc <- liftEffect $ Window.document =<< window
  mroot <- selectElement query
  case mroot of
    Nothing -> pure unit
    Just root -> void $ liftEffect $ renderComponent (mkSpec (HTMLDocument.toDocument doc)) (HTMLElement.toNode root) w

-- Spec
type WidgetSpec = V.VDomSpec (Array P.Prop) (Thunk HTMLNode)
mkSpec :: DOM.Document -> WidgetSpec
mkSpec document = V.VDomSpec
  { buildWidget: buildThunk unHTMLNode
  , buildAttributes: buildProp
  , document
  }

-- Render a component onto a DOM element
renderComponent :: forall a . WidgetSpec -> DOM.Node -> Widget HTML a -> Effect Unit
renderComponent spec parent winit = do
  Tuple winit' v <- dischargePartialEffect winit
  initMachine <- EFn.runEffectFn1 (V.buildVDom spec) (render v)
  _ <- DOM.appendChild (V.extract initMachine) parent
  ref <- Ref.new initMachine
  handler ref (Right winit')
  where
    render v = unHTMLNode (nodeBuilder "div" [] v)
    handler ref = go
      where
        go (Left err) = log ("FAILED! " <> show err)
        go (Right r) = do
          machine <- Ref.read ref
          v <- discharge (handler ref) r
          res <- EFn.runEffectFn2 V.step machine (render v)
          Ref.write res ref

-- Attribution - Everything below was taken from Halogen.Aff.Utils
-- https://github.com/purescript-halogen/purescript-halogen/blob/master/src/Halogen/Aff/Util.purs

-- | Waits for the document to load.
awaitLoad :: Aff Unit
awaitLoad = makeAff \callback -> do
  rs <- readyState =<< Window.document =<< window
  case rs of
    Loading -> do
      et <- Window.toEventTarget <$> window
      listener <- eventListener (\_ -> callback (Right unit))
      addEventListener ET.domcontentloaded listener false et
      pure $ effectCanceler (removeEventListener ET.domcontentloaded listener false et)
    _ -> do
      callback (Right unit)
      pure nonCanceler

-- | Waits for the document to load and then finds the `body` element.
awaitBody :: Aff HTMLElement
awaitBody = do
  awaitLoad
  body <- selectElement (QuerySelector "body")
  maybe (throwError (error "Could not find body")) pure body

-- | Tries to find an element in the document.
selectElement :: QuerySelector -> Aff (Maybe HTMLElement)
selectElement query = do
  mel <- liftEffect $
    ((querySelector query <<< HTMLDocument.toParentNode <=< Window.document) =<< window)
  pure $ HTMLElement.fromElement =<< mel

-- | Runs an `Aff` value of the type commonly used by Halogen components. Any
-- | unhandled errors will be re-thrown as exceptions.
runAffX :: forall x. Aff x -> Effect Unit
runAffX = runAff_ (either throwException (const (pure unit)))
