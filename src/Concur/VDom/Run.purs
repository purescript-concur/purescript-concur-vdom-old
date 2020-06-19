module Concur.VDom.Run where

import Prelude

import Concur.Core.Discharge (discharge)
import Concur.Core.Event (Observer(..), effMap, observe)
import Concur.Core.Types (Widget)
import Concur.Thunk.Internal (Thunk, buildThunk)
import Concur.VDom.DOM (nodeBuilder)
import Concur.VDom.Props.Internal (buildProp)
import Concur.VDom.Props.Internal as P
import Concur.VDom.Types (HTML, HTMLNode(..), unHTMLNode)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (error)
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
renderWidgetInto query w = runObserver awaitLoad \_ -> do
  doc <- Window.document =<< window
  mroot <- selectElement query
  case mroot of
    Nothing -> pure unit
    Just root -> void $ renderComponent (mkSpec (HTMLDocument.toDocument doc)) (HTMLElement.toNode root) w

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
  -- Tuple winit' v <- dischargePartialEffect winit
  initMachine <- EFn.runEffectFn1 (V.buildVDom spec) (emptyMessage "LOADING...")
  _ <- DOM.appendChild (V.extract initMachine) parent
  ref <- Ref.new initMachine
  handler ref (Right winit)
  where
    -- If I use `mempty` or `text` initially then the entire machine fails
    -- This seems like a halogen-vdom bug
    emptyMessage s = unHTMLNode (nodeBuilder "div" [] [HTMLNode (V.Text s)])
    render Nothing = emptyMessage "NOTHING!!"
    render (Just arr) = unHTMLNode (nodeBuilder "div" [] arr)
    handler ref = go
      where
        go (Left err) = log ("FAILED! " <> show err)
        go (Right r) = do
          machine <- Ref.read ref
          mv <- discharge go r
          case mv of
            Nothing -> pure unit
            Just v -> do
              res <- EFn.runEffectFn2 V.step machine (render (Just v))
              Ref.write res ref

-- Attribution - Everything below was taken from Halogen.Aff.Utils
-- https://github.com/purescript-halogen/purescript-halogen/blob/master/src/Halogen/Aff/Util.purs

-- | Waits for the document to load.
awaitLoad :: Observer Unit
awaitLoad = Observer \callback -> do
  rs <- readyState =<< Window.document =<< window
  case rs of
    Loading -> do
      et <- Window.toEventTarget <$> window
      listener <- eventListener (\_ -> callback unit)
      addEventListener ET.domcontentloaded listener false et
      pure $ removeEventListener ET.domcontentloaded listener false et
    _ -> do
      callback unit
      pure (pure unit)

-- | Waits for the document to load and then finds the `body` element.
awaitBody :: Observer HTMLElement
awaitBody = effMap awaitLoad \_ -> do
  body <- selectElement (QuerySelector "body")
  maybe (throwError (error "Could not find body")) pure body

-- | Tries to find an element in the document.
selectElement :: QuerySelector -> Effect (Maybe HTMLElement)
selectElement query = do
  mel <- (querySelector query <<< HTMLDocument.toParentNode <=< Window.document) =<< window
  pure $ HTMLElement.fromElement =<< mel

-- | Runs an `Observer` in the background, calling a handler on completion
runObserver :: forall x. Observer x -> (x -> Effect Unit) -> Effect Unit
runObserver o handler = void $ observe o handler
