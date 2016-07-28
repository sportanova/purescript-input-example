module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen
import Halogen.Util (awaitBody, runHalogenAff)
import Halogen.HTML.Properties.Indexed as PI
import Halogen.HTML.Elements as E
import Halogen.HTML.Indexed as HI
import Data.Generic (class Generic, gCompare, gEq)
import Control.Monad.Aff (Aff)
import Halogen.HTML.Core (Prop, prop, propName, attrName)
import Halogen.HTML.Elements.Indexed (Leaf)
import Halogen.HTML.Properties.Indexed (IProp, I)
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (Maybe(..))
import Control.Monad.Eff.JQuery as J

inputx :: forall p i. Leaf (accept :: I,  autocapitalize :: I, autocomplete :: I, autocorrect :: I, autofocus :: I, checked :: I, disabled :: I, form :: I, formaction :: I, formenctype :: I, formmethod :: I, formnovalidate :: I, formtarget :: I, height :: I, list :: I, max :: I, min :: I, multiple :: I, onAbort :: I, onChange :: I, onError :: I, onInput :: I, onInvalid :: I, onLoad :: I, onSearch :: I, onSelect :: I, pattern :: I, placeholder :: I, readonly :: I, required :: I, size :: I, src :: I, step :: I, inputType :: I, value :: I, width :: I) p i
inputx = unsafeCoerce E.input

refine :: forall a r i. (a -> Prop i) -> a -> IProp r i
refine = unsafeCoerce

onOffProp :: forall i. String -> Boolean -> Prop i
onOffProp pName = prop (propName pName) (Just $ attrName pName) <<< (\b -> if b then "on" else "off")

autoCapitalizeP :: forall i. Boolean -> Prop i
autoCapitalizeP = onOffProp "autocapitalize"

autocapitalize :: forall r i. Boolean -> IProp (autocapitalize :: I | r) i
autocapitalize = refine autoCapitalizeP

autoCorrectP :: forall i. Boolean -> Prop i
autoCorrectP = onOffProp "autocorrect"

autocorrect :: forall r i. Boolean -> IProp (autocorrect :: I | r) i
autocorrect = refine autoCorrectP


type State = { }

data Query a =
    Initialize a
  | Finalize a

ui :: forall eff. Component State Query (ExEff eff)
ui = lifecycleComponent {
  render,
  eval,
  initializer: Just (action Initialize),
  finalizer: Just (action Finalize)
} where

  render :: State -> ComponentHTML Query
  render st = HI.div_
    [
      HI.div_ [HI.text "autocorrect attribute not added"]
    , inputx
        [
          PI.name "example"
        , PI.autocomplete false
        , autocorrect false
        , autocapitalize false
        , PI.spellcheck false
        , PI.inputType PI.InputText
        ]
    , HI.div_ [HI.text "autocorrect attribute added through jquery"]
    , inputx
      [
        PI.name "working-example"
      , PI.id_ "working-example"
      , PI.autocomplete false
      , autocorrect false
      , autocapitalize false
      , PI.spellcheck false
      , PI.inputType PI.InputText
      ]
    ]

  eval :: Natural Query (ComponentDSL State Query (ExEff eff))
  eval (Initialize next) = do
    inputEl <- fromEff $ J.select ("#working-example")
    x <- fromEff $ J.setAttr "autocorrect" "off" inputEl
    pure next
  eval (Finalize next) = pure next


data Slot = Slot

derive instance slotGeneric :: Generic Slot

instance eqSlot :: Eq Slot where
  eq = gEq

instance ordGeneric :: Ord Slot where
  compare = gCompare

type ExEff eff = Aff (EX eff)
type EX eff = HalogenEffects (eff)

main :: forall eff. Eff (EX eff) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui {} body
