-- | Utility module for form rendering.
module Util.Forms where

import Prelude
import Control.Monad
import Yesod
import Yesod.Form.Functions

renderDivsUk :: Monad m => FormRender m a
renderDivsUk aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
        widget = [whamlet|
$newline never
\#{fragment}
$forall view <- views
    <div :fvRequired view:.required :not $ fvRequired view:.optional .uk-form-row>
        <label .uk-form-label for=#{fvId view}>#{fvLabel view}
        <div .uk-form-controls>
            ^{fvInput view}
            $maybe tt <- fvTooltip view
                <div .uk-form-help-block>#{tt}
            $maybe err <- fvErrors view
                <div .uk-form-help-block>#{err}
|]
    return (res, widget)

