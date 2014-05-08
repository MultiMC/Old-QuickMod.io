-- | Utility module for form rendering.
module Util.Forms where

import Prelude
import Control.Monad
import Control.Applicative
import qualified Data.Text as T
import Yesod
import Yesod.Form
import Yesod.Form.Input
import Yesod.Form.Functions

-- {{{ Render functions

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

-- }}}

-- {{{ Custom fields

-- | A form field that displays a text box and takes a comma separated list as input.
listField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m [T.Text]
listField = Field
    { fieldParse = parseHelper $ (Right . filter (/="") . map T.strip . T.split (==',')) -- Split by commas and dropp empty entries.
    , fieldView = \fid name attrs val isReq ->
        let valStr = T.intercalate ", " <$> val in
        [whamlet|
            $newline never
            <input id="#{fid}" name="#{name}" *{attrs} type="text" :isReq:required value="#{either id id valStr}">
        |]
    , fieldEnctype = UrlEncoded
    }

-- }}}

-- {{{ Field settings stuff

-- Generates field settings.
fieldS :: (Yesod app) => T.Text -> SomeMessage app -> Maybe (SomeMessage app) -> FieldSettings app
fieldS fid label tt = FieldSettings
    { fsLabel = label
    , fsTooltip = tt
    , fsId = Just fid
    , fsName = Just fid
    , fsAttrs = [("class", "uk-form-width-large")]
    }

-- }}}

