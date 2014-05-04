module Util.Navbar where

import Prelude

import Yesod
import Data.Text (Text)
import Data.Maybe
import Control.Monad

-- | Widget which renders a navbar entry with a subtitle and a route.
navEntry :: (Yesod app) => Text -> Text -> Route app -> WidgetT app IO ()
navEntry title subtitle route = do
    active <- isActive
    [whamlet|
      <li :active:.uk-active>
        <a href=@{route} .uk-navbar-nav-subtitle>
          #{title}
          <div>#{subtitle}
    |]
  where
    isActive = do
        cr <- getCurrentRoute
        return $ isJust (cr >>= guard . (==route))

