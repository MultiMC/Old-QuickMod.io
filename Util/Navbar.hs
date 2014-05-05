module Util.Navbar where

import Prelude

import Yesod
import Data.Text (Text)
import Data.Maybe
import Control.Monad

-- | Widget which renders a navbar entry with a subtitle and a route.
navEntry :: (Yesod app) => SomeMessage app -> SomeMessage app -> Route app -> WidgetT app IO ()
navEntry title subtitle route = do
    active <- isActive
    [whamlet|
      <li :active:.uk-active>
        <a href=@{route} .uk-navbar-nav-subtitle>
          _{title}
          <div>_{subtitle}
    |]
  where
    isActive = do
        cr <- getCurrentRoute
        return $ isJust (cr >>= guard . (==route))

