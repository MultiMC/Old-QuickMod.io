-- | Additional error handling utilities.
module Util.Error where

import Control.Error
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Maybe

hoistMaybeT :: Monad m => m (Maybe a) -> MaybeT m a
hoistMaybeT m = hoistMaybe =<< lift m

