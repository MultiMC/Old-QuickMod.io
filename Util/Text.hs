-- | Text manipulation utilities.
module Util.Text where

import Prelude
import qualified Data.Text as T

-- | Joins a list of texts with the given text separating them.
joinWith :: T.Text -> [T.Text] -> T.Text
joinWith sep = T.concat . map (`T.append` sep)

-- | Similar to joinWith, but doesn't add the separator at the end.
joinWith' :: T.Text -> [T.Text] -> T.Text
joinWith' sep = T.dropEnd (T.length sep) . T.concat . map (`T.append` sep)

