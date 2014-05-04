module Util.QuickMod where

import Import

-- | Returns the QuickMod database entry for the given UID or 404s.
requireQuickMod :: Text -> HandlerT App IO (Entity QuickMod)
requireQuickMod uid = do
    qm' <- runDB $ getBy $ UniqueUid uid
    case qm' of
         Nothing -> notFound
         Just qm -> return qm

