module Util.QuickMod where

import Import

-- | Returns the QuickMod database entry for the given UID or 404s.
requireQuickMod :: Text -> HandlerT App IO (Entity QuickMod)
requireQuickMod = runDB . requireQuickModDB

-- | Does the same as requireQuickMod, but inside a database transaction.
requireQuickModDB :: Text -> YesodPersistBackend App (HandlerT App IO) (Entity QuickMod)
requireQuickModDB uid = do
    qm' <- getBy $ UniqueUid uid
    case qm' of
         Nothing -> lift $ notFound
         Just qm -> return qm

