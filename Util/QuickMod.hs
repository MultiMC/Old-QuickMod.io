module Util.QuickMod where

import Import

import Control.Error
import Control.Monad
import Util.Error
import Yesod.Auth

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

-- | Does the same as requireQuickMod, but returns permission denied if the user can't edit the given mod.
requireEditableQuickMod :: Text -> HandlerT App IO (Entity QuickMod)
requireEditableQuickMod uid = do
    qm <- requireQuickMod uid
    requireCanEditMod $ entityVal qm
    return qm


-- | Checks if the current user can edit the given QuickMod.
canEditMod :: QuickMod -> Handler Bool
canEditMod qm = isJustT $ do
    usrId <- hoistMaybeT $ maybeAuthId
    guard (quickModOwner qm == usrId)
    return ()

-- | Shows a permission denied page if the user can't edit the given QuickMod.
requireCanEditMod :: QuickMod -> Handler ()
requireCanEditMod qm = do
    allowEdit <- canEditMod qm
    if allowEdit
       then return ()
       else permissionDeniedI MsgCantEdit


-- | Gets a version the user can edit from the database.
-- 404s if it doesn't exist.
-- Gives permission denied if the user can't edit the mod.
requireEditableQmVsn :: Text -> Text -> Handler (Entity QmVersion)
requireEditableQmVsn uid vname = do
    qmEnt <- requireEditableQuickMod uid
    requireQmVsn (entityKey qmEnt) vname

-- | Gets the given version in the given QuickMod. 404s if it doesn't exist.
requireQmVsn :: QuickModId -> Text -> Handler (Entity QmVersion)
requireQmVsn modId vsnName = runDB $ do
    vsn' <- getBy $ UniqueVsnName modId vsnName
    case vsn' of
         Nothing -> lift $ notFound
         Just vsn -> return vsn

