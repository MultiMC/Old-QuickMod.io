module Handler.QuickModFile where

import Import

import Control.Error
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Default
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Encoding as TE
import qualified Data.QuickMod as Q
import Util.Error

getQuickModFileR :: Text -> Handler Text
getQuickModFileR uid = do
    qmod' <- qmFileFromDB uid
    case qmod' of
         Nothing -> notFound
         Just qmod -> return $ TE.decodeUtf8 $ B.toStrict $ encode qmod

-- TODO: Clean up this mess

-- | This function gets a QuickMod data structure for the database entry with the given uid.
qmFileFromDB :: Text -> Handler (Maybe Q.QuickMod)
qmFileFromDB uid = do
    renderUrl <- getUrlRender
    runDB $ runMaybeT $ do
        qmEnt <- hoistMaybeT $ getBy $ UniqueUid uid
        let qm = entityVal qmEnt
            qmId = entityKey qmEnt
        authors <- map qmAuthorDB <$> selectList [QmAuthorMod ==. qmId] []
        refs <- map qmRefDB <$> selectList [QmReferenceMod ==. qmId] []
        versions <- lift $ qmVersionsFromDB qmId
        just $ def {
              Q.qmUid = quickModUid qm
            , Q.qmName = quickModName qm
            , Q.qmRepo = "quickmod.io" -- TODO: This should depend on deployment configuration.
            , Q.qmDescription = Just $ quickModDesc qm
            , Q.qmUpdateUrl = renderUrl $ QuickModFileR uid
            , Q.qmTags = quickModTags qm
            , Q.qmCategories = quickModCategories qm
            , Q.qmAuthors = authors
            , Q.qmReferences = refs
            , Q.qmVersions = versions
            }

qmAuthorDB a' = Q.Author (qmAuthorName a) (qmAuthorRoles a) where a = entityVal a'

qmRefDB r' = Q.Reference (qmReferenceUid r) (qmReferenceUrl r) where r = entityVal r'

-- | This function gets a list of QuickMod Version data structures for the given QuickMod ID.
qmVersionsFromDB :: QuickModId -> YesodPersistBackend App (HandlerT App IO) [Q.Version]
qmVersionsFromDB qmid =
    mapM (\vEnt -> do
            let v = entityVal vEnt
                vId = entityKey vEnt
            vRefs <- map vRefDB <$> selectList [QmVersionRefVerId ==. vId] []
            return $ def {
                  Q.vsnName = qmVersionName v
                , Q.vsnType = qmVersionType v
                , Q.vsnMCCompat = qmVersionMcCompat v
                , Q.vsnForgeCompat = qmVersionForgeCompat v
                , Q.vsnReferences = vRefs
                , Q.vsnDlType = qmVersionDlType v
                , Q.vsnInstallType = qmVersionInstallType v
                , Q.vsnMd5 = qmVersionMd5 v
                , Q.vsnUrl = qmVersionUrl v
                }
        ) =<< selectList [QmVersionMod ==. qmid] []

vRefDB r' = Q.VsnReference {
      Q.vrefUid = qmVersionRefUid r
    , Q.vrefType = qmVersionRefType r
    , Q.vrefVersion = qmVersionRefModVsn r
    } where r = entityVal r'


getQuickModIndexR :: Handler Html
getQuickModIndexR = error "Not yet implemented: getQuickModIndexR"

