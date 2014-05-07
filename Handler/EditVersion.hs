module Handler.EditVersion where

import Import

import Yesod
import Control.Error
import Control.Monad
import Data.Maybe
import Util.Forms
import Util.QuickMod
import qualified Data.Text as T
import qualified Data.QuickMod as Q
import qualified Text.Blaze.Html5 as H


-- {{{ Version edit form

versionEditForm :: Maybe VersionInfoForm -> Form VersionInfoForm
versionEditForm fd = renderDivsUk $ VersionInfoForm
    <$> areq textField  (fs "name"          MsgVsnNameLabel         $ Just MsgVsnNameTip)       (vifName <$> fd)
    <*> aopt textField  (fs "type"          MsgVsnTypeLabel         $ Just MsgVsnTypeTip)       (vifType <$> fd)
    <*> areq dltField   (fs "dlType"        MsgVsnDlTypeLabel       $ Just MsgVsnDlTypeTip)     (vifDlType <$> fd)
    <*> areq insttField (fs "installType"   MsgVsnInstTypeLabel     $ Just MsgVsnInstTypeTip)   (vifInstallType <$> fd)
    -- FIXME: This is only optional for certain install types.
    <*> aopt urlField   (fs "url"           MsgVsnUrlLabel          $ Just MsgVsnUrlTip)        (vifUrl <$> fd)
  where
    fs :: Text -> AppMessage -> Maybe AppMessage -> FieldSettings App
    fs fid label tooltip = fieldS fid (SomeMessage label) (SomeMessage <$> tooltip)

    dltField = selectFieldList
        [ (MsgVsnDirectDl, Q.DirectDL)
        , (MsgVsnParallelDl, Q.ParallelDL)
        , (MsgVsnSequentialDl, Q.SequentialDL)
        ]
    insttField = selectFieldList
        [ (MsgVsnForgeModInst, Q.ForgeMod)
        , (MsgVsnFCoreModInst, Q.ForgeCoreMod)
        , (MsgVsnConfigPackInst, Q.ConfigPack)
        , (MsgVsnGroupModInst, Q.GroupMod)
        ]

-- | Data type for holding information about a version.
data VersionInfoForm = VersionInfoForm
    { vifName           :: Text
    , vifType           :: Maybe Text
    , vifDlType         :: Q.DownloadType
    , vifInstallType    :: Q.InstallType
    , vifUrl            :: Maybe Text
    }

-- }}}

-- {{{ Form widget

vsnFormWidget :: AppMessage -> Route App -> (Widget, Enctype) -> Widget
vsnFormWidget title route (wform, enctype) = [whamlet|
    <form .uk-form .uk-form-horizontal method=post action=@{route} enctype=#{enctype}>
        <fieldset>
            <legend>_{title}
            ^{wform}
        <button>_{MsgSaveBtn}
    |]

-- }}}

-- TODO: D.R.Y. - Combine these and generalize.
-- {{{ Add version

addVsnFormWidget :: Text -> (Widget, Enctype) -> Widget
addVsnFormWidget uid = vsnFormWidget MsgAddVsnTitle (AddVersionR uid)

getAddVersionR :: Text -> Handler Html
getAddVersionR uid = do
    qm <- entityVal <$> requireEditableQuickMod uid
    formInfo <- generateFormPost $ versionEditForm $ Nothing
    defaultLayout $ addVsnFormWidget uid formInfo

postAddVersionR :: Text -> Handler Html
postAddVersionR uid = do
    qmId <- entityKey <$> requireEditableQuickMod uid
    ((result, wform), enctype) <- runFormPost $ versionEditForm $ Nothing
    case result of
         FormMissing -> error "Form missing"
         FormFailure errs -> defaultLayout $ addVsnFormWidget uid (wform, enctype)
         FormSuccess vif -> do
             let vsn = QmVersion {
                   qmVersionName = vifName vif
                 , qmVersionMod = qmId
                 , qmVersionType = vifType vif
                 , qmVersionMcCompat = []
                 , qmVersionForgeCompat = Nothing
                 , qmVersionDlType = Just $ vifDlType vif
                 , qmVersionInstallType = Just $ vifInstallType vif
                 , qmVersionMd5 = Nothing -- TODO: MD5 support
                 , qmVersionUrl = vifUrl vif
                 }
             -- FIXME: Trying to add a duplicate gives a really ugly error message.
             runDB $ insert_ vsn
             redirect $ QuickModPageR uid

-- }}}

-- {{{ Edit version

editVsnFormWidget :: Text -> Text -> (Widget, Enctype) -> Widget
editVsnFormWidget uid vname = vsnFormWidget MsgEditVsnTitle (EditVersionR uid vname)

-- | Generates a VersionInfoForm object from the given database entry.
vsnFormInfo :: QmVersion -> VersionInfoForm
vsnFormInfo vsn = VersionInfoForm {
      vifName = qmVersionName vsn
    , vifType = qmVersionType vsn
    , vifDlType = fromMaybe Q.ParallelDL $ qmVersionDlType vsn
    , vifInstallType = fromMaybe Q.ForgeMod $ qmVersionInstallType vsn
    , vifUrl = qmVersionUrl vsn
    }

getEditVersionR :: Text -> Text -> Handler Html
getEditVersionR uid vname = do
    vsn <- entityVal <$> requireEditableQmVsn uid vname
    formInfo <- generateFormPost $ versionEditForm $ Just $ vsnFormInfo vsn
    defaultLayout $ editVsnFormWidget uid vname formInfo

postEditVersionR :: Text -> Text -> Handler Html
postEditVersionR uid vname = do
    vsnEnt <- requireEditableQmVsn uid vname
    let vsn = entityVal vsnEnt
        vsnId = entityKey vsnEnt
    ((result, wform), enctype) <- runFormPost $ versionEditForm $ Just $ vsnFormInfo vsn
    case result of
         FormMissing -> error "Form missing"
         FormFailure errs -> defaultLayout $ editVsnFormWidget uid vname (wform, enctype)
         FormSuccess vif -> do
             runDB $ update vsnId
                [ QmVersionName =. vifName vif
                , QmVersionType =. vifType vif
                , QmVersionDlType =. (Just $ vifDlType vif)
                , QmVersionInstallType =. (Just $ vifInstallType vif)
                , QmVersionUrl =. vifUrl vif
                ]
             renderMsg <- getMessageRender
             setMessage $ toHtml $ renderMsg $ MsgVersionUpdated
             redirect $ QuickModPageR uid

-- }}}

