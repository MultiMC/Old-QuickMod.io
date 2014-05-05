module Handler.ManageQuickMods where

import Import

import Control.Error
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import Util.Text
import Util.QuickMod
import Util.Error
import Util.Forms
import Yesod.Auth
import Yesod.Form.Input

-- {{{ Info form

-- | A table for displaying QuickMod info
infoTable :: QInfoFormData -> Widget
infoTable qm = [whamlet|
    ^{iEntry  "uid"           MsgModUidLabel $            qifUid qm}
    ^{iEntry  "authors"       MsgModAuthorsLabel $        qifAuthors qm}
    ^{iEntryM "website"       MsgModWebsiteLabel $        qifWebsite qm}
    ^{iEntryM "issuesUrl"     MsgModIssuesUrlLabel $      qifIssuesUrl qm}
    ^{iEntryM "donationsUrl"  MsgModDonationsUrlLabel $   qifDonationsUrl qm}
    ^{iEntry  "categories"    MsgModCatsLabel $           qifCats qm}
    ^{iEntry  "tags"          MsgModTagsLabel $           qifTags qm}
    |]
  where
    iEntry = infoEntry False
    iEntryM = infoEntryM False

-- | A form for editing QuickMod info.
editForm :: Maybe QInfoFormData -> Form QInfoFormData
editForm formData = do
    let defUid          = qifUid <$> formData
        defName         = qifName <$> formData
        defWebsite      = qifWebsite <$> formData
        defIssuesUrl    = qifIssuesUrl <$> formData
        defDonationsUrl = qifDonationsUrl <$> formData
        defAuthors      = qifAuthors <$> formData
        defCats         = qifCats <$> formData
        defTags         = qifTags <$> formData
    renderDivsUk $ QInfoFormData
        <$> areq textField      (fsl MsgModUidLabel           $ Just MsgModUidTooltip)      defUid
        <*> areq textField      (fsl MsgModNameLabel          $ Nothing)                    defName
        <*> aopt urlField       (fsl MsgModWebsiteLabel       $ Nothing)                    defWebsite
        <*> aopt urlField       (fsl MsgModIssuesUrlLabel     $ Nothing)                    defIssuesUrl
        <*> aopt urlField       (fsl MsgModDonationsUrlLabel  $ Nothing)                    defDonationsUrl
        <*> areq textField      (fsl MsgModAuthorsLabel       $ Nothing)                    defAuthors  -- TODO: List fields
        <*> areq textField      (fsl MsgModCatsLabel          $ Nothing)                    defCats
        <*> areq textField      (fsl MsgModTagsLabel          $ Nothing)                    defTags
  where
    fsl :: AppMessage -> Maybe AppMessage -> FieldSettings App
    fsl msg tt = FieldSettings
        { fsLabel = SomeMessage msg
        , fsTooltip = SomeMessage <$> tt
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs = [("class", "uk-form-width-large")]
        }

-- | Data type for holding QuickMod info for the table.
data QInfoFormData = QInfoFormData
    { qifUid            :: Text
    , qifName           :: Text
    , qifWebsite        :: Maybe Text
    , qifIssuesUrl      :: Maybe Text
    , qifDonationsUrl   :: Maybe Text
    , qifAuthors        :: Text -- TODO: Make this a list somehow. Will need to define new form fields.
    , qifCats           :: Text -- ~meow
    , qifTags           :: Text
    }

formDataFromInfo :: QModPageInfo -> QInfoFormData
formDataFromInfo (QModPageInfo qm _ authors _) = QInfoFormData
    { qifUid = quickModUid qm
    , qifName = quickModName qm
    , qifWebsite = quickModWebsite qm
    , qifIssuesUrl = quickModIssuesUrl qm
    , qifDonationsUrl = quickModDonationsUrl qm
    , qifAuthors = joinWith' ", " $ map qmAuthorName $ authors
    , qifTags = joinWith' ", " $ quickModTags qm
    , qifCats = joinWith' ", " $ quickModCategories qm
    }

collapseMaybe :: Maybe (Maybe a) -> Maybe a
collapseMaybe val = val ?: Nothing

-- }}}

-- {{{ Page info

requireQMPageInfo :: Text -> Handler QModPageInfo
requireQMPageInfo uid = do
    -- TODO: Do all of this in one DB transaction.
    qmEnt <- requireQuickMod uid
    let qm = entityVal qmEnt
        qmId = entityKey qmEnt
    authors <- runDB $ map entityVal <$> selectList [QmAuthorMod ==. qmId] []
    versions <- runDB $ map entityVal <$> selectList [QmVersionMod ==. qmId] []
    return $ QModPageInfo qm qmId authors versions

data QModPageInfo = QModPageInfo
    { qiMod     :: QuickMod
    , qiModId   :: QuickModId
    , qiAuthors :: [QmAuthor]
    , qiVsns    :: [QmVersion]
    }

-- }}}

-- {{{ Page stuff

-- | Shows the given QuickMod information in a description list with the given label if it exists.
infoEntryM :: Bool -> Text -> AppMessage -> Maybe Text -> Widget
infoEntryM False fId msg mText = maybe [whamlet||]                  (infoEntry False fId msg) mText
infoEntryM True  fId msg mText = maybe (infoEntry True fId msg "")  (infoEntry True  fId msg) mText

-- | Shows the given QuickMod information in a description list with the given label.
infoEntry :: Bool -> Text -> AppMessage -> Text -> Widget
infoEntry editable fId msg text = [whamlet|
    <tr>
        <th>_{msg}
        <td :editable:.edit id=#{fId}>#{text}
    |]

-- }}}

-- {{{ Add QuickMod

getAddQuickModR :: Handler Html
getAddQuickModR = do
    (wform, enctype) <- generateFormPost addModForm
    defaultLayout $ formWidget wform enctype []

postAddQuickModR :: Handler Html
postAddQuickModR = do
    userId <- requireAuthId
    ((result, wform), enctype) <- runFormPost addModForm
    case result of
         FormMissing -> error "Form missing"
         FormFailure errs -> defaultLayout $ formWidget wform enctype errs
         FormSuccess qmf  -> do
             let quickMod = QuickMod {
                  quickModUid = qmfUid qmf
                , quickModName = qmfName qmf
                , quickModOwner = userId
                , quickModDesc = ""
                , quickModIcon = Nothing
                , quickModLogo = Nothing
                , quickModWebsite = Nothing
                , quickModIssuesUrl = Nothing
                , quickModDonationsUrl = Nothing
                , quickModTags = []
                , quickModCategories = []
                }
             runDB $ insert_ quickMod
             redirect $ QuickModPageR $ qmfUid qmf

formWidget :: Widget -> Enctype -> [Text] -> Widget
formWidget wform enctype errs =
    [whamlet|
        <form method=post action=@{AddQuickModR} enctype=#{enctype}>
            <div class="alert">#{show errs}
            ^{wform}
            <button>_{MsgSubmitBtn}
    |]

-- | The form
addModForm :: Form QuickModForm
addModForm = renderDivs $ QuickModForm
    <$> areq textField "Mod ID" Nothing
    <*> areq textField "Mod name" Nothing

data QuickModForm =
    QuickModForm { qmfUid       :: Text
                 , qmfName      :: Text
                 } deriving (Show)

-- }}}

-- {{{ Misc utilities

-- | Takes the given text and converts newlines into HTML <p> tags.
linesToParagraphs :: Text -> Html
linesToParagraphs = mapM_ (H.p . toHtml) . T.lines

-- | Checks if the current user can edit the given QuickMod.
canEdit :: QuickMod -> Handler Bool
canEdit qm = isJustT $ do
    usrId <- hoistMaybeT $ maybeAuthId
    guard (quickModOwner qm == usrId)
    return ()

-- | Shows a permission denied page if the user can't edit the given QuickMod.
requireCanEdit :: QuickMod -> Handler ()
requireCanEdit qm = do
    allowEdit <- canEdit qm
    if allowEdit
       then return ()
       else permissionDeniedI MsgCantEdit

-- }}}

-- {{{ QuickMod page

-- | Handler for the view QuickMod page.
getQuickModPageR :: Text -> Handler Html
getQuickModPageR uid = do
    info@(QModPageInfo qm _ authors versions) <- requireQMPageInfo uid
    let infoFormData = formDataFromInfo info
        description = linesToParagraphs $ quickModDesc qm
    defaultLayout $ do
        renderMsg <- getMessageRender
        setTitle $ toHtml $ renderMsg $ MsgModPageTitle $ quickModName qm
        $(widgetFile "quickmod-page")

-- }}}

-- {{{ QuickMod editor

getQuickModEditR :: Text -> Handler Html
getQuickModEditR uid = do
    info@(QModPageInfo qm _ _ _) <- requireQMPageInfo uid
    requireCanEdit qm
    (wform, enctype) <- generateFormPost $ editForm $ Just $ formDataFromInfo info
    defaultLayout $ do
        renderMsg <- getMessageRender
        setTitle $ toHtml $ renderMsg $ MsgEditModPageTitle $ quickModName qm
        $(widgetFile "quickmod-edit")

-- | Post handler for the edit page.
postQuickModEditR :: Text -> Handler Html
postQuickModEditR uid = do
    info@(QModPageInfo qm qmId _ _) <- requireQMPageInfo uid
    requireCanEdit qm
    ((result, wform), enctype) <- runFormPost $ editForm $ Just $ formDataFromInfo info
    case result of
         FormMissing -> error "Form missing"
         FormFailure err ->
             defaultLayout $ do
                 $(widgetFile "quickmod-edit")
         FormSuccess form -> do
             runDB $ update qmId
                [ QuickModUid =. qifUid form
                , QuickModName =. qifName form
                , QuickModWebsite =. qifWebsite form
                , QuickModIssuesUrl =. qifIssuesUrl form
                , QuickModDonationsUrl =. qifDonationsUrl form
                ]
             renderMsg <- getMessageRender
             setMessage $ toHtml $ renderMsg $ MsgQuickModUpdated
             redirect $ QuickModPageR $ qifUid form

-- }}}

