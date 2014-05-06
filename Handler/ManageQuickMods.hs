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
    ^{tEntry  "uid"           MsgModUidLabel $            qifUid qm}
    ^{tEntry  "authors"       MsgModAuthorsLabel $        qifAuthors qm}
    ^{uEntryM "website"       MsgModWebsiteLabel $        qifWebsite qm}
    ^{uEntryM "issuesUrl"     MsgModIssuesUrlLabel $      qifIssuesUrl qm}
    ^{uEntryM "donationsUrl"  MsgModDonationsUrlLabel $   qifDonationsUrl qm}
    ^{tEntry  "categories"    MsgModCatsLabel $           qifCats qm}
    ^{tEntry  "tags"          MsgModTagsLabel $           qifTags qm}
    |]
  where
    tEntry = textEntry
    tEntryM = entryM textEntry
    uEntryM = entryM urlEntry

-- | A form for editing QuickMod info.
editForm :: Maybe QInfoFormData -> Form QInfoFormData
editForm fData = renderDivsUk $ QInfoFormData
    <$> areq textField      (fsl MsgModUidLabel           $ Just MsgModUidTooltip)      (qifUid <$> fData)
    <*> areq textField      (fsl MsgModNameLabel          $ Nothing)                    (qifName <$> fData)
    <*> aopt urlField       (fsl MsgModWebsiteLabel       $ Nothing)                    (qifWebsite <$> fData)
    <*> aopt urlField       (fsl MsgModIssuesUrlLabel     $ Nothing)                    (qifIssuesUrl <$> fData)
    <*> aopt urlField       (fsl MsgModDonationsUrlLabel  $ Nothing)                    (qifDonationsUrl <$> fData)
    <*> areq textField      (fsl MsgModAuthorsLabel       $ Nothing)                    (qifAuthors <$> fData) -- TODO: List fields
    <*> areq textField      (fsl MsgModCatsLabel          $ Nothing)                    (qifCats <$> fData)
    <*> areq textField      (fsl MsgModTagsLabel          $ Nothing)                    (qifTags <$> fData)
  where
    -- Generates field settings.
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

-- | Constructs a QInfoFormData object from the given QModPageInfo
formData :: QModPageInfo -> QInfoFormData
formData (QModPageInfo qm _ authors _) = QInfoFormData
    { qifUid = quickModUid qm
    , qifName = quickModName qm
    , qifWebsite = quickModWebsite qm
    , qifIssuesUrl = quickModIssuesUrl qm
    , qifDonationsUrl = quickModDonationsUrl qm
    , qifAuthors = joinWith' ", " $ map qmAuthorName $ authors
    , qifTags = joinWith' ", " $ quickModTags qm
    , qifCats = joinWith' ", " $ quickModCategories qm
    }

-- }}}

-- {{{ Fields

type EntryField a = Text -> AppMessage -> a -> Widget

-- | Shows the given QuickMod information in a description list with the given label if it exists.
entryM :: EntryField a -> Text -> AppMessage -> Maybe a -> Widget
entryM f fId msg mVal = maybe [whamlet||] (f fId msg) mVal

-- | Shows the given QuickMod information in a description list with the given label.
textEntry :: Text -> AppMessage -> Text -> Widget
textEntry fId msg text = [whamlet|
    <tr>
        <th>_{msg}
        <td id=#{fId}>#{text}
    |]

-- | Shows the given URL in a description list with the given label.
urlEntry :: Text -> AppMessage -> Text -> Widget
urlEntry fId msg text = [whamlet|
    <tr>
        <th>_{msg}
        <td id=#{fId}>
            <a href=#{text}>#{text}
    |]

-- }}}

-- {{{ Page info

-- | Gets a QModPageInfo object for the given uid or 404s if there is no such QuickMod.
requireQMPageInfo :: Text -> Handler QModPageInfo
requireQMPageInfo uid = runDB $ do
        qmEnt <- requireQuickModDB uid
        let qm = entityVal qmEnt
            qmId = entityKey qmEnt
        authors  <- map entityVal <$> selectList [QmAuthorMod ==. qmId] []
        versions <- map entityVal <$> selectList [QmVersionMod ==. qmId] []
        return $ QModPageInfo qm qmId authors versions

data QModPageInfo = QModPageInfo
    { qiMod     :: QuickMod
    , qiModId   :: QuickModId
    , qiAuthors :: [QmAuthor]
    , qiVsns    :: [QmVersion]
    }

-- }}}

-- {{{ Add QuickMod

getAddQuickModR :: Handler Html
getAddQuickModR = do
    (wform, enctype) <- generateFormPost $ editForm $ Nothing
    defaultLayout $ addModFormWidget wform enctype []

postAddQuickModR :: Handler Html
postAddQuickModR = do
    userId <- requireAuthId
    ((result, wform), enctype) <- runFormPost $ editForm $ Nothing
    case result of
         FormMissing -> error "Form missing"
         FormFailure errs -> defaultLayout $ addModFormWidget wform enctype errs
         FormSuccess qif  -> do
             let quickMod = QuickMod {
                  quickModUid = qifUid qif
                , quickModName = qifName qif
                , quickModOwner = userId
                , quickModDesc = ""
                , quickModIcon = Nothing
                , quickModLogo = Nothing
                , quickModWebsite = qifWebsite qif
                , quickModIssuesUrl = qifIssuesUrl qif
                , quickModDonationsUrl = qifDonationsUrl qif
                , quickModTags = []
                , quickModCategories = []
                }
             runDB $ insert_ quickMod
             redirect $ QuickModPageR $ qifUid qif

addModFormWidget :: Widget -> Enctype -> [Text] -> Widget
addModFormWidget wform enctype errs = [whamlet|
        <form .uk-form .uk-form-horizontal method=post action=@{AddQuickModR} enctype=#{enctype}>
            <fieldset>
                <legend>_{MsgAddFormLegend}
                ^{wform}
            <button>_{MsgSubmitBtn}
    |]

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
    let infoFormData = formData info
        description = linesToParagraphs $ quickModDesc qm
    defaultLayout $ do
        renderMsg <- getMessageRender
        setTitle $ toHtml $ renderMsg $ MsgModPageTitle $ quickModName qm
        $(widgetFile "quickmod-page")

-- }}}

-- {{{ QuickMod editor

-- | Renders QuickMod edit widget.
quickModEditWidget :: Text -> QModPageInfo -> Widget -> Enctype -> Widget
quickModEditWidget uid (QModPageInfo qm _ _ _) wform enctype = do
    renderMsg <- getMessageRender
    setTitle $ toHtml $ renderMsg $ MsgEditModPageTitle $ quickModName qm
    $(widgetFile "quickmod-edit")

getQuickModEditR :: Text -> Handler Html
getQuickModEditR uid = do
    info@(QModPageInfo qm _ _ _) <- requireQMPageInfo uid
    requireCanEdit qm
    (wform, enctype) <- generateFormPost $ editForm $ Just $ formData info
    defaultLayout $ quickModEditWidget uid info wform enctype

-- | Post handler for the edit page.
postQuickModEditR :: Text -> Handler Html
postQuickModEditR uid = do
    info@(QModPageInfo qm qmId _ _) <- requireQMPageInfo uid
    requireCanEdit qm
    ((result, wform), enctype) <- runFormPost $ editForm $ Just $ formData info
    case result of
         FormMissing -> invalidArgs ["Form missing"]
         FormFailure _ -> defaultLayout $ quickModEditWidget uid info wform enctype
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

