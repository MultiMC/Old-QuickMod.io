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
import Yesod.Auth
import Yesod.Form.Input

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

-- {{{ QuickMod Page

requireQMPageInfo :: Text -> Handler QModPageInfo
requireQMPageInfo uid = do
    -- TODO: Do all of this in one DB transaction.
    qmEnt <- requireQuickMod uid
    let qm = entityVal qmEnt
        qmId = entityKey qmEnt
    authors <- runDB $ map entityVal <$> selectList [QmAuthorMod ==. qmId] []
    versions <- runDB $ map entityVal <$> selectList [QmVersionMod ==. qmId] []
    return $ QModPageInfo qm authors versions

data QModPageInfo = QModPageInfo
    { qiMod     :: QuickMod
    , qiAuthors :: [QmAuthor]
    , qiVsns    :: [QmVersion]
    }


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


-- | Takes the given text and converts newlines into HTML <p> tags.
linesToParagraphs :: Text -> Html
linesToParagraphs = mapM_ (H.p . toHtml) . T.lines


-- | Checks if the current user can edit the given QuickMod.
canEdit :: QuickMod -> Handler Bool
canEdit qm = isJustT $ do
    usrId <- hoistMaybeT $ maybeAuthId
    guard (quickModOwner qm == usrId)
    return ()

getQuickModPageR :: Text -> Handler Html
getQuickModPageR uid = do
    info@(QModPageInfo qm authors versions) <- requireQMPageInfo uid
    editing <- canEdit qm
    let (iEntry, iEntryM) = (infoEntry editing, infoEntryM editing)
    let description = linesToParagraphs $ quickModDesc qm
    -- TODO: make the URLs actual links.
    defaultLayout $ do
        $(combineScripts 'StaticR
            [ js_jquery_jeditable_mini_js ])
        renderMsg <- getMessageRender
        setTitle $ toHtml $ renderMsg $ MsgModPageTitle $ quickModName qm
        $(widgetFile "quickmod-page")


-- | Form data for QuickMod edits
data QModEditFormData = QModEditFormData
    { fieldId       :: Text
    , fieldValue    :: Text
    }

quickModEditForm :: FormInput Handler QModEditFormData
quickModEditForm = QModEditFormData
    <$> ireq textField "id"
    <*> ireq textField "value"

postQuickModEditR :: Text -> Handler Text
postQuickModEditR uid = do
    qmEnt <- requireQuickMod uid
    let qmId = entityKey qmEnt
        qm = entityVal qmEnt
    allowEdit <- canEdit qm
    if not allowEdit
       then permissionDeniedI MsgCantEdit
       else return ()
    (QModEditFormData field value) <- runInputPost quickModEditForm
    let updateQ = case field of
         "name" ->              [QuickModName =. value]
         "icon" ->              [QuickModIcon =. Just value]
         "logo" ->              [QuickModLogo =. Just value]
         "website" ->           [QuickModWebsite =. Just value]
         "issuesUrl" ->         [QuickModIssuesUrl =. Just value]
         "donationsUrl" ->      [QuickModDonationsUrl =. Just value]
         _ ->           []
    runDB $ update qmId updateQ
    return ""

-- }}}

