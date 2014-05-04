module Handler.ManageQuickMods where

import Import

import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import Util.Text
import Util.QuickMod

-- {{{ Add QuickMod

getAddQuickModR :: Handler Html
getAddQuickModR = do
    (wform, enctype) <- generateFormPost addModForm
    defaultLayout $ formWidget wform enctype []

postAddQuickModR :: Handler Html
postAddQuickModR = do
    ((result, wform), enctype) <- runFormPost addModForm
    case result of
         FormMissing -> error "Form missing"
         FormFailure errs -> defaultLayout $ formWidget wform enctype errs
         FormSuccess qmf  -> do
             let quickMod = QuickMod {
                  quickModUid = qmfUid qmf
                , quickModName = qmfName qmf
                --, quickModOwner = UserId 0 -- TODO: Implement
                , quickModDesc = qmfDesc qmf
                , quickModIcon = qmfIcon qmf
                , quickModLogo = qmfLogo qmf
                , quickModWebsite = qmfWebsite qmf
                , quickModIssuesUrl = Nothing
                , quickModDonationsUrl = Nothing
                , quickModTags = map T.strip $ T.splitOn "," $ qmfTags qmf
                , quickModCategories = map T.strip $ T.splitOn "," $ qmfCats qmf
                }
             runDB $ insert_ quickMod
             redirect $ QuickModFileR $ qmfUid qmf

formWidget :: Widget -> Enctype -> [Text] -> Widget
formWidget wform enctype errs =
    [whamlet|
        <form method=post action=@{AddQuickModR} enctype=#{enctype}>
            <div class="alert">#{show errs}
            ^{wform}
            <button>Add
    |]

-- | The form
addModForm :: Form QuickModForm
addModForm = renderDivs $ QuickModForm
    <$> areq textField "Mod ID" Nothing
    <*> areq textField "Mod name" Nothing
    <*> (unTextarea <$> areq textareaField "Mod description" Nothing)
    <*> aopt textField "Mod icon" Nothing
    <*> aopt textField "Mod logo" Nothing
    <*> aopt textField "Mod website" Nothing
    <*> areq textField "Mod tags" Nothing
    <*> areq textField "Mod categories" Nothing

data QuickModForm =
    QuickModForm { qmfUid       :: Text
                 , qmfName      :: Text
                 , qmfDesc      :: Text
                 , qmfIcon      :: Maybe Text
                 , qmfLogo      :: Maybe Text
                 , qmfWebsite   :: Maybe Text
                 , qmfTags      :: Text
                 , qmfCats      :: Text -- ~meow
                 } deriving (Show)

-- }}}

-- {{{ QuickMod Page

getQuickModPageR :: Text -> Handler Html
getQuickModPageR uid = do
    -- TODO: Do all of this in one DB transaction.
    qmEnt <- requireQuickMod uid
    let qm = entityVal qmEnt
        qmId = entityKey qmEnt
    authors <- runDB $ map entityVal <$> selectList [QmAuthorMod ==. qmId] []

    let paragraphs = linesToParagraphs $ quickModDesc qm
    -- TODO: make the URLs actual links.
    defaultLayout [whamlet|
        <h1>#{quickModName qm}
        <table .uk-table .uk-table-striped .uk-table-hover>
            <thead>
                <tr>
                    <th colspan=2>_{MsgModInfoTable}
            <tbody>
                ^{infoEntry  MsgModUidLabel $           quickModUid qm}
                ^{infoEntry  MsgModAuthorsLabel $       (joinWith' ", " $ map qmAuthorName $ authors)}
                ^{infoEntryM MsgModWebsiteLabel $       quickModWebsite qm}
                ^{infoEntryM MsgModIssuesUrlLabel $     quickModIssuesUrl qm}
                ^{infoEntryM MsgModDonationsUrlLabel $  quickModDonationsUrl qm}
                ^{infoEntry  MsgModCatsLabel $          (joinWith' ", " $ quickModCategories qm)}
                ^{infoEntry  MsgModTagsLabel $          (joinWith' ", " $ quickModTags qm)}

        #{paragraphs}
        |]


-- | Shows the given QuickMod information in a description list with the given label if it exists.
infoEntryM :: AppMessage -> Maybe Text -> Widget
infoEntryM msg mText = maybe [whamlet||] (infoEntry msg) mText

-- | Shows the given QuickMod information in a description list with the given label.
infoEntry :: AppMessage -> Text -> Widget
infoEntry msg text = [whamlet|
    <tr>
        <th>_{msg}
        <td>#{text}
    |]


-- | Takes the given text and converts newlines into HTML <p> tags.
linesToParagraphs :: Text -> Html
linesToParagraphs = mapM_ (H.p . toHtml) . T.lines

-- }}}

