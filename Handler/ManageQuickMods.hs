module Handler.ManageQuickMods where

import Import

import qualified Data.Text as T

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

