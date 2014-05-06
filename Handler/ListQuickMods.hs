module Handler.ListQuickMods where

import Import

getListQuickModsR :: Handler Html
getListQuickModsR = do
    -- TODO: Add filtering parameter stuff.
    mods <- runDB $ selectList [] []
    defaultLayout $ do
        $(widgetFile "mod-list")

