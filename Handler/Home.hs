{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3
    ( BootstrapFormLayout (..), renderBootstrap3, withSmallInput )

getHomeR :: Handler Html
getHomeR = do
    content <- mapToList <$> getContent
    defaultLayout $ do
        setTitle "Israel's cable company HOT Sucks"
        $(widgetFile "homepage")

getStoryR :: Text -> Handler Html
getStoryR name = do
    content <- getContent
    (title, body) <- maybe notFound return $ lookup name content
    defaultLayout $ do
        setTitle $ toHtml title
        $(widgetFile "story")