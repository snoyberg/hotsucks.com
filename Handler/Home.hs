{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3
    ( BootstrapFormLayout (..), renderBootstrap3, withSmallInput )
import Yesod.Sitemap

getHomeR :: Handler Html
getHomeR = do
    (contentMap, homepageContent) <- getContent
    let content = mapToList contentMap
    defaultLayout $ do
        setTitle "Israel's cable company HOT Sucks"
        $(widgetFile "homepage")

getStoryR :: Text -> Handler Html
getStoryR name = do
    (content, _) <- getContent
    (title, body) <- maybe notFound return $ lookup name content
    defaultLayout $ do
        setTitle $ toHtml title
        $(widgetFile "story")

getSitemapR :: Handler TypedContent
getSitemapR = do
    (content, _) <- getContent
    sitemapList $ home : map toSM (mapToList content)
  where
    home = SitemapUrl
        { sitemapLoc = HomeR
        , sitemapLastMod = Nothing
        , sitemapChangeFreq = Just Weekly
        , sitemapPriority = Just 1
        }
    toSM (name, _) = SitemapUrl
        { sitemapLoc = StoryR name
        , sitemapLastMod = Nothing
        , sitemapChangeFreq = Just Monthly
        , sitemapPriority = Just 0.7
        }

getRobotsR :: Handler Text
getRobotsR = robots SitemapR