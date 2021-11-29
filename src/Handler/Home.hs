{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

-- | welcome and about
getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "welcome to logicstuff!"
        $(widgetFile "homepage")

-- | problemsets main page
getProblemSetsR :: Handler Html
getProblemSetsR = do
    defaultLayout $ do
        setTitle "logicstuff | problemsets"
        $(widgetFile "problemsets")

-- | problemset01
getProblemSet01R :: Handler Html
getProblemSet01R = do
    defaultLayout $ do
        setTitle "logicstuff | problemsets/problemset01"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet02R :: Handler Html
getProblemSet02R = do
    defaultLayout $ do
        setTitle "logicstuff | problemsets/problemset02"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet03R :: Handler Html
getProblemSet03R = do
    defaultLayout $ do
        setTitle "logicstuff | problemsets/problemset03"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet04R :: Handler Html
getProblemSet04R = do
    defaultLayout $ do
        setTitle "logicstuff | problemsets/problemset04"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet05R :: Handler Html
getProblemSet05R = do
    defaultLayout $ do
        setTitle "logicstuff | problemsets/problemset05"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet06R :: Handler Html
getProblemSet06R = do
    defaultLayout $ do
        setTitle "logicstuff | problemsets/problemset06"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet07R :: Handler Html
getProblemSet07R = do
    defaultLayout $ do
        setTitle "logicstuff | problemsets/problemset07"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet08R :: Handler Html
getProblemSet08R = do
    defaultLayout $ do
        setTitle "logicstuff | problemsets/problemset08"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet09R :: Handler Html
getProblemSet09R = do
    defaultLayout $ do
        setTitle "logicstuff | problemsets/problemset09"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet10R :: Handler Html
getProblemSet10R = do
    defaultLayout $ do
        setTitle "logicstuff | problemsets/problemset10"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet11R :: Handler Html
getProblemSet11R = do
    defaultLayout $ do
        setTitle "logicstuff | problemsets/problemset11"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSetsAllR :: Handler Html
getProblemSetsAllR = do
    defaultLayout $ do
        setTitle "logicstuff | problemsets/problemsetsall"
        $(widgetFile "problemsetscommon")

-- | trees main page
getTreesR :: Handler Html
getTreesR = do
    defaultLayout $ do
        setTitle "logicstuff | trees"
        $(widgetFile "trees")

-- | trees main page
postTreesR :: Handler Html
postTreesR = do
    defaultLayout $ do
        setTitle "logicstuff | trees"
        $(widgetFile "trees")