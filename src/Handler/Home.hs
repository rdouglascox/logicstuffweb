{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

-- imports from problemsets
import MakePS.MakePS01
import MakePS.MakePS02
import MakePS.MakePS03
import MakePS.MakePS04
import MakePS.MakePS05
import MakePS.MakePS06
import MakePS.MakePS07
import MakePS.MakePS08
import MakePS.MakePS09
import MakePS.MakePS10
import MakePS.MakePS11

-- imports for pl trees form
import Forms.PLtrees

newtype PropForm = PropForm   -- my own thingy for getting propositions
    { propInput :: Text
    }


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
        ps <- liftIO mkps01html
        setTitle "logicstuff | problemsets/problemset01"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet02R :: Handler Html
getProblemSet02R = do
    defaultLayout $ do
        ps <- liftIO mkps02html
        setTitle "logicstuff | problemsets/problemset02"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet03R :: Handler Html
getProblemSet03R = do
    defaultLayout $ do
        ps <- liftIO mkps03html
        setTitle "logicstuff | problemsets/problemset03"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet04R :: Handler Html
getProblemSet04R = do
    defaultLayout $ do
        ps <- liftIO mkps04html
        setTitle "logicstuff | problemsets/problemset04"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet05R :: Handler Html
getProblemSet05R = do
    defaultLayout $ do
        ps <- liftIO mkps05html
        setTitle "logicstuff | problemsets/problemset05"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet06R :: Handler Html
getProblemSet06R = do
    defaultLayout $ do
        ps <- liftIO mkps06html
        setTitle "logicstuff | problemsets/problemset06"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet07R :: Handler Html
getProblemSet07R = do
    defaultLayout $ do
        ps <- liftIO mkps07html
        setTitle "logicstuff | problemsets/problemset07"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet08R :: Handler Html
getProblemSet08R = do
    defaultLayout $ do
        ps <- liftIO mkps08html
        setTitle "logicstuff | problemsets/problemset08"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet09R :: Handler Html
getProblemSet09R = do
    defaultLayout $ do
        ps <- liftIO mkps09html
        setTitle "logicstuff | problemsets/problemset09"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet10R :: Handler Html
getProblemSet10R = do
    defaultLayout $ do
        ps <- liftIO mkps10html
        setTitle "logicstuff | problemsets/problemset10"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet11R :: Handler Html
getProblemSet11R = do
    defaultLayout $ do
        ps <- liftIO mkps11html
        setTitle "logicstuff | problemsets/problemset11"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSetsAllR :: Handler Html
getProblemSetsAllR = do
    defaultLayout $ do
        ps1 <- liftIO mkps01html
        ps2 <- liftIO mkps02html
        ps3 <- liftIO mkps03html
        ps4 <- liftIO mkps04html
        ps5 <- liftIO mkps05html
        ps6 <- liftIO mkps06html
        ps7 <- liftIO mkps07html
        ps8 <- liftIO mkps08html
        ps9 <- liftIO mkps09html
        ps10 <- liftIO mkps10html
        ps11 <- liftIO mkps11html
        setTitle "logicstuff | problemsets/problemsetsall"
        $(widgetFile "problemsetsall")


propForm :: Form PropForm
propForm = renderBootstrap3 BootstrapBasicForm $ PropForm
    <$> areq textField textSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "Enter a list of propositions here."
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "e.g. (A->B),(CvD),((E&G)<->D)")
                ]
            }

-- let's give my form a page of its own

getTreesR :: Handler Html
getTreesR = do
    (formWidget', formEnctype') <- generateFormPost propForm   -- my own form
    defaultLayout $ do
        setTitle "logicstuff | truth trees"
        $(widgetFile "trees") 

postTreesR :: Handler Html
postTreesR = do
    ((result', formWidget'), formEnctype') <- runFormPost propForm
    let submission' = case result' of
            FormSuccess res -> Just res
            _ -> Nothing
    defaultLayout $ do
            let mytreehtml = case submission' of
                    Nothing -> "" 
                    Just (PropForm prop) -> prop
            mytree <- liftIO (treeformHTML mytreehtml)
            setTitle "logicstuff | truth trees"
            $(widgetFile "treesresult")