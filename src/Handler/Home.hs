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
import ClassyPrelude.Yesod (checkBoxField, FieldSettings (FieldSettings))
import ClassyPrelude (Bool(True))




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


-- | here's is how we do forms in yesod

data PropForm = PropForm   -- my own thingy for getting propositions
    { propInput :: Text
    , isArg :: Bool
    }

propForm :: Form PropForm
propForm = renderBootstrap3 BootstrapBasicForm $ PropForm
    <$> areq textField textSettings Nothing
    <*> areq checkBoxField boxSettings Nothing
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
          boxSettings = FieldSettings
            { fsLabel = "Is the input an argument? "
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs = []
            }

-- let's give my form a page of its own

getPLTreesR :: Handler Html
getPLTreesR = do
    (formWidget', formEnctype') <- generateFormPost propForm   -- my own form
    defaultLayout $ do
        setTitle "logicstuff | truth trees"
        $(widgetFile "trees") 

postPLTreesR :: Handler Html
postPLTreesR = do
    ((result', formWidget'), formEnctype') <- runFormPost propForm
    let submission' = case result' of
            FormSuccess res -> Just res
            _ -> Nothing
    defaultLayout $ do
            let mytreehtml = case submission' of
                    Nothing -> "" 
                    Just (PropForm prop _) -> prop
            let arg = case submission' of
                      Nothing -> False
                      Just (PropForm _ True) -> True
                      Just (PropForm _ False) -> False
            if arg 
                then do
                mytree <- liftIO (treeformHTMLa mytreehtml)
                setTitle "logicstuff | truth trees"
                $(widgetFile "treesresult")
                else do
                mytree <- liftIO (treeformHTML mytreehtml)
                setTitle "logicstuff | truth trees"
                $(widgetFile "treesresult")