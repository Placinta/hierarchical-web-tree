
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies  #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell, GADTs, QuasiQuotes, ScopedTypeVariables, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -w -fno-warn-unused-imports -fno-warn-type-defaults #-}
module TreeController(runApp) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Char (toLower)

import Happstack.Server (nullConf, simpleHTTP, toResponse, ok, dir, path, serveDirectory, Browsing(..),
  FromReqURI(..), Method(GET, POST), method, ServerPart, ServerPartT, Response)
import           Text.Blaze ((!))
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import qualified Text.Blaze as B 
import qualified Text.Blaze.Internal as B 

import Database.Persist.Sqlite
import Data.Pool (Pool)
import GHC.Int (Int64)
import TreeModel

connectionCount = 2

runApp :: IO ()
runApp = 
  withSqlitePool "myhaskdb.db" connectionCount $ \pool -> do
    runDB pool $ do
        setupDB
        deleteFromTreeTable
        insertTestData
    simpleHTTP nullConf (runReaderT appRouter pool)

appRouter :: ReaderT ConnectionPool (ServerPartT IO) Response
appRouter =
  msum [ 
    dir "add" $ path $ \pid -> insertNewNode pid,
    dir "delete" $ path $ \nid -> deleteSelectedNodeAndChildren nid,
    dir "static" $ serveDirectory EnableBrowsing  ["index.html"] "../static",
    homepage
    ] 

runDB :: MonadBaseControl IO m => Pool Connection -> SqlPersistT (NoLoggingT (ResourceT m)) a -> m a
runDB pool action = runResourceT . runNoLoggingT $ runSqlPool action pool 

insertNewNode :: Int64 -> ReaderT ConnectionPool (ServerPartT IO) Response
insertNewNode pid = do
    pool <- ask
    tree <- liftIO $ do 
      runDB pool $ insertIntoTreeTable "additional" 10 pid
      runDB pool getTreeFromDB
    makeTemplateResponse tree

deleteSelectedNodeAndChildren :: Int -> ReaderT ConnectionPool (ServerPartT IO) Response
deleteSelectedNodeAndChildren nid = do
    pool <- ask
    tree <- liftIO $ do
      when (nid /= 1) $ do
        old_tree <- runDB pool getTreeFromDB
        runDB pool $ deleteNodeFromTreeTable nid old_tree
      runDB pool getTreeFromDB
    makeTemplateResponse tree

homepage :: ReaderT ConnectionPool (ServerPartT IO) Response
homepage = do
    pool <- ask
    tree <- liftIO $ runDB pool getTreeFromDB
    makeTemplateResponse tree

--makeTemplateResponse :: (Show a) => Tree a -> ServerPart Response
makeTemplateResponse tree = ok $ toResponse $
                            homePageTemplate "Hello, HappS!"
                            [H.meta ! A.name "keywords" ! A.content "happstack, persistent, blaze, html, esqueleto, persistent, sqlite"] 
                            (buildTreeOutput tree)

buildTreeOutput :: Mytree -> B.MarkupM ()
buildTreeOutput EmptyTree = return ()
buildTreeOutput (Node (TreeInfo text nid _) children) = do
  H.ul ! A.class_ "list-group" $ do
    H.li ! A.class_ "list-group-item" $ do 
      H.div $ do
        H.toHtml (text ++ " ")
        H.a ! A.href (B.toValue $ "/add/" ++ (show nid) ) $ do
          H.span ! A.class_ "glyphicon glyphicon-plus-sign" $ "" 
        H.span $ " "
        case nid of
          1 -> return ()
          otherwise -> H.a ! A.href (B.toValue $ "/delete/" ++ (show nid) ) $ do
             H.span ! A.class_ "glyphicon glyphicon-minus-sign" $ "" 
      mapM_ (\child -> buildTreeOutput child) children

homePageTemplate :: String -> [H.Html] -> H.Html -> H.Html
homePageTemplate title headers body =
    H.docTypeHtml $ do
      H.head $ do
        H.title (H.toHtml title)
        H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
        H.link ! A.href "/static/css/bootstrap.min.css" ! A.type_ "text/css" ! A.rel "stylesheet" ! A.media "screen"
        H.link ! A.href "/static/css/bootstrap-glyphicons.css" ! A.type_ "text/css" ! A.rel "stylesheet" ! A.media "screen"
        H.link ! A.href "/static/css/custom.css" ! A.type_ "text/css" ! A.rel "stylesheet" ! A.media "screen"
        H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
        H.script "" ! A.src "http://code.jquery.com/jquery.js"
        H.script "" ! A.src "/static/js/bootstrap.min.js" ! A.type_ "text/javascript" 
        sequence_ headers
      H.body $ do
        H.div ! A.class_ "navbar navbar-inverse navbar-fixed-top" $ do
          H.div ! A.class_ "container" $ do
            H.a ! A.href "#" ! A.class_ "navbar-brand" $ H.toHtml title
            H.div ! A.class_ "nav-collapse collapse" $ do
              H.ul ! A.class_ "nav navbar-nav" $ do
                H.li ! A.class_ "active" $ do
                  H.a ! A.href "#" $ "Home"
        H.div ! A.class_ "container" $ do
          H.div ! A.class_ "body-content" $ do
            H.h2 "Friendly hierarchical tree"
            body
        return ()