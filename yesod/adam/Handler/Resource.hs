{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Resource where

import Import


--getTodoListR :: Handler RepHtml
--getTodoListR = undefined

data Resource' = Resource' { url' :: Text
                           , title' :: Text
                           , content' :: Textarea
                           }

extendResource' :: UserId -> Resource' -> Resource
extendResource' author (Resource' u t c) = Resource author u t c

addResourceForm :: Form Resource'
addResourceForm = renderBootstrap $ Resource'
                <$> areq textField "Slug" Nothing
                <*> areq textField "Title" Nothing
                <*> areq textareaField "Content" Nothing

isAdmin :: Entity User -> Bool
isAdmin u = userAdmin $ entityVal u

getRootR :: Handler RepHtml
getRootR = do
         ma <- maybeAuth
         resources <- runDB $ selectList [][Asc ResourceTitle]

         (formWidget, enctype) <-
                           generateFormPost addResourceForm

         defaultLayout $(widgetFile "root")

postRootR :: Handler ()
postRootR = do
          authId <- requireAuthId

          ((result, _), _) <- runFormPost $ addResourceForm

          _ <- case result of
            FormSuccess r' -> do
                        let r = extendResource' authId r'
                        _ <-runDB $ insert r
                        setMessage "Success."
                        --redirect ResourceR $ resourceUrl r
            _ -> setMessage "Error."

          redirect RootR

getResourceR :: Text -> Handler RepHtml
getResourceR url = do
             resourceEntity <-runDB $ getBy404 $ UniqueUrl url
             let resource = entityVal resourceEntity

             defaultLayout $(widgetFile "resource")