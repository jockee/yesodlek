module Handler.Home (module Handler.Home) where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import qualified Userx
import qualified Company
import qualified Data.Text as T
import Userx (Userx)
import Company ()
import Database.SQLite.Simple
import Network.HTTP.Simple
import qualified Control.Exception as CE
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    liftIO someFunc
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe FileForm
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Choose a file"
    <*> areq textField textSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "What's on the file?"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "File description")
                ]
            }

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")

toUsers :: L8.ByteString -> [Userx]
toUsers xs = incUids dec
  where
    dec = decode xs :: Maybe [Userx]

loadJSON :: L8.ByteString -> String
loadJSON xs = L8.unpack . encode $ incUids dec
  where
    dec = decode xs :: Maybe [Userx]

incUids :: Maybe [Userx] -> [Userx]
incUids Nothing = []
incUids (Just users) = incOrDec <$> users
  where
    incOrDec user = incDec user . shouldInc $ T.uncons (Userx.name user)
    shouldInc (Just (x, _)) = x == 'C'
    shouldInc Nothing = False
    incDec user True = user { Userx.uid = Userx.uid user - 9 }
    incDec user False = user { Userx.uid = Userx.uid user + 9 }

insertUserxs :: Connection -> [Userx] -> IO ()
insertUserxs conn = mapM_ insertUserx
    where
      insertUserx user = execute conn "INSERT INTO users (id, name, company_name, company_catchphrase) VALUES (?, ?, ?, ?)" (Userx.uid user, Userx.name user, cN user, cF user)
      cN user = Company.name $ Userx.company user
      cF user = Company.catchPhrase $ Userx.company user

someFunc :: IO ()
someFunc = do
  rsp <- httpLBS "http://jsonplaceholder.typicode.com/users"
  conn <- open "db/test.db"
  didInsert <- CE.try $ insertUserxs conn . toUsers $ getResponseBody rsp :: IO (Either SQLError ())
  case didInsert of
    Left ex -> putStrLn . T.pack $ "Caught exception: " ++ show ex
    Right _ -> print ("ok" :: Text)
  r <- query_ conn "SELECT * from users" :: IO [Userx]
  mapM_ print r
  close conn
