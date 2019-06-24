{-# LANGUAGE OverloadedStrings #-}
module Level02.Core (runApp, app) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)

import           Level02.Types            (ContentType(CTPlainText, CTJson), Error(NotFoundError), RqType(AddRq, ViewRq, ListRq),
                                           mkCommentText, mkTopic, mkErrorPrintout, getTopic, getCommentText,
                                           renderContentType)

-- |-------------------------------------------|
-- |- Don't start here, go to Level02.Types!  -|
-- |-------------------------------------------|

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse status ct = responseLBS status [("Content-Type", renderContentType ct)]

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 = mkResponse status200

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 = mkResponse status404

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 = mkResponse status400

-- |----------------------------------------------------------------------------------
-- These next few functions will take raw request information and construct         --
-- one of our types.                                                                --
--                                                                                  --
-- By breaking out these smaller functions, we're able to isolate our               --
-- validation requirements into smaller components that are simpler to maintain     --
-- and verify. It also allows for greater reuse and it also means that              --
-- validation is not duplicated across the application, maybe incorrectly.          --
--------------------------------------------------------------------------------------

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest t lbs = do
  topic <- mkTopic t
  comment <- mkCommentText $ lazyByteStringToStrictText lbs
  Right $ AddRq topic comment
  where
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText =
      decodeUtf8 . LBS.toStrict

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest t = ViewRq <$> (mkTopic t)

mkListRequest
  :: Either Error RqType
mkListRequest = Right ListRq

-- |----------------------------------
-- end of RqType creation functions --
--------------------------------------

encodeText :: Text -> LBS.ByteString
encodeText = LBS.fromStrict . encodeUtf8

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse = (resp400 CTPlainText) . encodeText . mkErrorPrintout

-- | Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest req
  | (length reqPath) == 1 && (reqPath !! 0) == "list" && reqMethod == "GET"  = return mkListRequest
  | (length reqPath) == 2 && (reqPath !! 1) == "view"  && reqMethod == "GET" = return $ mkViewRequest $ reqPath !! 0
  | (length reqPath) == 2 && (reqPath !! 1) == "add"  && reqMethod == "POST" = do
    let topic = reqPath !! 0
    comment <- strictRequestBody req
    return $ mkAddRequest topic comment
  | otherwise = return $ Left NotFoundError
  where reqMethod = requestMethod req
        reqPath = pathInfo req


-- | If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleValidRequest
  :: RqType
  -> Either Error Response
handleValidRequest ListRq                = Right $ resp200 CTPlainText "Topic list not implemented yet"
handleValidRequest (ViewRq topic)        = Right $ resp200 CTPlainText $ encodeText $ "Viewing topic " <> (getTopic topic) <> " not implemented yet"
handleValidRequest (AddRq topic comment) = Right $ resp200 CTPlainText $ encodeText $ "Adding a comment to topic " <> (getTopic topic) <> " not implemented yet:\n" <> (getCommentText comment)

handleRequest :: Request -> IO Response
handleRequest req = do
  parsedReq <- mkRequest req
  case parsedReq of
    Left err -> return $ mkErrorResponse err
    Right validReq -> return $ case handleValidRequest validReq of
      Left err -> mkErrorResponse err
      Right resp -> resp

-- | Reimplement this function using the new functions and ``RqType`` constructors as a guide.
app
  :: Application
app req cb = do
  resp <- handleRequest req
  cb resp

runApp :: IO ()
runApp = run 3000 app
