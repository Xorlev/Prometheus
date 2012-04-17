module UrlExtender (expandUrlEasy) where

import Network.HTTP
import System.IO
import Data.Maybe
import Network.URI
import Text.XML.Light

expandUrlEasy :: String -> IO (String)
expandUrlEasy url =
        do resp <- expandUrl url
           case resp of
                Left x -> return $ "Error connecting: " ++ show x
                Right x -> return $ url ++ " -> " ++ longurl
                        where
                                xml = fromJust $ parseXMLDoc x
                                items = findElements (QName "long-url" Nothing Nothing) xml
                                longurl = (strContent (head items))

expandUrl :: String -> IO (Either String String)
expandUrl url =
    do resp <- simpleHTTP request
       case resp of
            Left x -> return $ Left ("Error connecting: " ++ show x)
            Right r ->
                  case rspCode r of
                    (2,_,_) -> return $ Right (rspBody r)
                    _ -> return $ Left (show r)
    where 
        request = Request {rqURI = uri,
                            rqMethod = GET,
                            rqHeaders = [],
                            rqBody = ""}
        uri = fromJust $ parseURI $ "http://api.longurl.org/v2/expand?url=" ++ (urlEncode url)