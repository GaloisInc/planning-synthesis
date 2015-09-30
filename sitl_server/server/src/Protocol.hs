{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Protocol where

import           Data.Aeson
                     (ToJSON(..),FromJSON(..),encode,decode,(.:),withObject
                     ,object,(.=),Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Numeric (readSigned,readFloat,showFloat,showFFloat)
import           System.IO (Handle)


-- Utility Types ---------------------------------------------------------------

data Position = Position { posLat, posLon, posAlt :: !Double }
                deriving (Show,Read,Generic)

instance ToJSON Position where
  toJSON Position { .. } =
    object [ "lat" .= posLat
           , "lon" .= posLon
           , "alt" .= posAlt ]

instance FromJSON Position where
  parseJSON = withObject "Position" $ \ obj ->
    do posLat <- obj .: "lat"
       posLon <- obj .: "lon"
       posAlt <- obj .: "alt"
       return Position { .. }

readPosition :: ReadS Position
readPosition str1 =
  do (posLat,',':str2) <- readSigned readFloat str1
     (posLon,',':str3) <- readSigned readFloat str2
     (posAlt,rest)     <- readSigned readFloat str3
     return (Position { .. },rest)

showPosition :: Position -> ShowS
showPosition Position { .. } = showFloat posLat
                             . showChar ','
                             . showFloat posLon
                             . showChar ','
                             . showFloat posAlt

portland :: Position
portland  = Position { posLat = 45.5975711595468
                     , posLon = -122.6885446999222
                     , posAlt = 5.587
                     }


data Wind = Wind { windSpeed, windDir, windTurb :: !Double }
            deriving (Show,Generic)

readWind :: ReadS Wind
readWind str1 =
  do (windSpeed,',':str2) <- readFloat str1
     (windDir,  ',':str3) <- readFloat str2
     (windTurb, ',':str4) <- readFloat str3
     return (Wind { .. },str4)

showWind :: Wind -> ShowS
showWind Wind { .. } = showFloat windSpeed
                     . showChar ','
                     . showFloat windDir
                     . showChar ','
                     . showFloat windTurb

noWind :: Wind
noWind  = Wind { windSpeed = 0, windDir = 0, windTurb = 0 }


-- | The direction of the aircraft in degrees, with 0 being North.
type Heading = Double

readHome :: ReadS (Position,Heading)
readHome str1 =
  do (pos,',':str2) <- readPosition str1
     (heading,rest) <- readFloat str2
     return ((pos,heading),rest)

showHome :: (Position,Heading) -> ShowS
showHome (pos, heading) = showPosition pos
                        . showChar ','
                        . showFFloat (Just 0) heading


north, east, south, west :: Heading
north = 0
east  = 90
south = 180
west  = 270


type Waypoint = Position


-- Requests --------------------------------------------------------------------

-- | SITL instance identifiers.
data Instance = Controlled
              | Obstacle String Int
                deriving (Typeable,Show,Eq,Ord)

instance ToJSON Instance where
  toJSON inst = object [ "name" .= instanceName inst
                       , "id"   .= instanceId   inst ]

instanceName :: Instance -> String
instanceName Controlled     = "controlled"
instanceName (Obstacle n _) = "obstacle-" ++ n

instanceId :: Instance -> Int
instanceId Controlled     = 0
instanceId (Obstacle _ i) = i + 1


data Request = MoveTo !Waypoint
             | GetPositions
             | GetStatus
             | StartObstacles
               deriving (Show,Generic)

instance FromJSON Request where
  parseJSON = withObject "Request" $ \ obj ->
    do method <- obj .: "method"
       case method :: String of
         "move-to"         -> MoveTo `fmap` (obj .: "params")
         "positions"       -> return GetPositions
         "status"          -> return GetStatus
         "start-obstacles" -> return StartObstacles
         _                 -> fail "Unable to parse request"

-- | Read a request off of the wire.  Fails when the request is invalid, or the
getRequest :: Handle -> IO Request
getRequest h =
  do bytes <- getPayload h
     case decode bytes of
       Just req -> return req
       Nothing  -> fail ("Invalid request: " ++ show bytes)


-- Responses -------------------------------------------------------------------

data Response = Ack
                -- ^ The request has been accepted
              | Positions [(Instance,Position,Heading)]
                -- ^ All positions of known objects
              | Status Bool
                -- ^ The current status of the controlled quad copter
                deriving (Show,Generic)

instance ToJSON Response where
  toJSON Ack            = object [ "ack"       .= True                ]
  toJSON (Positions ps) = object [ "positions" .= map jsonPosition ps ]
  toJSON (Status armed) = object [ "armed"     .= armed               ]

jsonPosition :: (Instance,Position,Heading) -> Value
jsonPosition (inst,pos,hdg) =
  object [ "instance" .= inst
         , "position" .= pos
         , "heading"  .= hdg ]

respondResult :: Response -> Value
respondResult val = object [ "result" .= toJSON val ]

respondError :: String -> Value
respondError val = object [ "error" .= toJSON val ]

putResponse :: Handle -> Value -> IO ()
putResponse h resp = putPayload h (encode resp)


-- Wire Protocol ---------------------------------------------------------------

-- The protocol is extremely simple, and text-based: newlines delimit requests,
-- which are json-encoded.
getPayload :: Handle -> IO L8.ByteString
getPayload client =
     L8.fromStrict `fmap` S8.hGetLine client

-- | Send some bytes down to the client.
putPayload :: Handle -> L8.ByteString -> IO ()
putPayload client bytes =
     L8.hPutStrLn client bytes
