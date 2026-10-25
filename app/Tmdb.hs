{-# LANGUAGE OverloadedStrings #-}
module Tmdb
  ( Movie(..), Person(..), Genre(..), Collection(..), Details(..)
  , trendingWeek, popularAround, genres
  , searchMovies, searchPeople, searchCollections
  , discoverGenre, collectionParts, movieDetails
  ) where

import Control.Exception (SomeException, catch)
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar (addDays, fromGregorian, showGregorian)
import Network.HTTP.Simple
import System.Environment (lookupEnv)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Movie = Movie
  { movieId       :: Int
  , movieTitle    :: String
  , movieYear     :: Int       -- ^ 0 when TMDB has no release date
  , movieOverview :: String
  , movieVote     :: Double    -- ^ TMDB average, 0-10
  } deriving (Eq, Show)

data Person = Person
  { personId    :: Int
  , personName  :: String
  , personDept  :: String      -- ^ "Acting", "Directing", ...
  , personFilms :: [Movie]     -- ^ TMDB's "known for" films
  } deriving (Eq, Show)

data Genre = Genre
  { genreId   :: Int
  , genreName :: String
  } deriving (Eq, Show)

data Collection = Collection
  { collId   :: Int
  , collName :: String
  } deriving (Eq, Show)

-- | Extra information fetched when a film is opened.
data Details = Details
  { dRuntime   :: Int          -- ^ minutes, 0 if unknown
  , dTagline   :: String
  , dGenres    :: [String]
  , dDirectors :: [String]
  , dCast      :: [String]
  } deriving (Eq, Show)

-------------------------------------------------------------------------------
-- JSON
-------------------------------------------------------------------------------

instance FromJSON Movie where
  parseJSON = withObject "Movie" $ \o -> do
    i   <- o .: "id"
    t   <- o .:? "title"        .!= ""
    rel <- o .:? "release_date" .!= ""
    ov  <- o .:? "overview"     .!= ""
    v   <- o .:? "vote_average" .!= 0
    return (Movie i t (yearOf rel) ov v)

yearOf :: String -> Int
yearOf s = case reads (take 4 s) of
  [(y, "")] -> y
  _         -> 0

instance FromJSON Person where
  parseJSON = withObject "Person" $ \o -> do
    i  <- o .: "id"
    n  <- o .:? "name" .!= ""
    d  <- o .:? "known_for_department" .!= ""
    ks <- o .:? "known_for" .!= []
    return (Person i n d (mapMaybe knownMovie ks))

-- | "known_for" mixes films and TV shows; keep only the films.
knownMovie :: Value -> Maybe Movie
knownMovie = parseMaybe $ withObject "known_for" $ \o -> do
  kind <- o .:? "media_type" .!= ("" :: String)
  if kind == "movie" then parseJSON (Object o) else fail "not a film"

instance FromJSON Genre where
  parseJSON = withObject "Genre" $ \o -> Genre <$> o .: "id" <*> o .:? "name" .!= ""

instance FromJSON Collection where
  parseJSON = withObject "Collection" $ \o ->
    Collection <$> o .: "id" <*> o .:? "name" .!= ""

instance FromJSON Details where
  parseJSON = withObject "Details" $ \o -> do
    rt <- o .:? "runtime" .!= 0
    tg <- o .:? "tagline" .!= ""
    gs <- o .:? "genres"  .!= ([] :: [Genre])
    cr <- o .:? "credits"
    let (dirs, cast) = maybe ([], []) creditNames cr
    return (Details rt tg (map genreName gs) dirs cast)

creditNames :: Value -> ([String], [String])
creditNames v = (directors, take 5 cast)
  where
    crew, cast' :: [Value]
    crew  = fromMaybe [] (parseMaybe (withObject "credits" (.: "crew")) v)
    cast' = fromMaybe [] (parseMaybe (withObject "credits" (.: "cast")) v)
    directors = [ n | c <- crew, Just ("Director", n) <- [parseMaybe jobAndName c] ]
    cast      = mapMaybe (parseMaybe castName) cast'

    jobAndName :: Value -> Parser (String, String)
    jobAndName = withObject "crew" $ \o -> (,) <$> o .: "job" <*> o .: "name"

    castName :: Value -> Parser String
    castName = withObject "cast" (.: "name")

-- Envelopes TMDB wraps its lists in.
newtype Results a = Results [a]
instance FromJSON a => FromJSON (Results a) where
  parseJSON = withObject "Results" $ \o -> Results <$> o .: "results"

newtype Parts = Parts [Movie]
instance FromJSON Parts where
  parseJSON = withObject "Parts" $ \o -> Parts <$> o .: "parts"

newtype GenreList = GenreList [Genre]
instance FromJSON GenreList where
  parseJSON = withObject "GenreList" $ \o -> GenreList <$> o .: "genres"

-------------------------------------------------------------------------------
-- HTTP
-------------------------------------------------------------------------------
apiKey :: String
apiKey = "6c40d87f51f3d4e3ebcc28122a58b481"

getJson :: FromJSON a => String -> [(String, String)] -> IO (Either String a)
getJson path params = request apiKey `catch` onError
  where
    request key = do
      req0 <- parseRequest ("https://api.themoviedb.org/3" ++ path)
      let bearer = length key > 40          -- v4 tokens are long, v3 keys are 32
          query  = [ (enc k, Just (enc v))
                   | (k, v) <- params ++ [ ("api_key", key) | not bearer ] ]
          req1   = setRequestQueryString query req0
          req2   | bearer    = addRequestHeader "Authorization"
                                 (BS.pack ("Bearer " ++ key)) req1
                 | otherwise = req1
      resp <- httpLBS req2
      case getResponseStatusCode resp of
        200  -> return (eitherDecode (getResponseBody resp))
        401  -> return (Left "HTTP 401 - TMDB rejected the API key")
        code -> return (Left ("HTTP " ++ show code))

    onError :: SomeException -> IO (Either String a)
    onError e = return (Left (show e))

    enc = TE.encodeUtf8 . Text.pack

list :: FromJSON a => String -> [(String, String)] -> IO (Either String [a])
list path params = fmap (fmap unwrap) (getJson path params)
  where unwrap (Results xs) = xs

-------------------------------------------------------------------------------
-- Endpoints
-------------------------------------------------------------------------------

-- | "Most popular this week"
trendingWeek :: IO (Either String [Movie])
trendingWeek = list "/trending/movie/week" []

-- | "Most popular on <date>": TMDB has no per-day popularity history, so this
-- is the most-voted films released in the 60 days up to that date, i.e. what
-- was in cinemas at the time.
popularAround :: (Integer, Int, Int) -> IO (Either String [Movie])
popularAround (y, m, d) =
  list "/discover/movie"
    [ ("primary_release_date.gte", showGregorian (addDays (-60) day))
    , ("primary_release_date.lte", showGregorian day)
    , ("sort_by", "vote_count.desc")
    , ("include_adult", "false") ]
  where day = fromGregorian y m d

genres :: IO (Either String [Genre])
genres = fmap (fmap unwrap) (getJson "/genre/movie/list" [])
  where unwrap (GenreList gs) = gs

searchMovies :: String -> IO (Either String [Movie])
searchMovies q = list "/search/movie" [("query", q), ("include_adult", "false")]

searchPeople :: String -> IO (Either String [Person])
searchPeople q = list "/search/person" [("query", q), ("include_adult", "false")]

-- | TMDB "collections" (film series) stand in for Letterboxd lists.
searchCollections :: String -> IO (Either String [Collection])
searchCollections q = list "/search/collection" [("query", q)]

discoverGenre :: Int -> IO (Either String [Movie])
discoverGenre gid =
  list "/discover/movie"
    [ ("with_genres", show gid), ("sort_by", "popularity.desc")
    , ("vote_count.gte", "100"), ("include_adult", "false") ]

collectionParts :: Int -> IO (Either String [Movie])
collectionParts cid = fmap (fmap unwrap) (getJson ("/collection/" ++ show cid) [])
  where unwrap (Parts ms) = ms

movieDetails :: Int -> IO (Either String Details)
movieDetails mid = getJson ("/movie/" ++ show mid) [("append_to_response", "credits")]
