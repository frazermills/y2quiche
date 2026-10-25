module Main (main) where

import Fudgets
import Data.Char (isSpace, toLower, toUpper)
import Numeric (showFFloat)
import System.IO.Unsafe (unsafePerformIO)

import Tmdb (Movie(..), Person(..), Genre(..), Collection(..), Details(..))
import qualified Tmdb as T

-------------------------------------------------------------------------------
-- Entry point
-------------------------------------------------------------------------------

main :: IO ()
main = do
  st0 <- loadInitialState
  fudlogue (shellF "Letterboxd 2000 - Film Diary & Reviews" (appF st0))

loadInitialState :: IO AppState
loadInitialState = do
  wk <- T.trendingWeek
  rt <- T.popularAround retroDate
  gs <- T.genres
  let problems = [ e | Left e <- [ () <$ wk, () <$ rt, () <$ gs ] ]
      msg = case problems of
              []      -> "Welcome to Letterboxd 2000. Pick a poster, or try (?) SEARCH."
              (e : _) -> "TMDB error: " ++ e ++ "   (is TMDB_API_KEY set?)"
  return AppState
    { activeTab     = HomeTab
    , viewVer       = 0
    , weekFilms     = either (const []) id wk
    , retroFilms    = either (const []) id rt
    , openSection   = Nothing
    , genreList     = either (const []) id gs
    , searchScope   = ScFilms
    , lastQuery     = ""
    , searchHits    = []
    , hitsTitle     = "RESULTS"
    , selectedMovie = Nothing
    , movieInfo     = Nothing
    , currentRating = 3
    , currentReview = ""
    , watchlist     = []
    , loggedFilms   = []
    , statusMsg     = msg
    }

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data Review = Review
  { revMovie  :: Movie
  , revRating :: Int       -- 1 to 5 stars
  , revText   :: String
  }

-- | The pages of the site (the nav bar in the sketches, plus a film page)
data Tab = HomeTab | SearchTab | FilmTab | LogTab | DiaryTab | WatchlistTab
  deriving (Eq, Show)

-- | The two shelves on the home page, only one can be expanded at a time
data Section = SecWeek | SecRetro
  deriving (Eq, Show)

-- | The six filter buttons on the search page
data Scope = ScFilms | ScCast | ScCrew | ScGenres | ScLists | ScMembers
  deriving (Eq, Show)

-- | Anything that can turn up as a "poster" in a result grid
data SearchHit
  = HFilm Movie
  | HPerson Person
  | HGenre Genre
  | HColl Collection

data AppState = AppState
  { activeTab     :: Tab
  , viewVer       :: Int              -- bumped when a page needs rebuilding
  , weekFilms     :: [Movie]
  , retroFilms    :: [Movie]
  , openSection   :: Maybe Section
  , genreList     :: [Genre]
  , searchScope   :: Scope
  , lastQuery     :: String
  , searchHits    :: [SearchHit]
  , hitsTitle     :: String
  , selectedMovie :: Maybe Movie
  , movieInfo     :: Maybe Details
  , currentRating :: Int
  , currentReview :: String
  , watchlist     :: [Movie]
  , loggedFilms   :: [Review]
  , statusMsg     :: String
  }

retroDate :: (Integer, Int, Int)
retroDate = (2003, 10, 25)

retroLabel :: String
retroLabel = let (y, m, d) = retroDate in show d ++ "/" ++ show m ++ "/" ++ show y

-------------------------------------------------------------------------------
-- Actions & state transitions
-------------------------------------------------------------------------------

data UserAction
  = GoTab Tab
  | ToggleSection Section
  | SetScope Scope
  | SubmitSearch String
  | OpenHit SearchHit
  | LogSearch String
  | UpdateRating Int
  | UpdateReviewText String
  | ClickLogMovie
  | ClickAddToWatchlist

-- | Run a TMDB request lazily, the first time its result is looked at.
lazyIO :: IO a -> a
lazyIO = unsafePerformIO
{-# NOINLINE lazyIO #-}

-- | State transition function for the film diary stream processor
stepApp :: AppState -> UserAction -> (AppState, AppState)
stepApp st act = (st', st') where st' = stepState st act

bumpView :: AppState -> AppState
bumpView st = st { viewVer = viewVer st + 1 }

stepState :: AppState -> UserAction -> AppState
stepState st act = case act of
  GoTab t -> st { activeTab = t }

  ToggleSection s ->
    bumpView st { openSection = if openSection st == Just s then Nothing else Just s }

  SetScope sc    -> scopeChanged st { searchScope = sc }
  SubmitSearch q -> scopeChanged st { lastQuery = q }
  OpenHit h      -> openHit h st
  LogSearch q    -> logSearch q st

  UpdateRating n ->
    st { currentRating = n, statusMsg = "Rating set to " ++ show n ++ "/5." }

  UpdateReviewText txt ->
    st { currentReview = txt, statusMsg = "Review saved. Press LOG A FILM to add it to your diary." }

  ClickLogMovie ->
    case selectedMovie st of
      Just m ->
        st { loggedFilms   = Review m (currentRating st) (currentReview st) : loggedFilms st
           , currentRating = 3
           , currentReview = ""
           , activeTab     = DiaryTab
           , statusMsg     = "Logged " ++ filmName m ++ " to your diary." }
      Nothing -> st { statusMsg = "Pick a film first: type its title above and press Enter." }

  ClickAddToWatchlist ->
    case selectedMovie st of
      Just m
        | movieId m `elem` map movieId (watchlist st) ->
            st { statusMsg = filmName m ++ " is already on your watchlist." }
        | otherwise ->
            st { watchlist = m : watchlist st
               , activeTab = WatchlistTab
               , statusMsg = "Added " ++ filmName m ++ " to your watchlist." }
      Nothing -> st { statusMsg = "Pick a film first." }

-- | A filter button was pressed, or a search was submitted
scopeChanged :: AppState -> AppState
scopeChanged st = case searchScope st of
  ScGenres ->
    bumpView st { searchHits  = [ HGenre g | g <- genreList st
                                           , null q || containsCI q (genreName g) ]
                , hitsTitle   = "GENRES"
                , statusMsg   = "Pick a genre." }
  ScMembers ->
    bumpView st { searchHits  = []
                , hitsTitle   = "MEMBERS"
                , statusMsg   = "Members are not available: TMDB has no member data." }
  sc | null q ->
    bumpView st { searchHits  = []
                , hitsTitle   = scopeName sc
                , statusMsg   = "Searching in " ++ scopeName sc ++ ". Type a term and press Enter." }
     | otherwise -> runSearch sc q st
  where q = trimSpaces (lastQuery st)

runSearch :: Scope -> String -> AppState -> AppState
runSearch sc q st = bumpView st
  { searchHits = hits
  , hitsTitle  = "RESULTS FOR \"" ++ q ++ "\" IN " ++ scopeName sc
  , statusMsg  = either ("TMDB error: " ++) (\xs -> show (length xs) ++ " results.") result
  }
  where
    result = lazyIO (searchFor sc q)
    hits   = either (const []) (take 10) result

searchFor :: Scope -> String -> IO (Either String [SearchHit])
searchFor ScFilms q = fmap (fmap (map HFilm))   (T.searchMovies q)
searchFor ScCast  q = fmap (fmap (map HPerson . filter isActor))       (T.searchPeople q)
searchFor ScCrew  q = fmap (fmap (map HPerson . filter (not . isActor))) (T.searchPeople q)
searchFor ScLists q = fmap (fmap (map HColl))   (T.searchCollections q)
searchFor _       _ = return (Right [])

isActor :: Person -> Bool
isActor p = personDept p == "Acting"

-- | A poster was clicked
openHit :: SearchHit -> AppState -> AppState
openHit (HFilm m) st = openFilm m st
openHit (HPerson p) st = bumpView st
  { searchHits = map HFilm (take 10 (personFilms p))
  , hitsTitle  = "KNOWN FOR - " ++ upperStr (personName p)
  , statusMsg  = personName p ++ " (" ++ personDept p ++ ")" }
openHit (HGenre g) st =
  showFilms ("GENRE - " ++ upperStr (genreName g)) (lazyIO (T.discoverGenre (genreId g))) st
openHit (HColl c) st =
  showFilms ("COLLECTION - " ++ upperStr (collName c)) (lazyIO (T.collectionParts (collId c))) st

showFilms :: String -> Either String [Movie] -> AppState -> AppState
showFilms title result st = bumpView st
  { searchHits = either (const []) (map HFilm . take 10) result
  , hitsTitle  = title
  , statusMsg  = either ("TMDB error: " ++) (const title) result }

openFilm :: Movie -> AppState -> AppState
openFilm m st = bumpView st
  { activeTab     = FilmTab
  , selectedMovie = Just m
  , movieInfo     = either (const Nothing) Just (lazyIO (T.movieDetails (movieId m)))
  , statusMsg     = "Opened " ++ filmName m ++ "." }

-- | Look a title up on the Log page and select the best match
logSearch :: String -> AppState -> AppState
logSearch q st = case lazyIO (T.searchMovies q) of
  Right (m : _) -> st { selectedMovie = Just m
                      , movieInfo     = either (const Nothing) Just (lazyIO (T.movieDetails (movieId m)))
                      , statusMsg     = "Selected " ++ filmName m ++ "." }
  Right []      -> st { statusMsg = "No film found for \"" ++ q ++ "\"." }
  Left e        -> st { statusMsg = "TMDB error: " ++ e }

-------------------------------------------------------------------------------
-- Small pure helpers
-------------------------------------------------------------------------------

filmName :: Movie -> String
filmName m = movieTitle m ++ (if movieYear m == 0 then "" else " (" ++ show (movieYear m) ++ ")")

yearStr :: Movie -> String
yearStr m = if movieYear m == 0 then "----" else show (movieYear m)

stars :: Int -> String
stars n = "[" ++ replicate n '*' ++ replicate (5 - n) '-' ++ "]"

oneDp :: Double -> String
oneDp v = showFFloat (Just 1) v ""

scopeName :: Scope -> String
scopeName ScFilms   = "FILMS"
scopeName ScCast    = "CAST"
scopeName ScCrew    = "CREW"
scopeName ScGenres  = "GENRES"
scopeName ScLists   = "LISTS"
scopeName ScMembers = "MEMBERS"

upperStr :: String -> String
upperStr = map toUpper

trimSpaces :: String -> String
trimSpaces = dropWhile isSpace . reverse . dropWhile isSpace . reverse

joinWith :: String -> [String] -> String
joinWith _   []  = ""
joinWith sep xs  = foldr1 (\a b -> a ++ sep ++ b) xs

-- | Case-insensitive "needle occurs in haystack"
containsCI :: String -> String -> Bool
containsCI needle hay = any (startsWith (low needle)) (suffixes (low hay))
  where
    low = map toLower
    suffixes []         = [[]]
    suffixes s@(_ : t)  = s : suffixes t
    startsWith [] _             = True
    startsWith _ []             = False
    startsWith (a : xs) (b : ys) = a == b && startsWith xs ys

-- | Pad (or cut) a string to exactly n characters
fillTo :: Int -> String -> String
fillTo n s = take n (s ++ repeat ' ')

-- | Greedy word wrap
wrapWords :: Int -> String -> [String]
wrapWords w = map unwords . go [] 0 . concatMap breakLong . words
  where
    breakLong x | length x <= w = [x]
                | otherwise     = take w x : breakLong (drop w x)
    go cur _ [] = [ reverse cur | not (null cur) ]
    go cur len (x : xs)
      | null cur                   = go [x] (length x) xs
      | len + 1 + length x <= w    = go (x : cur) (len + 1 + length x) xs
      | otherwise                  = reverse cur : go [x] (length x) xs

-- | Wrap into exactly n lines of at most w characters, ending in ".." if cut
fitLines :: Int -> Int -> String -> [String]
fitLines n w s
  | length ls > n = take (n - 1) ls ++ [ take (w - 2) (ls !! (n - 1)) ++ ".." ]
  | otherwise     = ls ++ replicate (n - length ls) ""
  where ls = wrapWords w s

rowsOf :: Int -> [a] -> [[a]]
rowsOf _ [] = []
rowsOf n xs = take n xs : rowsOf n (drop n xs)

-------------------------------------------------------------------------------
-- trying to make it look like a web page from the year 2000
-------------------------------------------------------------------------------

colSilver, colNavy, colInk, colWhite, colYellow, colPhosphor, colBlack :: String
colSilver   = "#c0c0c0"
colNavy     = "#000080"
colInk      = "#000000"
colWhite    = "#ffffff"
colYellow   = "#ffff66"
colPhosphor = "#33ff33"
colBlack    = "#000000"

-- The three Letterboxd dots
colOrange, colGreen, colBlue :: String
colOrange = "#ff8000"
colGreen  = "#00e054"
colBlue   = "#40bcf4"

fontBody, fontBold, fontHeading, fontBanner, fontMonoBold :: String
fontBody     = "-*-helvetica-medium-r-*-*-24-*-*-*-*-*-*-*"
fontBold     = "-*-helvetica-bold-r-*-*-24-*-*-*-*-*-*-*"
fontHeading  = "-*-helvetica-bold-r-*-*-28-*-*-*-*-*-*-*"
fontBanner   = "-*-times-bold-r-*-*-48-*-*-*-*-*-*-*"
fontMonoBold = "-*-courier-bold-r-*-*-24-*-*-*-*-*-*-*"

-- | Number of characters the full-width text rows are sized for
rowWidth :: String
rowWidth = replicate 144 'x'

-- | Width of a poster card in characters
cardW :: Int
cardW = 28

-- | A fixed caption (a display that never changes), with its own look
captionF :: Customiser (DisplayF String) -> String -> F a b
captionF cust s = displayF' (cust . setInitDisp s . setInitSize s) >=^< const s

textStyle :: Customiser (DisplayF String)
textStyle = setFont fontBody . setFgColor colInk . setBgColor colSilver . setInitSize rowWidth

-- | A full-width line of plain text
plainLineF :: String -> F a b
plainLineF = captionF textStyle

-- | A navy title bar with white text
barF :: String -> F a b
barF = captionF (setFont fontHeading . setFgColor colWhite . setBgColor colNavy . setInitSize rowWidth)

-- | A full-width line of text computed from the state
liveF :: (AppState -> String) -> F AppState b
liveF f = displayF' textStyle >=^< f

-- | A button that ignores state updates and outputs a Click when pressed
btnF :: String -> String -> F a Click
btnF font label =
  buttonF' (setFont font . setFgColor colInk . setBgColor colSilver) label >=^^< nullSP

-------------------------------------------------------------------------------
-- Fudget UI
--
-- One big fudget  F AppState UserAction  holds every widget, so the layout
-- order is exactly the order written below.
--  loopF:   actions -> stateSP -> (state) -> widgets -> actions
-------------------------------------------------------------------------------

appF :: AppState -> F AppState AppState
appF st0 = loopF (absF (stateSP st0) >==< uiF st0)

-- | Stream processor maintaining application state using mapAccumlSP
stateSP :: AppState -> SP UserAction AppState
stateSP st = putSP st $ mapAccumlSP stepApp st

uiF :: AppState -> F AppState UserAction
uiF st0 = vBoxF (foldr1 (>*<) [ headerF, navF, pageHostF st0, statusF, footerF ])

-- | Logo and banner across the top
headerF :: F a b
headerF = hBoxF (foldr1 (>*<)
  [ dot colOrange, dot colGreen, dot colBlue
  , captionF (setFont fontBanner . setFgColor colWhite . setBgColor colNavy)
             " LETTERBOXD 2000 "
  , captionF (setFont fontBody . setFgColor colYellow . setBgColor colNavy)
             "  your purely functional film diary - best viewed at 800x600  " ])
  where dot c = captionF (setFont fontBanner . setFgColor c . setBgColor colNavy) "O"

-- | Green-screen status line
statusF :: F AppState b
statusF = displayF' (setFont fontMonoBold . setFgColor colPhosphor . setBgColor colBlack
                     . setInitSize rowWidth) >=^< statusMsg

footerF :: F a b
footerF = captionF (setFont fontBody . setFgColor colInk . setBgColor colSilver)
  "Let's put something cool in the footer"

-------------------------------------------------------------------------------
-- Navigation bar: avatar | search | + | (two spare cells) -> five tabs.
-------------------------------------------------------------------------------

navF :: F AppState UserAction
navF = hBoxF (foldr1 (>*<)
  [ navCell ":-) HOME"   HomeTab
  , navCell "(?) SEARCH" SearchTab
  , navCell "(+) LOG"    LogTab
  , navCell "DIARY"      DiaryTab
  , navCell "WATCHLIST"  WatchlistTab ])

navCell :: String -> Tab -> F AppState UserAction
navCell label t = vBoxF
  (   (const (GoTab t) >^=< btnF fontMonoBold (fillTo 12 label))
  >*< (displayF' hatchStyle >=^< (\st -> if navTab st == t then hatchOn else hatchOff)) )

-- | The film page counts as part of Search for highlighting
navTab :: AppState -> Tab
navTab st = if activeTab st == FilmTab then SearchTab else activeTab st

hatchOn, hatchOff :: String
hatchOn  = replicate 14 '/'
hatchOff = replicate 14 ' '

hatchStyle :: Customiser (DisplayF String)
hatchStyle = setFont fontMonoBold . setFgColor colNavy . setBgColor colSilver . setInitSize hatchOn

-------------------------------------------------------------------------------
-- Page switching
--
-- dynF swaps the page fudget whenever the active tab or the page's content
-- version changes. Buttons get their labels from the state at that moment.
-------------------------------------------------------------------------------

pageKey :: AppState -> (Tab, Int)
pageKey st = (activeTab st, viewVer st)

pageHostF :: AppState -> F AppState UserAction
pageHostF st0 = dynF (pageFor st0) >=^^< hostSP (pageKey st0)

hostSP :: (Tab, Int) -> SP AppState (Either (F AppState UserAction) AppState)
hostSP key = getSP $ \st ->
  let k = pageKey st
  in if k /= key
       then putSP (Left (pageFor st)) $ putSP (Right st) $ hostSP k
       else putSP (Right st) $ hostSP key

pageFor :: AppState -> F AppState UserAction
pageFor st = case activeTab st of
  HomeTab      -> homePage st
  SearchTab    -> searchPage st
  FilmTab      -> filmPage st
  LogTab       -> logPage
  DiaryTab     -> rowsPage "YOUR DIARY"     diaryLines
  WatchlistTab -> rowsPage "YOUR WATCHLIST" watchLines

-------------------------------------------------------------------------------
-- Poster cards
-------------------------------------------------------------------------------

posterColours :: [String]
posterColours = ["#000080", "#800000", "#006060", "#555500", "#600060", "#303030", "#004000"]

colourFor :: Int -> String
colourFor n = posterColours !! (abs n `mod` length posterColours)

hitColour :: SearchHit -> String
hitColour (HFilm m)   = colourFor (movieId m)
hitColour (HPerson p) = colourFor (personId p)
hitColour (HGenre g)  = colourFor (genreId g)
hitColour (HColl c)   = colourFor (collId c)

hitText :: SearchHit -> [String]
hitText (HFilm m)   = fitLines 2 cardW (movieTitle m) ++ [ yearStr m ++ "  *" ++ oneDp (movieVote m) ]
hitText (HPerson p) = fitLines 2 cardW (personName p)  ++ [ personDept p ]
hitText (HGenre g)  = fitLines 2 cardW (genreName g)   ++ [ "genre" ]
hitText (HColl c)   = fitLines 2 cardW (collName c)    ++ [ "collection" ]

-- | A poster: a coloured block of text with an OPEN button underneath
posterCard :: String -> [String] -> UserAction -> F AppState UserAction
posterCard col ls act = vBoxF (foldr1 (>*<)
  (  map line ls
  ++ [ const act >^=< btnF fontMonoBold "   OPEN   " ] ))
  where
    line s = captionF (setFont fontMonoBold . setFgColor colWhite . setBgColor col)
                      (fillTo cardW s)

hitCard :: SearchHit -> F AppState UserAction
hitCard h = posterCard (hitColour h) (hitText h) (OpenHit h)

filmCard :: Movie -> F AppState UserAction
filmCard = hitCard . HFilm

-- | Posters in rows of five, as in the sketches
posterGrid :: [F AppState UserAction] -> F AppState UserAction
posterGrid [] = plainLineF "(nothing to show - see the status bar below)"
posterGrid cs = vBoxF (foldr1 (>*<) [ hBoxF (foldr1 (>*<) r) | r <- rowsOf 5 cs ])

-------------------------------------------------------------------------------
-- Home page: "most popular this week" and "most popular on 25/8/2003"
-------------------------------------------------------------------------------

homePage :: AppState -> F AppState UserAction
homePage st = vBoxF (foldr1 (>*<)
  [ shelf SecWeek  "MOST POPULAR THIS WEEK"          (weekFilms st)
  , shelf SecRetro ("MOST POPULAR ON " ++ retroLabel) (retroFilms st) ])
  where
    shelf sec title films = vBoxF (foldr1 (>*<)
      (  [ barF title ]
      ++ [ posterGrid (map filmCard (take count films)) | not hidden ]
      ++ [ const (ToggleSection sec) >^=< btnF fontBold (if isOpen then "  ^ less  " else "  v more  ") ] ))
      where
        isOpen = openSection st == Just sec
        hidden = openSection st /= Nothing && not isOpen
        count  = if isOpen then 20 else 10

-------------------------------------------------------------------------------
-- Search page: search box and six filter buttons, then the results
-------------------------------------------------------------------------------

searchPage :: AppState -> F AppState UserAction
searchPage st = vBoxF (foldr1 (>*<)
  [ barF "SEARCH"
  , SubmitSearch >^=< (stringInputF >=^^< nullSP)
  , liveF (\s -> "Search terms above, then Enter.   Searching in: " ++ scopeName (searchScope s))
  , hBoxF (  vBoxF (foldr1 (>*<) [ filt ScGenres, filt ScLists, filt ScFilms ])
         >*< vBoxF (foldr1 (>*<) [ filt ScCast,   filt ScCrew,  filt ScMembers ]) )
  , barF (hitsTitle st)
  , posterGrid (map hitCard (searchHits st)) ])
  where
    filt sc = const (SetScope sc) >^=< btnF fontBold (fillTo 14 ("  " ++ scopeLabel sc))

scopeLabel :: Scope -> String
scopeLabel ScFilms   = "films"
scopeLabel ScCast    = "cast"
scopeLabel ScCrew    = "crew"
scopeLabel ScGenres  = "genres"
scopeLabel ScLists   = "lists"
scopeLabel ScMembers = "members"

-------------------------------------------------------------------------------
-- Film page
-------------------------------------------------------------------------------

filmPage :: AppState -> F AppState UserAction
filmPage st = case selectedMovie st of
  Nothing -> vBoxF (barF "FILM"
                    >*< plainLineF "No film selected. Use (?) SEARCH, or (+) LOG to look one up by title.")
  Just m  -> vBoxF (foldr1 (>*<)
    (  barF (filmName m)
    :  map plainLineF (filmBody m (movieInfo st))
    ++ [ hBoxF (  (const (GoTab LogTab)       >^=< btnF fontBold "  Log this film  ")
              >*< (const ClickAddToWatchlist >^=< btnF fontBold "  + Watchlist  ") ) ] ))

filmBody :: Movie -> Maybe Details -> [String]
filmBody m md =
     [ "Directed by " ++ orUnknown (maybe [] dDirectors md)
     , "Genre: " ++ orUnknown (maybe [] dGenres md) ++ runtime
     , "TMDB score: " ++ oneDp (movieVote m) ++ " / 10"
     , "" ]
  ++ [ "\"" ++ dTagline d ++ "\"" | Just d <- [md], not (null (dTagline d)) ]
  ++ take 6 (wrapWords 64 (movieOverview m))
  ++ [ "" ]
  ++ wrapWords 64 ("Starring: " ++ orUnknown (maybe [] dCast md))
  ++ [ "(extra details unavailable - see the status bar)" | Nothing <- [md] ]
  where
    orUnknown [] = "(unknown)"
    orUnknown xs = joinWith ", " xs
    runtime = case md of
      Just d | dRuntime d > 0 -> "     Runtime: " ++ show (dRuntime d) ++ " min"
      _                       -> ""

-------------------------------------------------------------------------------
-- Log page: the big "log a film" button
-------------------------------------------------------------------------------

logPage :: F AppState UserAction
logPage = vBoxF (foldr1 (>*<)
  [ barF "LOG A FILM"
  , plainLineF "Film title (press Enter to look it up):"
  , LogSearch >^=< (stringInputF >=^^< nullSP)
  , liveF (\s -> "Selected: " ++ maybe "(none yet)" filmName (selectedMovie s))
  , hBoxF (foldr1 (>*<)
      (  plainLineF "Rate it:"
      :  [ const (UpdateRating n) >^=< btnF fontBold (replicate n '*') | n <- [1 .. 5] ] ))
  , liveF (\s -> "Your rating: " ++ stars (currentRating s) ++ "  (" ++ show (currentRating s) ++ "/5)")
  , plainLineF "Review (press Enter to save):"
  , UpdateReviewText >^=< (stringInputF >=^^< nullSP)
  , liveF (\s -> "Your review: " ++ (if null (currentReview s) then "(nothing written yet)" else currentReview s))
  , const ClickLogMovie       >^=< btnF fontHeading "          LOG A FILM          "
  , const ClickAddToWatchlist >^=< btnF fontBold    "  + Add to Watchlist  " ])

-------------------------------------------------------------------------------
-- Diary and watchlist pages: plain text rows
-------------------------------------------------------------------------------

-- | Number of text rows on the diary and watchlist pages
panelRows :: Int
panelRows = 12

rowsPage :: String -> (AppState -> [String]) -> F AppState UserAction
rowsPage title f = vBoxF (foldr1 (>*<)
  (  barF title
  :  [ liveF (\s -> (f s ++ repeat "") !! i) | i <- [0 .. panelRows - 1] ] ))

diaryLines :: AppState -> [String]
diaryLines st =
  (show (length (loggedFilms st)) ++ " films logged")
  : (if null (loggedFilms st)
       then [ "Nothing logged yet. Press (+) LOG, pick a film and press LOG A FILM." ]
       else map fmt (take (panelRows - 1) (loggedFilms st)))
  where
    fmt r = stars (revRating r) ++ "  " ++ filmName (revMovie r)
            ++ (if null (revText r) then "" else "  -  " ++ revText r)

watchLines :: AppState -> [String]
watchLines st =
  (show (length (watchlist st)) ++ " films")
  : (if null (watchlist st)
       then [ "Your watchlist is empty. Open a film and press + Watchlist." ]
       else map (\m -> "+  " ++ filmName m) (take (panelRows - 1) (watchlist st)))
