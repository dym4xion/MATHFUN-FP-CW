-- -- -- -- -- -- -- -- MATHFUN FP COURSEWORK 2018/2019 -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- STUDENT NO.: 850844             -- -- -- -- -- -- -- --
-- ASSESSMENT:
-- Core functionality      = 32 marks
-- User Interface and File = 10 marks
-- Code Quality            =  8 marks

import Text.Read

-- Type Synonyms
type Title = String
type Artist = String
type Year = Int
type Sales = Int

-- Album Type Definition
data Album = Album Title Artist Year Sales
             deriving(Eq,Ord,Show,Read)

testData :: [Album]
testData = [Album "Greatest Hits" "Queen" 1981 6300000,
            Album "Gold: Greatest Hits" "ABBA" 1992 5400000,
            Album "Sgt. Pepper's Lonely Hearts Club Band" "The Beatles" 1967 5340000,
            Album "21" "Adele" 2011 5110000,
            Album "(What's the Story) Morning Glory?" "Oasis" 1995 4940000,
            Album "Thriller" "Michael Jackson" 1982 4470000,
            Album "The Dark Side of the Moon" "Pink Floyd" 1973 4470000,
            Album "Brothers in Arms" "Dire Straits"         1985 4350000,
            Album "Bad" "Michael Jackson" 1987 4140000,
            Album "Rumours" "Fleetwood Mac" 1977 4090000,
            Album "Greatest Hits II" "Queen" 1991 3990000,
            Album "Back to Black" "Amy Winehouse" 2006 3940000,
            Album "The Immaculate Collection" "Madonna" 1990 3700000,
            Album "25" "Adele" 2015 3500000,
            Album "Stars" "Simply Red" 1991 3450000,
            Album "Come On Over" "Shania Twain" 1998 3430000,
            Album "x" "Ed Sheeran" 2014 3380000,
            Album "Legend" "Bob Marley" 1984 3380000,
            Album "Bat Out of Hell" "Meat Loaf" 1977 3370000,
            Album "Back to Bedlam" "James Blunt" 2004 3360000,
            Album "Urban Hymns" "The Verve" 1997 3340000,
            Album "Bridge over Troubled Water" "Simon & Garfunkel" 1970 3260000,
            Album "1" "The Beatles" 2000 3230000,
            Album "Spirit" "Leona Lewis" 2007 3170000,
            Album "Crazy Love" "Michael Bublé" 2009 3130000,
            Album "No Angel" "Dido" 2000 3090000,
            Album "White Ladder" "David Gray" 1998 3020000,
            Album "The Fame" "Lady Gaga" 2009 2990000,
            Album "Only by the Night" "Kings of Leon" 2008 2980000,
            Album "A Rush of Blood to the Head" "Coldplay" 2002 2960000,
            Album "Talk on Corners" "The Corrs" 1997 2960000,
            Album "Spice" "Spice Girls" 1996 2960000,
            Album "Life for Rent" "Dido" 2003 2900000,
            Album "Beautiful World" "Take That" 2006 2880000,
            Album "The Joshua Tree" "U2" 1987 2880000,
            Album "Hopes and Fears" "Keane" 2004 2860000,
            Album "The War of the Worlds" "Jeff Wayne" 1978 2800000,
            Album "X&Y" "Coldplay" 2005 2790000,
            Album "Jagged Little Pill" "Alanis Morissette" 1995 2780000,
            Album "Tubular Bells" "Mike Oldfield" 1973 2760000,
            Album "Scissor Sisters" "Scissor Sisters" 2004 2760000,
            Album "...But Seriously" "Phil Collins" 1989 2750000,
            Album "Tracy Chapman" "Tracy Chapman" 1988 2710000,
            Album "Parachutes" "Coldplay" 2000 2710000,
            Album "The Man Who" "Travis" 1999 2687500,
            Album "Greatest Hits" "ABBA" 1975 2606000,
            Album "I've Been Expecting You" "Robbie Williams" 1998 2586500,
            Album "Come Away with Me" "Norah Jones" 2002 2556650,
            Album "Graceland" "Paul Simon" 1986 2500000,
            Album "Ladies & Gentlemen: The Best of" "George Michael" 1998 2500000]

-------------------------------- Functional Code ------------------------------

-- DEMO I - Convert the list into a single string which, if output using putStrLn, 
-- will display the data formatted neatly into four columns

-- Function to stringify every element in the album list to one string.
albumsToString :: [Album] -> String
albumsToString (x:xs) = albumToString (x) ++ "\n" ++ albumsToString (xs)
albumsToString [] = ""

-- Function to format an album as a string to make columns.
albumToString :: Album -> String
albumToString (Album title artist year sales) = title ++ 
                                                (replicate (37 - length(title)) ' ') ++ 
                                                "  " ++ artist ++ 
                                                (replicate (17 - length(artist)) ' ') ++ 
                                                "  " ++ (show (year)) ++ 
                                                "  "  ++ (show (sales))
                                                
-- DEMO II - Give the top 10 albums in descending order of sales
top10 :: [Album] -> [Album]
top10 = take 10

-- DEMO III - Give all albums that were released between two given years (inclusive)
albumsByYearRange :: Int -> Int -> [Album] -> [Album]
albumsByYearRange lb ub = filter (\(Album _ _ y _) -> y >= lb && y <= ub)

-- DEMO IV - Give all albums whose titles begin with a given prefix
albumsPrefixedWith :: String -> [Album] -> [Album]
albumsPrefixedWith pre = filter (\(Album t _ _ _) -> pre `isPrefix` t)

-- Function to determine if string prefixes another string
isPrefix :: [Char] -> [Char] -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) = if x == y then isPrefix xs ys else False

-- V - Give the total sales figure for a given artist (i.e. the sum of the 
-- sales figures of the artist’s albums)
totalArtistSales :: String -> [Album] -> Int
totalArtistSales artist li = totalSales (albumsByArtist artist li)

-- Function to produce a list of Albums by artists
albumsByArtist :: String -> [Album] -> [Album]
albumsByArtist artist = filter (\(Album _ a _ _) -> a == artist)

-- Function to produce total sales of Albums from a list.
totalSales :: [Album] -> Int
totalSales ((Album _ _ _ s):xs) = s + totalSales (xs)
totalSales [] = 0

-- DEMO VI - Give a list of pairs of artists’ names with the number of albums 
-- they have in the top 50 (each artist should appear exactly once in the result)
artistsNumTop50 :: [Album] -> [(Artist, Int)]
artistsNumTop50 albs = map (\x -> (x, (numAlbumsByArtInT50 x albs))) uniqArt
                       where
                       uniqArt = listOfUniqueArtists (generateArtistsList albs)

-- Function to get the number of top 50 albums of an artist
numAlbumsByArtInT50 :: Artist -> [Album] -> Int
numAlbumsByArtInT50 art li = length (albumsByArtist art li)

-- Fuction to reduce a list of of artists by removing duplicated
listOfUniqueArtists :: [Artist] -> [Artist]
listOfUniqueArtists (x:xs)  
    | x `elem` xs = listOfUniqueArtists xs
    | otherwise   = x:(listOfUniqueArtists xs)
listOfUniqueArtists [] = []

-- Function to generate a list of artists from a list of albums
generateArtistsList :: [Album] -> [Artist]
generateArtistsList ((Album _ a _ _):xs) = a:(generateArtistsList xs) 
generateArtistsList [] = []

-- VII - Remove the 50th (lowest-selling) album and add a given (new) album 
-- into the list (which may be placed higher than 50th place depending on its 
-- sales figure)
addNewRemove50th :: Album -> [Album] -> [Album]
addNewRemove50th alb li = insertAlbumInOrder alb (take 49 li)

-- Function to insert a new album into a list of albums in order of sales
insertAlbumInOrder :: Album -> [Album] -> [Album]
insertAlbumInOrder (Album nt na ny ns) ((Album t a y s):xs) = if ns > s 
                                                                then newAlbum:compareAlbum:xs
                                                                else compareAlbum:(insertAlbumInOrder newAlbum xs)
                                                                where
                                                                newAlbum = (Album nt na ny ns)
                                                                compareAlbum = (Album t a y s)
insertAlbumInOrder x [] = [x]

-- DEMO VIII - Increase the sales figure for one of the albums given its title & 
-- artist and the additional sales, possibly changing the album’s position 
-- in the list (if no album with the given details exists, the function should 
-- do nothing)
increaseAlbumSales :: Title -> Artist -> Int -> [Album] -> [Album]
increaseAlbumSales title artist increm li = insertAlbumInOrder moddedAlbum newList
                                            where
                                            moddedAlbum = captureModifyAlbumData title artist increm li
                                            newList = removeFromList title artist li

-- Function to remove the album whose price is being updated
removeFromList :: Title -> Artist -> [Album] -> [Album]
removeFromList tt ta ((Album t a y s):xs) = if t == tt && a == ta
                                             then xs
                                             else x:(removeFromList tt ta xs)
                                             where
                                             x = (Album t a y s)

-- Function to capture the data for the album being updated, returns with
-- updated sales value. Throws error if the album is not in the list.
captureModifyAlbumData :: Title -> Artist -> Int -> [Album] -> Album
captureModifyAlbumData tt ta inc ((Album t a y s):xs) = if t == tt && a == ta
                                                         then (Album t a y (s + inc))
                                                         else captureModifyAlbumData tt ta inc xs
captureModifyAlbumData _ _ _ [] = error "Album not in list. List not modified."

-- Demo function to test basic functionality (without persistence - i.e.
-- testData doesn't change and nothing is saved/loaded to/from albums file).
demo :: Int -> IO ()
demo 1  = putStrLn (albumsToString testData)
demo 2  = putStrLn (albumsToString (top10 testData))
-- all albums released between 2000 and 2008 inclusive
demo 3  = putStrLn (albumsToString (albumsByYearRange 2000 2008 testData)) 
-- all albums with titles beginning with "Th" 
demo 4  = putStrLn (albumsToString (albumsPrefixedWith "Th" testData))
-- total sales figure for "Queen"
demo 5  = putStrLn (show (totalArtistSales "Queen" testData))
-- all artists with the number of times they appear in the top 50 
demo 6  = putStrLn (show (artistsNumTop50 testData)) 
-- albums after removing 50th album and adding "Progress" by "Take That" from 2010 with 2700000 sales
demo 7  = putStrLn (albumsToString (addNewRemove50th (Album "Progress" "Take That" 2010 2700000) testData))
-- albums after increasing sales of "21" by "Adele" by 400000
demo 8  = putStrLn (albumsToString (increaseAlbumSales "21" "Adele" 400000 testData))

----------------------------------- UI Code -----------------------------------

main :: IO()
main = do
    albums <- getFileContents
    interface albums

getFileContents :: IO [Album]
getFileContents = do
    albsStr <- readFile "albums.txt"
    return (read albsStr :: [Album])

interface :: [Album] -> IO()
interface albs = do
    printCommands
    putStr "\nEnter Command: "
    cmd <- getLine
    exeComm cmd albs

printCommands :: IO ()
printCommands = do
    putStrLn "\nCOMMAND OPTIONS:"
    putStrLn "Type: 'all'      to print all of the albums."
    putStrLn "Type: 'top10'    to print the top ten albums by sales"
    putStrLn "Type: 'range'    to print the albums between an inclusive year range."
    putStrLn "Type: 'prefix'   to print the albums starting with a specific string."
    putStrLn "Type: 'artSales' to print the total sales for a given artist."
    putStrLn "Type: 'artTop50' to print a list of tuples with each artist and the number of albums the artist has in the top 50."
    putStrLn "Type: 'add'      to add a new album after removing the 50th album by sales and print the updated album set."
    putStrLn "Type: 'inc'      to increase the sales value for an album given its title and artist then print the updated album set."
    putStrLn "Type: 'exit'     to save the album set to file."

exeComm :: String -> [Album] -> IO()
exeComm "all" albs = do
    putStrLn "\nALL ALBUMS:"
    putStrLn (albumsToString albs)
    interface albs

exeComm "top10" albs = do
    putStrLn "\nTOP 10 ALBUMS:"
    putStrLn (albumsToString (top10 albs))
    interface albs

exeComm "range" albs = do 
    putStr "Enter Inclusive Lower Bound: "
    lb <- getInt
    putStr "Enter Inclusive Upper Bound: "
    ub <- getInt
    
    putStrLn ("\nALL ALBUMS FROM " ++ (show lb) ++ " TO " ++ (show ub) ++ " (INCLUSIVE):")
    putStrLn (albumsToString (albumsByYearRange lb ub albs))
    interface albs

exeComm "prefix" albs = do
    putStr "\nEnter Prefix String: "
    pref <- getLine
    putStrLn ("\nALBUMS WITH THE PREFIX '" ++ pref ++ "':")
    putStrLn (albumsToString (albumsPrefixedWith pref albs))
    interface albs

exeComm "artSales" albs = do
    putStr "\nEnter Artist Name: "
    art <- getLine
    putStrLn ("\nTOTAL SALES FOR THE ARTIST '" ++ art ++ "':")
    putStrLn (show (totalArtistSales art albs))
    interface albs

exeComm "artTop50" albs = do 
    putStrLn "\nLIST OF PAIRS SHOING THE NUMBER OF TOP 50 ALBUMS EACH ARTIST HAS:"
    putStrLn (show (artistsNumTop50 testData))
    interface albs

exeComm "add" albs = do
    putStr "Enter New Album Title: "
    ti <- getLine
    putStr "Enter New Album Artist: "
    ar <- getLine
    putStr "Enter New Album Release Year: "
    ry <- getInt
    putStr "Enter New Album Number Of Sales: "
    ns <- getInt
    putStrLn "\nUPDATED ALBUM SET WITH NEW ALBUM:"
    putStrLn (albumsToString (addNewRemove50th (Album ti ar ry ns) albs))
    interface (addNewRemove50th (Album ti ar ry ns) albs)

exeComm "inc" albs = do
    putStr "Enter Album Name: "
    ti <- getLine
    putStr "Enter Artist Name: "
    ar <- getLine
    putStr "Enter Quantity of Sales to Increase By: "
    qty <- getInt
    putStrLn ("\nUPDATED ALBUM SET WITH UPDATED ALBUM SALES FOR " ++ ti ++ " by " ++ ar ++ ":")
    putStrLn (albumsToString (increaseAlbumSales ti ar qty albs))
    interface albs

exeComm "exit" albs = do
    putStrLn "\nALBUM SET IS BEING SAVED TO FILE."
    writeFile "albums.txt" (show albs)
    putStrLn "ALBUM SAVED TO FILE. PROGRAM WILL NOW CLOSE."

exeComm _ albs = do
    putStrLn "Invalid command, try again."
    interface albs


-- Would it be better here to force the user to enter a valid integer or to return them to the interface?
getInt :: IO Int
getInt = do 
    str <- getLine
    if (isValidInt str) 
    then return (read str :: Int)
    else do
        putStr "Value is not a valid integer. Try Again: "
        getInt

isValidInt :: String -> Bool
isValidInt str
    | (readMaybe str :: Maybe Integer) == Nothing = False
    | otherwise = True
