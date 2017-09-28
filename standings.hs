import Data.List
import Data.Char

----------------------------------------------------------------
-- CS 352 - Project 1 (starter file)
--
-- Andrew Williams
--
----------------------------------------------------------------

----------------------------------------------------------------
-- function exec - reads the contents of the file "input.txt",
--   and creates an index of the words; it writes the result to
--   standard output
--
-- **** YOU SHOULD NOT HAVE TO CHANGE THIS FUNCTION ****
--
-- calling sequence:
--   exec
--
-- example:
--   (See example in the function 'createIndex'.)
--
-- status:
--   Complete. writes table of values to console
--
----------------------------------------------------------------
exec:: IO()
exec =
  do input <- readFile "input.txt"
     putStr (createStandings input)

----------------------------------------------------------------
-- function exec2 - reads the contents of the file "input.txt",
--   and creates an index of the words; it writes the result to
--   the file "output.txt"
--
-- **** YOU SHOULD NOT HAVE TO CHANGE THIS FUNCTION ****
--
-- calling sequence:
--   exec2
--
-- example:
--   (See example in the function 'createIndex'.)
--
-- status:
--   Complete. Writes table of values to output.txt
----------------------------------------------------------------
exec2:: IO()
exec2 =
  do input <- readFile "input.txt"
     writeFile "output.txt" (createStandings input)

----------------------------------------------------------------
-- function createStandings - treats its String argument as the contents of
-- a document that contains game results; produces list of standings,
-- ordered by win-loss record, of all the teams that have competed.
--
--
-- calling sequence:
--   createStandings str
--
-- example:
--   If the contents of the String 'str' is
--     Broncos 1 Pilots 2
--     Zags 2 Pilots 4
--     Zags 0 Broncos 0
--     Pilots 2 Broncos  3
--     Pilots 4 Zags 1
--     Broncos 1 Zags 0
--   Then the result should be:
--          Team       W       L       T      GF      GA
--       -----------------------------------------------
--        Pilots       3       1       0      12       7
--       Broncos       2       1       1       5       4
--          Zags       0       3       1       3       9
--   In other words, each team is listed in order of wins (W) minus losses
--   (L), with the tiebreaker based on goals for (GA) minus goals against
--   (GA). (If still tied, use alphabetical order of team name.)
--
-- status:
--   Creates table as shown above. Allows for arbitrary length
--   team names (no spaces) and any size of Int (not Integer)
--
----------------------------------------------------------------
createStandings:: String -> String
createStandings = formatOutput . map combineScores .
                    groupBy (\x y -> fst x == fst y) .
                      sort . concatMap getScores .
                        concat . groupBy (\x y -> head x == head y) .
                          sort . map words . lines


----------------------------------------------------------------
-- function getScores
-- Takes a list of strings and returns a list of two tuples
-- for each team with their five score numbers
--
-- calling sequence:
--    getScores ["thisIs", "42", "aList", "23"]
--
-- returns:
--    [("thisIs", [1,0,0,42,23]), ("aList", [0,1,0,23,42])]
----------------------------------------------------------------
getScores:: [String] -> [(String,[Int])]
getScores [team1, score1, team2, score2] = [(team1, scoreList1), (team2, scoreList2)]
  where
    team1score = read score1 :: Int
    team2score = read score2 :: Int

    -- set first 3 numbers based on who won, set scores as gf and ga
    scoreList1
      | team1score > team2score = [1, 0, 0, team1score, team2score] --win
      | team2score > team1score = [0, 1, 0, team1score, team2score] --loss
      | otherwise = [0, 0, 1, team1score, team2score] --tie

    scoreList2
      | team1score > team2score = [0, 1, 0, team2score, team1score] --loss
      | team2score > team1score = [1, 0, 0, team2score, team1score] --win
      | otherwise = [0, 0, 1, team2score, team1score] --tie


----------------------------------------------------------------
-- function combineScores
-- takes in a list of tuples, each with the name of a team and
-- a list of the five score numbers for that team for a
-- certain game. This function will be mapped to a sorted
-- list of these lists, so each given list will be headed
-- by the same team name. This function adds each of the lists
-- of scores together into a single list for the given team.
--
-- returns a single tuple with the team name and the list of
-- five score numbers
--
--
-- calling sequence:
--    getScores [("team4", [1,0,0,16,3]), ("team4", [0,1,0,26,7])]
--
-- returns:
--    ("team4", [1,1,0,42,10])
----------------------------------------------------------------
combineScores:: [(String, [Int])] -> (String, [Int])
combineScores list = (teamName, scoreList)
  where
    teamName = fst (head list)

    listOfLists = map snd list
    scoreList = foldr1 (zipWith (+)) listOfLists


----------------------------------------------------------------
-- function formatOutput
-- takes a list of tuples with the team name and five scores
-- and returns a single String, formatted for printing
--
--
-- calling sequence:
--    getScores [("team4", [1,0,0,16,3]), ("team5", [2,1,5,7,5])]
--
-- returns:
--    table, as shown in the comment for createStandings
----------------------------------------------------------------
formatOutput:: [(String, [Int])] -> String
formatOutput list = returnString
  where
    initNameLength = maximum ( map (length . fst) list)
      -- length of longest team name

    initNumLengths = foldr1 (zipWith max) ( map (map (length . show) . snd) list )
      -- list of longest length for each number in the scores list

    minNameLength = 8
    minNumLengths = repeat 5
      -- Define minimum lengths for table columns
    nameLength = max initNameLength minNameLength
    numLengths = zipWith max initNumLengths minNumLengths
      -- set width to minimum if less than minimum

    lengthMasterList = map (3 +) (nameLength : numLengths)
      -- 6 list of column widths with 3 spaces between each column


    valueMasterList = map (\tup -> fst tup : map show (snd tup)) list  -- 6 list of collumn values


    valueTable = concatMap (padLine lengthMasterList) valueMasterList
      -- format each line of the output and concatenate them

    topLine = padLine lengthMasterList ["Team","W","L","T","GF","GA"]
    sndLine = padLine lengthMasterList (repeat (replicate (maximum lengthMasterList) '-'))
      -- fill the second line with dashes

    returnString = topLine ++ sndLine ++ valueTable




----------------------------------------------------------------
-- function padLine
-- zips a list of ints with a list of Strings using the padLeft
-- function. This is used to format lines for the output table
-- when called repeatedly with the same list of lengths
--
-- example:
--    padLine [5,10,15] ["hello", "there", "friendo"]
--        ->   "hello     there        friendo\n"
----------------------------------------------------------------
padLine:: [Int] -> [String] -> String
padLine lengthsList valueList = concat (zipWith padLeft lengthsList valueList) ++ "\n"



----------------------------------------------------------------
-- function padLeft
-- pads the given string to make it the given length
--
-- example:
--   padLeft 7 "hi"   ->   "     hi"
----------------------------------------------------------------
padLeft:: Int -> String -> String
padLeft padLength text = reverse (take padLength (reverse text ++ cycle " "))
