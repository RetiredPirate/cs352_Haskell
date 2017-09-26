import Data.List
import Data.Char

----------------------------------------------------------------
-- CS 352 - Project 1 (starter file)
--
-- PUT YOUR NAME HERE
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
--   Incomplete.  Presently, it just echos the contents of the
--   input file to standard output, because the 'createIndex' function is
--   dummied up.
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
--   Incomplete.  Presently, it just echos the contents of the
--   input file to "output.txt", because the 'createIndex' function is
--   dummied up.
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
-- **** THIS IS THE FUNCTION YOU NEED TO CHANGE ****
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
--       -------   -----   -----   -----   -----   -----
--        Pilots       3       1       0      12       7
--       Broncos       2       1       1       5       4
--          Zags       0       3       1       3       9
--   In other words, each team is listed in order of wins (W) minus losses
--   (L), with the tiebreaker based on goals for (GA) minus goals against
--   (GA). (If still tied, use alphabetical order of team name.)
--
-- status:
--   Incomplete.  Presently, it just echos returns the contents of the
--   'str'.
--
----------------------------------------------------------------
createStandings:: String -> String
createStandings = show . map combineScores . groupBy (\x y -> fst x == fst y) .
        sortBy compare . concat . map getScores . concat .
        groupBy (\x y -> head x == head y) . sortBy compare . map words . lines


----------------------------------------------------------------
-- function getScores
-- Takes a list of strings and returns a list of two tuples
-- for each team with their five score numbers
----------------------------------------------------------------
getScores:: [String] -> [(String,[Int])]
getScores([team1, score1, team2, score2]) = [(team1, scoreList1), (team2, scoreList2)]
  where
    team1score = read score1 :: Int
    team2score = read score2 :: Int

    -- set first 3 numbers based on who won, set scores as gf and ga
    scoreList1 =
      if team1score > team2score
        then [1, 0, 0, team1score, team2score]
        else if team2score > team1score
          then [0, 1, 0, team1score, team2score]
          else [0, 0, 1, team1score, team2score]

    scoreList2 =
      if team1score > team2score
        then [0, 1, 0, team2score, team1score]
        else if team2score > team1score
          then [1, 0, 0, team2score, team1score]
          else [0, 0, 1, team2score, team1score]


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
----------------------------------------------------------------
combineScores:: [(String, [Int])] -> (String, [Int])
combineScores(list) = (teamName, scoreList)
  where
    teamName = fst (head list)

    listOfLists = map snd list
    scoreList = foldr1 (zipWith (+)) listOfLists


----------------------------------------------------------------
-- function formatScores
-- takes a list of tuples with the team name and five scores
-- and returns a single String, formatted for printing
----------------------------------------------------------------
formatOutput:: [(String, [Int])] -> String
formatOutput(list) = retString
  where
    nameLength = maximum ( map (length . fst) list)
    


----------------------------------------------------------------
-- function formatScores
-- takes a list of tuples with the team name and five scores
-- and returns a single String, formatted for printing
----------------------------------------------------------------
formatScores:: (String, [Int]) -> Int -> String
formatScores (teamName, [win, loss, tie, goalF, goalA]) nameLength = retString
  where
    retString = "NOPE"
