{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module Scavenge where
import Dictionaries
import Data.List (sort)
import Debug.Trace

--                                          Type Aliases
-- These type aliases help to abstract our code. 
-- 
type Hand = [Char]
type Move = String
type Play = [Move]
type Dictionary = [String]

-- All undefined values and functions should be completed. Your code will compile and test 
-- (with the --test flag) even if some functions are left undefined.
--
--                                       Milestone
--

-- Score takes a move and returns the sum of the letter values, according to the Scrabble scoring
-- system: https://hasbro-new.custhelp.com/app/answers/detail/a_id/55/
-- A helper function may be useful.
score :: Move -> Integer
score [] = 0
score (x:xs) 
    | x `elem` "AEIOULNSTR" = 1 + score xs
    | x `elem` "DG"         = 2 + score xs
    | x `elem` "BCMP"       = 3 + score xs
    | x `elem` "FHVWY"      = 4 + score xs
    | x `elem` "K"          = 5 + score xs
    | x `elem` "JX"         = 8 + score xs
    | x `elem` "QZ"         = 10 + score xs
    | otherwise             = error "No letter"

-- score "QA" == 11
-- score "JF" == 12

-- scorePlay takes a play and returns the total score of all words.
scorePlay :: Play -> Integer
scorePlay [] = 0
scorePlay (x:xs) = score x + scorePlay xs
-- scorePlay ["KB", "QA"] == 19 

-- remove takes an element and a list, and returns the list with one copy of that element removed.
-- You should not assume the list is sorted. If there are multiple copies of the element in the list,
-- only remove one of them. If the element doesn't occur, you should throw an error.
remove :: Eq a => a -> [a] -> [a]
remove _ [] = error "Element not found"
remove y (x:xs)
    | y == x = xs
     | otherwise = x : remove y xs



-- remove 7 [7,3,1,7,5] = [3,1,7,5] 
-- The order here doesn't matter, if you remove the second 7 it is okay.

-- updateHand should take a hand (a list of characters), and a move (a string), and return the hand
-- that remains after that move is played.
updateHand :: Hand -> Move -> Hand
updateHand hand [] = hand
updateHand hand (x:xs) = updateHand (remove x hand) xs

-- canMake takes a hand and a move, and tells you if that move can be made with that hand. Be sure to
-- consider the possibility that a letter may occur more times in the move than it does in the hand.
-- canMake :: Hand -> Move -> Bool
-- canMake _ [] = True
-- canMake [] _ = False
-- canMake (x:xs) letters
--     | x `elem` letters = canMake (remove x (x:xs)) (remove x letters)
--     | otherwise = canMake xs letters

help_canMake :: Hand -> Move -> Bool
help_canMake [] [] = True
help_canMake _ [] = True
help_canMake [] _ = False
help_canMake (x:xs) (y:ys)
    | y == x = help_canMake xs ys
    | otherwise = help_canMake xs (y:ys)


canMake :: Hand -> Move -> Bool
canMake hand [] = True
canMake [] moves = False
canMake hand moves@(x:xs) = 
    let sortedHand = sort hand
        sortedMove = sort moves
    in help_canMake sortedHand sortedMove

-- isValidMove tests if a move is valid with respect to a dictionary and hand: 
-- the move must be a word in the dictionary and a move that can be made with the hand.
isValidMove :: Dictionary -> Hand -> Move -> Bool
isValidMove [] hand move = False
isValidMove (x:xs) playerHand move =
    if x == move
        then canMake playerHand x
        else isValidMove xs playerHand move 

-- isValidPlay checks if a play is valid. Each move in the play must be a word in the dictionary, and
-- must be playable using whatever remains of the hand after all previous moves have been made.
isValidPlay :: Dictionary -> Hand -> Play -> Bool
isValidPlay _ _ [] = True 
isValidPlay dictionary playerHand (move:remainingMoves) =
    isValidMove dictionary playerHand move && isValidPlay dictionary (updateHand playerHand move) remainingMoves
-- isValidPlay tinyDict "TMAKE" ["TAKE"] = TRUE
-- isValidPlay tinyDict "TMAKE" ["MAKE"] = TRUE
-- isValidPlay tinyDict "TMAKE" ["TAKE","MAKE"] = False

-- validMoves: takes a dictionary and a hand, and returns all words that can be
-- created by letters in the hand. Order does not matter.
validMoves :: Dictionary -> Hand -> [Move]
validMoves [] hand = []
validMoves dict [] = []
validMoves (x:xs) playerHand = 
    if canMake playerHand x
        then x:(validMoves xs playerHand)
        else validMoves xs playerHand


--                                  End of Milestone!

--                                  Core Project 

-- --- Greedy Algorithm

-- greedyPlay: choose the best move you can at any given point in time, then check to see what
-- other moves you can make.
greedyPlay :: Dictionary -> Hand -> Play
greedyPlay dict [] = []
greedyPlay dict [x]
    | validMoves dict [] == [] = []
    | otherwise = validMoves dict [x]
greedyPlay dict hand
    | ifValid == [] = []
    | otherwise =
        let best_possibleWord = snd (maximum [(score x, x) | x <- ifValid])
            updatedHand = updateHand hand best_possibleWord
        in best_possibleWord : greedyPlay dict updatedHand
    where
        ifValid = validMoves dict hand
-- greedyPlay shortDict "CLOSEFLOOR" = ["FORCE", "SO"] 

-- --- Brute Force Algorithms
-- You are going to search for the best play, instead of just choosing the best move one at a time.
-- To do so, you will consider every possible play, by considering every possible combination of
-- words. You will implement two variants. 

-- powerset: return the powerset of the input, i.e. the list of all sub-lists.
-- You may assume the list has no duplicates. 
-- The output should not have duplicates, up to sorting.
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = 
    let power = powerset xs
    in [x:sub | sub <-power] ++ power
-- powerset [1,2] = [[],[1],[1,2],[2]]
-- It is acceptable to have [2,1] INSTEAD of [1,2], but not in addition.
-- length (powerset "abcde") = 32

-- The Naive Brute Force approach (naiveBrutePlay) takes every combination of moves in
-- the dictionary: the powerset of the dictionary. It will only work with very small
-- dictionaries, like tenWords.  You will then choose the best valid play out of that list.
naiveBrutePlay :: Dictionary -> Hand -> Play
naiveBrutePlay dict hand = foldl findBestValidPlay [] (powerset dict)
  where
    findBestValidPlay maxPlay play =
        if isValidPlay dict hand play && scorePlay play >= scorePlay maxPlay
        then play
        else maxPlay

 
-- The Smart Brute Force approach realizes that we can restrict the size of the dictionary
-- before we compute the powerset. There are probably MANY moves in the dictionary that we can't
-- create at all! So, first find all the moves that can be made with this hand. Then, take the
-- powerset of that restricted list to create a list of all plays made up of those moves. Then
-- find the best valid play from that list.
smartBrutePlay :: Dictionary -> Hand -> Play
smartBrutePlay dict hand = findBestValidPlay (generatePowerset validMoves) []
    where
        validMoves = [move | move <- dict, canMake hand move]

        generatePowerset [] = [[]]
        generatePowerset (x:xs) =
            let restPowerset = generatePowerset xs
            in [x:sub | sub <- restPowerset] ++ restPowerset

        findBestValidPlay [] maxPlay = maxPlay
        findBestValidPlay (play:plays) maxPlay =
            if isValidPlay dict hand play && scorePlay play >= scorePlay maxPlay
            then findBestValidPlay plays play
            else findBestValidPlay plays maxPlay




-- --- Best Play Algorithm

-- Finally we will try a recursive strategy to find the best play. Even the smart brute force
-- fails for large hands: I can make a lot of moves, but not a lot of moves simultaniously. Thus
-- the list of all possible plays is large, but the list of all valid plays is still likely
-- small. 
-- For this algorithm, start with the list of valid moves. Then, for each move find the
-- best play for the remaining hand. Select the hand that leads to the best overall score, counting both
-- the score of the move and the score of the best remaining play.

bestPlay:: Dictionary -> Hand -> Play 
bestPlay [] _ = []
bestPlay _ [] = []
bestPlay dict hand@(x:xs)
  | null valid = []
  | otherwise = bestScore plays []
  where
    valid = validMoves dict hand
    updateHandForMove = [(x, updateHand hand x) | x <- valid]
    plays = [y : bestPlay valid hand | (y, hand) <- updateHandForMove]

bestScore :: [Play] -> Play -> Play
bestScore [] plays = plays
bestScore (x:xs) plays = bestScore' (scorePlay x) (scorePlay plays)
  where
    bestScore' score_currentPlay maxScore
      | score_currentPlay > maxScore = bestScore xs x
      | otherwise = bestScore xs plays


