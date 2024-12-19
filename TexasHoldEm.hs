{-

No generative AI tools were used in the preparation of the solution to this work. This
refers to both the code and the report parts.

Created by Shohail Ismail

-}

module TexasHoldEm where

import Data.Char
import Data.List
import Data.Ord
import GHC.IO
import System.Random
import Text.Read

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show, Eq, Enum, Ord)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Enum, Ord)

data Card = Card {suit :: Suit, rank :: Rank} deriving (Show, Ord, Eq)

data HandRank = HighCard | OnePair | TwoPair | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | RoyalFlush | StraightFlush deriving (Eq, Ord, Show)

-- Deck is made to be a list of type Card
type Deck = [Card]

data PlayerMove = Fold | Check | Call | Bet Int | Raise Int deriving (Eq, Ord, Show)

-- RandomPlayer strategy
data PlayerType
  = RandomPlayer
  | SmartPlayer
  | AggressivePlayer
  | PassivePlayer
  | HumanPlayer
  deriving (Eq, Show)

data Player = Player
  { playerType :: PlayerType,
    name :: String,
    holeCards :: [Card],
    chips :: Int,
    isDealer :: Bool,
    isActive :: Bool
  }
  deriving (Eq, Show)

data GameState = GameState
  { deck :: Deck,
    communityCards :: [Card],
    pot :: Int,
    players :: [Player],
    smallBlind :: Int,
    bigBlind :: Int,
    currentDealer :: Int,
    currentBets :: [Int]
  }
  deriving (Show)

-- Initialises a full deck
initDeck :: Deck
initDeck = [Card s r | s <- [Hearts .. Spades], r <- [Two .. Ace]]

-- Shuffle the deck of cards using RNG
shuffleDeck :: Deck -> IO Deck
shuffleDeck [] = return []
shuffleDeck deck = do
  randomIndex <- randomRIO (0, length deck - 1)
  let chosenCard = deck !! randomIndex
  let smallerDeck = take randomIndex deck ++ drop (randomIndex + 1) deck
  restShuffled <- shuffleDeck smallerDeck
  return (chosenCard : restShuffled)

-- Deal cards from the deck
dealCards :: Int -> Deck -> ([Card], Deck)
dealCards n deck
  | n <= 0 = ([], deck) -- Returns deck if no cards requetsed
  | n > length deck = (deck, []) -- Returns remaining cards if more card requested than available
  | otherwise =
      splitAt n deck

-- Initialises the GameState
initGame :: Int -> Int -> Int -> [PlayerType] -> IO GameState
initGame numPlayers startingChips smallBlind playerTypes = do
  shuffledDeck <- shuffleDeck initDeck

  -- Assigns starting stack to all players and makes them Active
  let instantiatedPlayers =
        [ Player
            { playerType = playerStrategy,
              name = show i,
              holeCards = [],
              chips = startingChips,
              isDealer = i == 1, -- Starts off with first player as dealer.
              isActive = True
            }
          | (playerStrategy, i) <- zip playerTypes [1 ..]
        ]

  -- Creates the first GameState
  let initialGameState =
        GameState
          { deck = shuffledDeck,
            communityCards = [],
            pot = 0,
            players = instantiatedPlayers,
            smallBlind = smallBlind,
            bigBlind = smallBlind * 2,
            currentDealer = 0,
            currentBets = replicate numPlayers 0
          }

  return initialGameState

makeMove :: Player -> GameState -> IO PlayerMove
makeMove player gameState =
  case playerType player of
    RandomPlayer -> randomStrategy gameState player
    PassivePlayer -> return $ passiveStrategy gameState player
    AggressivePlayer -> return $ aggressiveStrategy gameState player
    HumanPlayer -> humanStrategy gameState player

-- -- Simulates a RandomPlayer strategy
randomStrategy :: GameState -> Player -> IO PlayerMove
randomStrategy gameState player = do
  -- Creates random values based for move option
  randomValue <- randomRIO (0, 100) :: IO Int

  -- Creates a psuedo-random, valid number of chips for the player to bet
  let randomChipNum = max (smallBlind gameState) $ min playerChips (10 * minBet `div` 100)
      -- Extracts game info to ensure moves are only made when valid and with approrpriate amounts
      playerIndex = head [i | (i, x) <- zip [0 ..] (players gameState), name x == name player]
      playerBet = currentBets gameState !! playerIndex
      playerChips = chips player
      minBet = maximum (currentBets gameState)

  -- Gives different probabilties for moves to be made
  return $
    if playerChips < minBet && minBet > 0
      then Fold
      else case () of
        _
          | randomValue < 20 && minBet == 0 -> Check
          | randomValue < 70 && minBet == 0 && playerChips >= minBet + randomChipNum -> Bet randomChipNum
          | randomValue < 45 && minBet > 0 && playerChips >= minBet - playerBet -> Call
          | randomValue <= 100 && playerChips >= minBet + randomChipNum -> Raise randomChipNum
          | otherwise -> Fold -- Folds only if number of chips falls short of minimum bet to continue playing

-- Smulates a player who only checks, calls or folds
passiveStrategy :: GameState -> Player -> PlayerMove
passiveStrategy gameState player =
  let playerIndex = head [i | (i, x) <- zip [0 ..] (players gameState), name x == name player]
      playerBet = currentBets gameState !! playerIndex
      minBet = maximum (currentBets gameState)
   in if chips player < minBet
        then Fold
        else
          if minBet == 0
            then Check
            else Call

-- Smulates a player who bets and raises whenever possible, only calling and folding if necessary
aggressiveStrategy :: GameState -> Player -> PlayerMove
aggressiveStrategy gameState player =
  let playerChips = chips player
      minBet = maximum (currentBets gameState)
      -- Raises by the small blind or a large % of chips
      amount = max (smallBlind gameState) (playerChips `div` 5)
   in if playerChips < minBet
        then Fold -- Player folds only when forced
        else
          if minBet == 0
            then Bet amount -- Always bets when given chance, mever checks
            else
              if playerChips >= minBet + amount
                then Raise amount -- Alwasy raises when given chance
                else Call -- Call only if forced

-- The human's strategy relies on their input and whatever playstyle they choose
humanStrategy :: GameState -> Player -> IO PlayerMove
humanStrategy gameState player = do
  -- Informational output on player's progress
  putStrLn $ "\nYou are player " ++ name player
  putStrLn $ "Your chips: " ++ show (chips player)
  putStrLn $ "The current community cards: " ++ show (communityCards gameState)
  putStrLn $ "Your hole cards: " ++ show (holeCards player)
  putStrLn $ "The pot is currently at " ++ show (pot gameState) ++ " dollars."

  let playerIndex = head [i | (i, x) <- zip [0 ..] (players gameState), name x == name player]
      playerBet = currentBets gameState !! playerIndex
      minBet = maximum (currentBets gameState)

  putStrLn $ "Minimum bet to call: " ++ show (minBet - playerBet)
  putStrLn "(Please enter F (fold), Ch (check), C (call), B <amount> (Bet <amount>), R <amount> (Raise <amount>) \n What is your move: "

  nextMove <- getLine
  case words (map toLower nextMove) of
    ["f"] -> return Fold
    ["ch"] ->
      if minBet == 0
        then return Check
        else invalidMove
    ["c"] ->
      if chips player >= (minBet - playerBet)
        then return Call
        else invalidMove
    ["b", amountStr] ->
      case readMaybe amountStr of
        Just amount ->
          if minBet == 0 && amount > 0 && amount <= chips player
            then return $ Bet amount
            else invalidMove
        Nothing -> invalidMove
    ["r", amountStr] ->
      case readMaybe amountStr of
        Just amount ->
          let raiseAmount = (minBet - playerBet) + amount
           in if chips player >= raiseAmount
                then return $ Raise amount
                else invalidMove
        Nothing -> invalidMove
    _ -> invalidMove
  where
    invalidMove = do -- Loops until a valid move is made
      putStrLn "Invalid move - please try again!"
      humanStrategy gameState player

-- Evaluates rank of a given hand
evaluateHand :: [Card] -> HandRank
evaluateHand cards
  | isRoyalFlush sortedCards = RoyalFlush
  | isStraightFlush sortedCards = StraightFlush
  | isFourOfAKind sortedCards = FourOfAKind
  | isFullHouse sortedCards = FullHouse
  | isFlush sortedCards = Flush
  | isStraight sortedCards = Straight
  | isThreeOfAKind sortedCards = ThreeOfAKind
  | isTwoPair sortedCards = TwoPair
  | isOnePair sortedCards = OnePair
  | otherwise = HighCard
  where
    sortedCards = sort cards

-- Helper functions to determine hand
isRoyalFlush :: [Card] -> Bool
isRoyalFlush cards =
  let highestRank = maximum (map rank cards)
   in isStraightFlush cards && highestRank == Ace

isStraightFlush :: [Card] -> Bool
isStraightFlush cards = isStraight cards && isFlush cards

isFourOfAKind :: [Card] -> Bool
isFourOfAKind cards = hasGroupOf cards 4

isFullHouse :: [Card] -> Bool
isFullHouse cards = 3 `elem` countOccurrencesOfRank cards && 2 `elem` countOccurrencesOfRank cards

isFlush :: [Card] -> Bool
isFlush cards = all ((== suit (head cards)) . suit) cards

isStraight :: [Card] -> Bool
isStraight cards =
  let ranks = map rank cards
      sortedRanks = sort ranks

      -- Checks for consecutive sequence of ranks (default case)
      checkForSequence :: [Rank] -> Bool
      checkForSequence [] = True
      checkForSequence [_] = True
      checkForSequence (x : y : rest) =
        (fromEnum y == fromEnum x + 1) && checkForSequence (y : rest)

      -- Accounts for Ace-low Straight (edge case)
      aceLow = [Ace, Two, Three, Four, Five]
   in (sortedRanks == aceLow) || checkForSequence sortedRanks

isThreeOfAKind :: [Card] -> Bool
isThreeOfAKind cards = hasGroupOf cards 3

isTwoPair :: [Card] -> Bool
isTwoPair cards = length (filter (>= 2) (countOccurrencesOfRank cards)) >= 2

isOnePair :: [Card] -> Bool
isOnePair cards = hasGroupOf cards 2

highestCard :: [Card] -> Card
highestCard = maximum

-- Helper functions to modularise code
countOccurrencesOfRank :: [Card] -> [Int]
countOccurrencesOfRank cards = map (\r -> length (filter ((== r) . rank) cards)) [Two .. Ace]

hasGroupOf :: [Card] -> Int -> Bool
hasGroupOf cards n = n `elem` countOccurrencesOfRank cards

-- Determines stronger hand given 2, accounting for ties
compareHands :: [Card] -> [Card] -> Ordering
compareHands hand1 hand2 =
  let rank1 = evaluateHand hand1
      rank2 = evaluateHand hand2
   in if rank1 /= rank2
        then compare rank1 rank2 -- Compare in case of no tie
        else case rank1 of -- Compare in case of tie
          OnePair -> compareRankClusters hand1 hand2 2 -- Tied with One Pair
          TwoPair -> compareRankClusters hand1 hand2 2 -- Tied with Two Pair
          ThreeOfAKind -> compareRankClusters hand1 hand2 3 -- Tied with 3 Of A Kind
          FourOfAKind -> compareRankClusters hand1 hand2 4 -- Tied with 4 Of A Kind
          FullHouse -> fullHouseTie hand1 hand2 -- Tied with Full House
          _ -> highestCardTie hand1 hand2 -- Tied with any other hand

-- Tie breaker for simple tie where only high cards are compared
highestCardTie :: [Card] -> [Card] -> Ordering
highestCardTie hand1 hand2 =
  let highestRankIn1 = sortBy (comparing Down) $ map rank hand1
      highestRankIn2 = sortBy (comparing Down) $ map rank hand2
   in compare highestRankIn1 highestRankIn2

-- Compares the ranking of groups of cards of matching ranks, rather than singular cards
compareRankClusters :: [Card] -> [Card] -> Int -> Ordering
compareRankClusters hand1 hand2 clusterSize =
  -- Gets number of each rank in each players' hands
  let rankOccurences1 = zip [Two .. Ace] (countOccurrencesOfRank hand1)
      rankOccurences2 = zip [Two .. Ace] (countOccurrencesOfRank hand2)

      -- Finds which rank has a count of clusterSize in each hand
      rankGroup1 = sortBy (comparing Down) [r | (r, count) <- rankOccurences1, count == clusterSize]
      rankGroup2 = sortBy (comparing Down) [r | (r, count) <- rankOccurences2, count == clusterSize]

      -- Sorts kicker in descending order for for comparison
      kickers1 = sortBy (comparing Down) [rank c | c <- hand1, rank c `notElem` rankGroup1]
      kickers2 = sortBy (comparing Down) [rank c | c <- hand2, rank c `notElem` rankGroup2]
   in -- Compares rank pairs, or kickers if rank pairs are equal
      if rankGroup1 /= rankGroup2
        then compare rankGroup1 rankGroup2
        else compare kickers1 kickers2

-- Helper function to tie-break Full House
fullHouseTie :: [Card] -> [Card] -> Ordering
fullHouseTie hand1 hand2 =
  -- Checks if trips are equal, and if so, checks pair for tie-breaking Full House
  let tripResult = compareRankClusters hand1 hand2 3
   in if tripResult /= EQ
        then tripResult
        else compareRankClusters hand1 hand2 2

-- Determines the winner out of active players given their best hands (accounting for ties)
determineWinner :: GameState -> [Player]
determineWinner gameState =
  let activePlayers = filter isActive (players gameState)
      evaluatedHands =
        [ (player, evalPlayerHand (holeCards player) (communityCards gameState))
          | player <- activePlayers
        ]
      bestHand = maximum $ map snd evaluatedHands
      winnerList = [player | (player, handRank) <- evaluatedHands, handRank == bestHand]
   in winnerList

-- Assess the best hand rank for a player given their hole and community cards
evalPlayerHand :: [Card] -> [Card] -> HandRank
evalPlayerHand holeCards communityCards =
  let allCards = holeCards ++ communityCards
   in maximum $ map evaluateHand (filter ((5 ==) . length) (subsequences allCards))

-- Loops a single betting round until all active players have made (all) valid actions
bettingRound :: GameState -> IO GameState
bettingRound gameState = do
  let activePlayers = filter isActive (players gameState)
      numOfPlayers = length activePlayers

  -- Recrusive function to handle loop of players being called until all valid actions have been taken
  let runBetting :: Int -> [Bool] -> GameState -> IO GameState
      runBetting index allActionsMade gs = do
        let validActed = and allActionsMade
            minBet = maximum (currentBets gs)
            activePlayers = filter isActive (players gs) -- Refilters by active players in current GameState

        -- Ends the round if all players have acted and each has matched the current minimum bet/folded
        if validActed && all (\(p, b) -> not (isActive p) || b == minBet) (zip (players gs) (currentBets gs))
          then return gs
          else do
            -- Checks if all players but one (or less) have folded and if so, ends round
            if length activePlayers <= 1
              then return gs
              else do
                let activePlayerIndex = index `mod` length activePlayers
                    player = activePlayers !! activePlayerIndex
                    playerIndex = head [i | (i, x) <- zip [0 ..] (players gs), name x == name player]
                    playerBet = currentBets gs !! playerIndex

                -- Checks if player is inactive and, if so, skips before allowing for actions
                if not (isActive player)
                  then runBetting (index + 1) allActionsMade gs
                  else do
                    move <- makeMove player gs
                    updatedGS <- case move of
                      Fold -> do
                        -- If folded, mark player inactive
                        let updatedPlayers =
                              take playerIndex (players gs)
                                ++ [player {isActive = False}]
                                ++ drop (playerIndex + 1) (players gs)
                        return gs {players = updatedPlayers}
                      -- Check only done if no bets made yet, and skips player
                      Check -> return gs
                      -- Bet only done if no bets made yet
                      Bet amount -> do
                        let updatedBets = updatePlayerBet (currentBets gs) playerIndex (playerBet + amount)
                            updatedPot = pot gs + amount
                            updatedPlayers = updatePlayerChips (players gs) playerIndex (chips player - amount)
                        return gs {players = updatedPlayers, currentBets = updatedBets, pot = updatedPot}

                      -- Matches current minimum bet to play (or max bet made so far)
                      Call -> do
                        let updatedChips = chips player - (minBet - playerBet)
                            updatedBets = updatePlayerBet (currentBets gs) playerIndex minBet
                            updatedPot = pot gs + (minBet - playerBet)
                            updatedPlayers = updatePlayerChips (players gs) playerIndex updatedChips
                        return gs {players = updatedPlayers, currentBets = updatedBets, pot = updatedPot}
                      -- Raises above current min. bet and sets new min.
                      Raise amount -> do
                        let raiseAmount = (minBet - playerBet) + amount
                            updatedChips = chips player - raiseAmount
                            updatedBets = updatePlayerBet (currentBets gs) playerIndex (playerBet + raiseAmount)
                            updatedPot = pot gs + raiseAmount
                            updatedPlayers = updatePlayerChips (players gs) playerIndex updatedChips
                        return gs {players = updatedPlayers, currentBets = updatedBets, pot = updatedPot}

                    -- Resets actions if a player raises, otherwise marks player as having acted validly
                    let newAllActionsMade =
                          case move of
                            Raise _ -> replicate numOfPlayers False
                            _ -> updatePlayerBet allActionsMade activePlayerIndex True

                    -- Recursive call to next player
                    runBetting (index + 1) newAllActionsMade updatedGS

  -- Start the betting round
  finalState <- runBetting 0 (replicate numOfPlayers False) gameState

  -- Reset all current bets to 0 for next betting round
  return finalState {currentBets = replicate (length (players gameState)) 0}

-- Helper functions
-- Updates player's chip count
updatePlayerChips :: [Player] -> Int -> Int -> [Player]
updatePlayerChips players index newChips =
  take index players ++ [player {chips = newChips}] ++ drop (index + 1) players
  where
    player = players !! index

-- Update player's bet in the currentBets list
updatePlayerBet :: [a] -> Int -> a -> [a]
updatePlayerBet bets playerIndex newAmount =
  take playerIndex bets ++ [newAmount] ++ drop (playerIndex + 1) bets

main :: IO ()
main = do
  -- Initialises a game with one human player
  gs <- initGame 4 1000 10 [HumanPlayer, RandomPlayer, PassivePlayer, AggressivePlayer]

  -- Prints initial game state for human player's reference
  --print gs
  putStrLn "\n\nWelcome to the Curry-no Royale, may the purest of luck be with you! Your details are as follows:\n"

  -- Runs 5 betting rounds
  finalGS <- bettingRound gs

  -- Prints final game state after showdown
  putStrLn "\n\nThe round is over - look below to see how you did!\n"
  --print finalGS
