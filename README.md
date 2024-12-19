# Texas Hold'em in Haskell

   * [Overview](#overview)
   * [Core features](#core-features)
   * [Limitations and future developments](#limitations-and-future-developments)
   * [Running the program](#how-to-run)

## Overview
This is a Haskell implementation of Texas Hold'em created for my COM2108 (Functional Programming) university module. This project includes a working Texas HoldEm Poker game with additional features such as predefined player strategies (random, passive, aggressive) and an option for human input to play the game. There is also extensive documentation outlining my design, choices for development, and iterative testing to ensure functionality and address edge cases.

## Core features
- Simulation of a Poker game with different player strategies:
  - RandomPlayer: Makes random decisions
  - PassivePlayer: Only checks, calls, or folds
  - AggressivePlayer: Frequently bets and raises, only calling and folding if necessary, and never checks.
- Functionality to simulate all streets of a round, including:
  - Deck initialisation and randomised shuffling
  - Hand evaluation and ranking (Royal Flush, Full House, etc.)
  - Betting mechanics including checks, folds, calls, bets, and raises
  - Determination of winners from hands including tie-breaker logic
- An option to play a game using the command line; can be done as a combination of human and AI players.

## Limitations and future developments
Some aspects of the project were not fully fleshed out, namely the human input feature which could use more UX polishing. Additionally, some features were not able to implemented due to time constraints, such as a SmartPlayer strategy which would involve a player making informed decisions using probabilities and statistics to calculate moves.

## Running the program
### Default game
The default game includes a RandomPlayer, PassivePlayer, AggressivePlayer, and one HumanPlayer, requring human input.

1) Clone the repository and ensure Haskell and required libraries (e.g., `System.Random`) are installed.
2) Compile the `TexasHoldEm.hs` file using GHC:

   ```bash
   ghc -o TexasHoldEm TexasHoldEm.hs
3) Run the executable:
   ```bash
   ./TexasHoldEm
4) Follow the command-line prompts to play the game.

### Custom game
To create a custom game with different player strategies, starting game state (i.e., different number of starting chips, number of players, current bets, etc.) or the number of rounds, modify `main` within the code.
