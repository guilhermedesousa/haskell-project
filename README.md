# Shogi Game

This is a command-line version of the traditional Japanese chess game **Shogi**, implemented in Haskell. The game is executed using the `stack` build tool.

![alt text](https://github.com/guilhermedesousa/shogi-haskell/blob/main/shogi-game-board.png)

## Table of Contents

- [Installation](#installation)
- [How to Play](#how-to-play)
- [Credits](#credits)

## Installation

Before running the Shogi game, make sure you have GHC compiler, language server ([Haskell Language Server - HLS](https://github.com/haskell/haskell-language-server)), and [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/) installed. The whole environment can be installed using a single tool name [GHCup](https://www.haskell.org/ghcup/).

### Clone the Repository

Clone the project to your local machine:

```bash
git clone https://github.com/guilhermedesousa/shogi-haskell
cd shogi-haskell
```

## How to Play

This is a text-based Shogi game, where you input commands to move pieces, replace captured pieces, or end the game.

### Move pieces

To move a piece, you must type valid source and destination positions. For example, if you wish to move a piece from row `7` and column `a` to row `6` and column `a`, you should input `7a 6a`.

### Replace pieces

To replace a piece, type `repor` and press `Enter` when prompted to input a move. If you have pieces to replace, you will then need to input the piece's number and the destination position. For example, if you want to replace piece number `2` in cell `6e`, you should input `2 6e`.

### End the game

If you wish to end the game, simply type `sair` at any time.

## Credits

This Shogi game was created by [@AlencarLima](https://github.com/AlencarLima) and [@guilhermedesousa](https://github.com/guilhermedesousa). It is a simplified version of the traditional Japanese board game, built for learning purposes and to practice Haskell.

Feel free to contribute or report any issues.