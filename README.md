# Connect-4

This is an implementation of Connect-4 in Racket. The game features a single-player mode in which you can play against the computer. The AI for this uses a Minimax Algorithm with Alpha-beta pruning.

# How to run

1. Install racket from http://racket-lang.org/ and ensure racket is available in your PATH
2. Run the racket program by executing ```racket main.rkt``` from the project folder
3. Follow instructions on the screen to play the game

# Limitations:

In single-player mode, the AI evaluates only up to four future moves of the player. This could be increased but will result in a noticeable delay.

The heuristic function used only looks at the count of adjacent coins and does not consider complex strategies/tactics.