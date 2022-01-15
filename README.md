<!-- badges: start -->
[![R-CMD-check](https://github.com/DavidASmith/wordler/workflows/R-CMD-check/badge.svg)](https://github.com/DavidASmith/wordler/actions)
<!-- badges: end -->

# wordler

An R implementation of the popular 
[WORDLE](https://www.powerlanguage.co.uk/wordle/) game.

## Gameplay

To start a game, run the following R code in an interactive session.

```{r}
library(wordler)
play_wordler()
```

The introductory instructions are displayed.

You must then attempt to correctly guess a five-letter word in (at most) six 
attempts.

After each guess, the letters are coloured to indicate how well the guess 
matches the target word.

Green letters are in the word _and_ in the right position (but may be repeated 
elsewhere). Yellow letters are present (at least once) somewhere in the target 
word.

Each guess must be a valid word.

## Installation

You can install directly from github using the devtools R package.

```{r}
# install.packages("devtools")
devtools::install_github("DavidASmith/wordler")
```
