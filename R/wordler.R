
#' Assess a guess against the target word
#'
#' @param game Wordler game object.
#'
#' @return Character vector of guess assessment.
  assess_guess <- function(game){

  # Confirm wordler object
  if(!is.wordler(game)){
    stop("`game` argument must be a wordler object.")
  }

  # Get required items from game object
  guess <- game$guess[[game$guess_count]]
  target <- unlist(strsplit(game$target, ""))

  # Do letters match position in target word?
  in_position <- guess == target

  # Letters that aren't in position can still be in word
  to_check <- guess[!in_position]

  # Lookup counts of remaining  guess letters in target word
  lookup <- count_freqs(to_check, target[!in_position])

  # We only count as many occurrences of a guess letter as
  # are in the lookup as being in the word
  in_word <- mapply(
    function(idx, l) {
      if (in_position[idx]) return(TRUE)
      if (sum(lookup[[l]]) > 0) {
        lookup[[l]] <<- lookup[[l]] - 1L
        return(TRUE)
      }
      else FALSE
    },
    idx = 1:length(guess),
    l = unlist(strsplit(guess, ""))
  )

  # Build assessment vector
  assessment <- ifelse(in_word, "in_word", "not_in_word")
  assessment <- ifelse(in_position, "in_position", assessment)

  # Add assessment to game object and return
  game$assess[[game$guess_count]] <- assessment
  game
}

#' Get counts of each letter in the target
#'
#' @param xs,target we count the occurrences of each element in
#'     \code{xs} in \code{target}
#' @return named list of elements of \code{xs} with counts
count_freqs <- function(xs, target) {
  xs <- unique(xs)
  names(xs) <- xs
  lapply(xs, function(x) sum(target == x))
}


#' Play a game of Wordle in the R console
#'
#' @param target_words A character vector of potential target words for the
#' game. A word will be randomly selected from this vector as the target word
#' to be guessed. Defaults to words used by the WORDLE game online
#' (?wordler::wordle_answers) if not provided.
#' @param allowed_words A character vector of valid words for the guess. Guess
#' must be in this vector to be allowed. Defaults to words used by the WORDLE
#' game online (?wordler::wordle_allowed) if not provided.
#'
#' @return No return value. Starts interactive game in console.
#'
#' @export
play_wordler <- function(target_words = NULL, allowed_words = NULL){

  print_instructions()

  # Establish default target words if none provided
  if(is.null(target_words)){
    target_words <- wordler::wordle_answers
  }

  # Establish default allowed words if none provided
  if(is.null(allowed_words)){
    allowed_words <- c(wordler::wordle_allowed,
                       wordler::wordle_answers
    )
  }

  # Create a new game
  game <- new_wordler()

  while(!game$game_over){
    print(game)

    # Ask player to guess a word
    new_guess <- readline("Enter a word: ")
    new_guess <- toupper(new_guess)

    # Make guess
    game <- have_a_guess(new_guess,
                         game,
                         allowed_words)

    # Has the player guessed correctly?
    if(game$game_won){
      print(game)
      cat("Congratulations, you won!")
      next()
    }

    # Are all the guesses used up
    if(game$guess_count == 6){
      print(game)
      cat("You have used all your guesses.\n")
      cat("The word you were looking for is", game$target)
    }
  }
}

#' Constructs a new object of class "wordler"
#'
#' Returns a "wordler" object which holds the state of a wordle game as guesses
#' are made. The returned object will have a target word which is selected from
#' the default list unless provided in the \code{target_words} argument.
#'
#' The wordler object is a list which has the following elements:
#'
#' \itemize{
#'   \item \code{game_over} - A logical indicating if the game is over. Set to
#'   \code{TRUE} if either the word is correctly guessed, or all guesses are
#'   used.
#'   \item \code{game_won}
#'   \item \code{guess_count}
#'   \item \code{target}
#'   \item \code{guess}
#'   \item \code{assess}
#'   \item \code{keyboard}
#' }
#'
#' @param target The target word for the game. Defaults to a random selection
#' from words used by the WORDLE game online (?wordler::wordle_answers) if not
#' provided.
#' @param game_over A logical indicating if the game is over. Defaults to FALSE.
#' @param game_won A logical indicating if the game has been won. In other
#' words, has the target word been correctly guessed.
#' @param guess_count An integer representing the number of guesses made so
#' far. Defaults to 0.
#' @param guess A list (of length 6) of character vectors (each of length 5)
#' representing the guesses of the target word. Each element of the list
#' represents one of six guesses allowed. Each guess defaults to
#' \code{c("_", "_", "_", "_", "_")} to represent a guess not yet made.
#' @param assess A list (of length 6) of character vectors (each of length 5)
#' representing an assessment of each letter in each guess.
#' @param keyboard A list (of length 3) of character vectors each representing
#' a row of a keyboard layout used to visualise the game by \code{print()}.
#' Defaults to QWERTY layout.
#' @param letters_known_not_in_word A character vector of letters known not to
#' be in the target word.
#' @param letters_known_in_word A character vector of letters know to be in the
#' target word.
#' @param letters_known_in_position A character vector of letters known to be
#' in the correct position in the target word.
#'
#' @return An object of class "wordler".
#' @export
#'
#' @examples
new_wordler <- function(target = sample(wordler::wordle_answers, 1),
                        game_over = FALSE,
                        game_won = FALSE,
                        guess_count = 0,
                        guess = lapply(1:6, function(x) unlist(strsplit("_____", ""))),
                        assess = lapply(1:6, function(x) rep("not_in_word", 5)),
                        keyboard = wordler::keyboards$qwerty,
                        letters_known_not_in_word = character(0),
                        letters_known_in_word = character(0),
                        letters_known_in_position = character(0)){

  # Validate target argument
  if(class(target) != "character"){
    stop("`target` must be of class 'character'")
  }
  if(nchar(target) != 5){
    stop("`target` must have exactly 5 characters")
  }
  if(length(target) != 1){
    stop("`target` must be a character vector of length 1")
  }

  # Validate logical arguments
  if(class(game_over) != "logical" | class(game_won) != "logical"){
    stop("`game_over` and `game_won` must both be of class 'logical'")
  }

  # Validate guess
  if(class(guess) != list() |
     length(guess != 6) |
     !all(unlist(lapply(guess, function(x) length(x) == 5)))){
     stop("`guess` must be a list with six items, each of which is a character vector of length 5")
  }

  # Validate assess
  if(class(assess) != list() |
     length(assess != 6) |
     !all(unlist(lapply(assess, function(x) length(x) == 5)))){
    stop("`assess` must be a list with six items, each of which is a character vector of length 5")
  }

  # Validate keyboard
  if(class(assess) != list() |
     length(assess != 6)){
    stop("`keyboard` must be a list with three items")
  }

  # Validate letters in word vectors
  if(class(letters_known_not_in_word) != "character"){

  }

  # Build list to represent game state
  wordler <- list(target = target,
                  game_over = game_over,
                  game_won = game_won,
                  guess_count = guess_count,
                  guess = guess,
                  assess = assess,
                  keyboard = keyboard,
                  letters_known_not_in_word = letters_known_not_in_word,
                  letters_known_in_word = letters_known_in_word,
                  letters_known_in_position = letters_known_in_position)

  # Set class and return
  class(wordler) <- "wordler"
  wordler
}


#' Establish if guess is correct and set game state accordingly
#'
#' @param game A wordler game object.
#'
#' @return A wordler game object.
#'
#' @examples
is_guess_correct <- function(game){

  # Get required items from game object
  guess <- game$guess[[game$guess_count]]
  target <- unlist(strsplit(game$target, ""))

  # Set game state if guess is correct
  if(all(guess == target)){
    game$game_over <- TRUE
    game$game_won <- TRUE
  }

  game
}


#' Submit a guess word to the wordler game object
#'
#' @param x The guess.
#' @param game The wordler game object.
#' @param allowed_words A character vector of valid words for the guess. Guess
#' must be in this vector to be allowed. Defaults to words used by the WORDLE
#' game online (?wordler::wordle_allowed) if not provided.
#'
#' @return A wordler game object.
#' @export
#'
#' @examples
have_a_guess <- function(x, game, allowed_words = NULL){

  # Game must not be already over
  if(game$game_over){
    message("The game is already over. Start a new one if you want to play again.")
    return(game)
  }

  # Default allowed_words
  if(is.null(allowed_words)){
    allowed_words <- c(wordle_answers, wordle_allowed)
  }

  # Guess must be in word list
  if(!(x %in% allowed_words)){
    message("Your word isn't in the list of valid words. Try again.")
  } else {

    # Player has used a guess
    game$guess_count <- game$guess_count + 1

    # Add guess to game
    game$guess[[game$guess_count]] <- unlist(strsplit(x, ""))

    # Assess guess
    game <- assess_guess(game)

    # Update known letters
    game <- update_letters_known_not_in_word(game)
    game <- update_letters_known_in_word(game)
    game <- update_letters_known_in_position(game)

    # Is guess correct?
    game <- is_guess_correct(game)

    # Are guesses all used?
    if(game$guess_count == 6){
      game$game_over <- TRUE
    }
  }
  game
}

#' Prints instructions to play a wordler game in the console
#'
#' @return No return value.
#'
#' @examples
print_instructions <- function(){
  # Introductory instructions
  cat("Guess the WORDLE in 6 tries.\n\n")
  cat("After each guess, the color of the letters will change to show how close your guess was to the word. e.g.\n\n")
  cat(crayon::green("W"), "E A R Y\n")
  cat("The letter W is in the word and in the correct spot\n\n")
  cat("P I", crayon::yellow("L"), "O T\n")
  cat("The letter L is in the word but in the wrong spot\n\n")
  cat("V A G U E\n")
  cat("None of the letters are in the word\n\n")
}

#' Prints a wordler game to the console.
#'
#' @param x A wordler game object.
#'
#' @return No return value.
#'
#' @export
#'
#' @examples
print.wordler <- function(x){

  game <- x
  keyboard <- game$keyboard

  # Determine which letters are known to be in word, not in word, or in position
  keyboard_letter_not_in_word <-
    lapply(game$keyboard, function(x) x %in% game$letters_known_not_in_word)
  keyboard_letter_in_word <-
    lapply(game$keyboard, function(x) x %in% game$letters_known_in_word)
  keyboard_letter_in_position <-
    lapply(game$keyboard, function(x) x %in% game$letters_known_in_position)

  # Print game state to console
  cat("\n")
  # Loop through all guesses
  for (i in 1:6) {
    cat("    ")
    # Loop through letters in each guess
    for(j in 1:5){
      if(game$assess[[i]][j] == "in_position"){
        cat(crayon::green$bold(game$guess[[i]][j]))
      } else if (game$assess[[i]][j] == "in_word") {
        cat(crayon::yellow$bold(game$guess[[i]][j]))
      } else {
        cat(crayon::bold(game$guess[[i]][j]))
      }
      cat(" ")
    }
    if(i == 2){
      # Display top row of keyboard
      cat("     ")
      for(j in seq_along(keyboard[[1]])){
        if(keyboard_letter_in_position[[1]][j]){
          cat(crayon::green(keyboard[[1]][j], " "))
        } else  if (keyboard_letter_in_word[[1]][j]){
          cat(crayon::yellow(keyboard[[1]][j], " "))
        } else  if (keyboard_letter_not_in_word[[1]][j]){
          cat(crayon::yellow(" ", " "))
        } else {
          cat(keyboard[[1]][j], " ")
        }
      }
    }
    if(i == 3){
      # Display middle row of keyboard
      cat("       ")
      for(j in seq_along(keyboard[[2]])){
        if(keyboard_letter_in_position[[2]][j]){
          cat(crayon::green(keyboard[[2]][j], " "))
        } else  if (keyboard_letter_in_word[[2]][j]){
          cat(crayon::yellow(keyboard[[2]][j], " "))
        } else  if (keyboard_letter_not_in_word[[2]][j]){
          cat(crayon::yellow(" ", " "))
        } else {
          cat(keyboard[[2]][j], " ")
        }
      }
    }
    if(i == 4){
      # Display bottom row of keyboard
      cat("         ")
      for(j in seq_along(keyboard[[3]])){
        if(keyboard_letter_in_position[[3]][j]){
          cat(crayon::green(keyboard[[3]][j], " "))
        } else  if (keyboard_letter_in_word[[3]][j]){
          cat(crayon::yellow(keyboard[[3]][j], " "))
        } else  if (keyboard_letter_not_in_word[[3]][j]){
          cat(crayon::yellow(" ", " "))
        } else {
          cat(keyboard[[3]][j], " ")
        }
      }
    }
    cat("\n")
  }
  cat("\n")

}


#' Establish which letters are known to be in the target word
#'
#' @param game A wordler game object.
#'
#' @return A wordler game object.
#'
#' @examples
update_letters_known_in_word <- function(game){
  # Target word represented as character vector
  target <- unlist(strsplit(game$target, ""))

  # Establish letters from all guesses which are in target word
  letters_known_in_word <- lapply(game$guess, function(x) x[x %in% target])
  letters_known_in_word <- unique(unlist(letters_known_in_word))

  # Add to game object and return
  game$letters_known_in_word <- letters_known_in_word
  game
}

#' Establish which letters are known to _not_ be in the target word
#'
#' @param game A wordler game object.
#'
#' @return A wordler game object.
#'
#' @examples
update_letters_known_not_in_word <- function(game){

  # Target word represented as character vector
  target <- unlist(strsplit(game$target, ""))

  # Establish letters from all guesses which are not in target word
  letters_known_not_in_word <- lapply(game$guess, function(x) x[!x %in% target])
  letters_known_not_in_word <- unique(unlist(letters_known_not_in_word))
  # Remove underscore (used as blanks for guesses not yet made)
  letters_known_not_in_word <- letters_known_not_in_word[!letters_known_not_in_word == "_"]

  # Add to game object and return
  game$letters_known_not_in_word <- letters_known_not_in_word
  game
}

#' Establish which letters are known to be in the correct position in the target
#' word
#'
#' @param game A wordler game object.
#'
#' @return A wordler game object.
#'
#' @examples
update_letters_known_in_position <- function(game){

  letters_known_in_position <- mapply(function(guess, assess) guess[assess == "in_position"],
                                      guess = game$guess,
                                      assess = game$assess)
  letters_known_in_position <- unlist(letters_known_in_position)
  letters_known_in_position <- unique(letters_known_in_position)

  game$letters_known_in_position <- letters_known_in_position

  game
}

#' Detects wordler objects
#'
#' @param x An R object
#' @param ... Additional arguments
#'
#' @return Returns TRUE if x is a wordler object, otherwise FALSE.
#' @export
#'
#' @examples
is.wordler <- function(x, ...) {
  class(x) == "wordler"
}
