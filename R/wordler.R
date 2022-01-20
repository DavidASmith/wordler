
#' Assess a guess against the target word
#'
#' @param game Wordler game object.
#'
#' @return Character vector of guess assessment.
assess_guess <- function(game){
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

#' Check if current guess complies with hard_mode rules
#'
#' @param guess The guess
#' @param game Wordler game object.
#' @return bool
check_guess_hard_mode <- function(guess, game) {
  if (!game$hard_mode) {
    return(TRUE)
  }
  if (game$guess_count == 0) {
    return(TRUE)
  }
  last_assess <- game$assess[[game$guess_count]]
  last_guess <- game$guess[[game$guess_count]]
  # Require in_position letters in last guess to match guess
  in_position <- last_assess == "in_position"
  guess_match <- last_guess == guess
  if (!all(ifelse(in_position, guess_match, TRUE))) {
    wrong <- !guess_match & in_position
    mapply(function(wrong, index) {
      if (wrong) {
        message(get_ordinal(index), " letter must be '", last_guess[[index]], "'")
      }
    },
    wrong, seq_along(wrong)
    )
    return(FALSE)
  }
  # Require in_word letters in last guess to be in guess
  in_word <- last_assess == "in_word"
  in_guess <- guess %in% last_guess
  if (!all(ifelse(in_word, in_guess, TRUE))) {
    wrong <- !in_guess & in_word
    mapply(function(wrong, index) {
      if (wrong) {
        message("'", last_guess[[index]], "' must be in word")
      }
    },
    wrong, seq_along(wrong)
    )
    return(FALSE)
  }

  # Else all good
  return(TRUE)
}

get_ordinal <- function(x) {
  switch(x, "1st", "2nd", "3rd", "4th", "5th")
}


#' Play a game of Wordle in the R console
#'
#' @param hard_mode Flag if hard mode should be used. In hard mode each letter
#'   in a correct position must be in the same position in future guesses, and
#'   any letters in the word must be used in future guesses.
#'
#' @export
play_wordler <- function(hard_mode = FALSE){

  print_instructions()

  # Create a new game
  game <- make_new_game(hard_mode)

  while(!game$game_over){
    print_game(game)

    # Ask player to guess a word
    new_guess <- readline("Enter a word: ")
    new_guess <- toupper(new_guess)

    # Make guess
    game <- have_a_guess(new_guess, game)

    # Has the player guessed correctly?
    if(game$game_won){
      print_game(game)
      cat("Congratulations, you won!")
      next()
    }

    # Are all the guesses used up
    if(game$guess_count == 6){
      print_game(game)
      cat("You have used all your guesses.\n")
      cat("The word you were looking for is", game$target)
    }
  }
}

#' Make a new blank wordle game
#'
#' @param hard_mode Flag if game is in hard mode
#' @return A list representing the Wordle game.
#'
#' @export
#'
#' @examples
make_new_game <- function(hard_mode = FALSE) {
  new_game <- list(
    hard_mode = hard_mode,
    game_over = FALSE,
    game_won = FALSE,
    guess_count = 0,
    target = sample(wordler::ubuntu_dict, 1),
    guess = list(
      unlist(strsplit("_____", "")),
      unlist(strsplit("_____", "")),
      unlist(strsplit("_____", "")),
      unlist(strsplit("_____", "")),
      unlist(strsplit("_____", "")),
      unlist(strsplit("_____", ""))
    ),
    assess = list(
      rep("not_in_word", 5),
      rep("not_in_word", 5),
      rep("not_in_word", 5),
      rep("not_in_word", 5),
      rep("not_in_word", 5),
      rep("not_in_word", 5)
    ),
    keyboard = list(
      row1 = c("Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P"),
      row2 = c("A", "S", "D", "F", "G", "H", "J", "K", "L"),
      row3 = c("Z", "X", "C", "V", "B", "N", "M")
    )
  )
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
#'
#' @return A wordler game object.
#' @export
#'
#' @examples
have_a_guess <- function(x, game) {

  # Game must not be already over
  if (game$game_over) {
    message("The game is already over. Start a new one if you want to play again.")
  }

  # Guess must be in word list
  if (!(x %in% wordler::ubuntu_dict)) {
    message("Your word isn't in the list of valid words. Try again.")
    return(game)
  }


  if (!check_guess_hard_mode(unlist(strsplit(x, "")), game)) {
    return(game)
  }


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
  if (game$guess_count == 6) {
    game$game_over <- TRUE
  }
  game
}

#' Prints instructions to play a wordler game in the console
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
#' @export
#'
#' @examples
print_game <- function(x){

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

  letters_known_in_word <- mapply(function(guess, assess) guess[assess == "in_word"],
                                  guess = game$guess,
                                  assess = game$assess)
  letters_known_in_word <- unlist(letters_known_in_word)
  letters_known_in_word <- unique(letters_known_in_word)

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
  letters_known_not_in_word <- mapply(function(guess, assess) guess[assess == "not_in_word"],
                                      guess = game$guess,
                                      assess = game$assess)
  letters_known_not_in_word <- unlist(letters_known_not_in_word)
  letters_known_not_in_word <- unique(letters_known_not_in_word)

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
