
#' Assess a guess against the target word
#'
#' @param guess The guess. Character vector of length 1.
#' @param word The target word. Single row character matrix.
#'
#' @return Character vector of guess assessment.
assess_guess <- function(guess, word){

  in_word <- unlist(lapply(as.vector(guess), function(x) x %in% unlist(strsplit(word, ""))))
  in_position <- as.vector(guess) == unlist(strsplit(word, ""))

  out <- ifelse(in_word, "in_word", "not_in_word")
  out <- ifelse(in_position, "in_position", out)
  out

}

#' Print current guesses to console
#'
#' @param guess_matrix Matrix of guesses.
#' @param assess_matrix Matrix of assessment of the guesses.
#'
#' @importFrom crayon green yellow bold
print_guesses <- function(guess_matrix, assess_matrix){
  cat("\n")
  for (i in 1:6) {
    cat("    ")
    for(j in 1:5){
      if(assess_matrix[i, j] == "in_position"){
        cat(crayon::green$bold(guess_matrix[i, j]))
      } else if (assess_matrix[i, j] == "in_word") {
        cat(crayon::yellow$bold(guess_matrix[i, j]))
      } else {
        cat(crayon::bold(guess_matrix[i, j]))
      }
      cat(" ")
    }
    cat("\n")
  }
  cat("\n")
}

#' Makes an empty matrix to hold guesses
#'
#' @return Empty guess matrix.
make_empty_guess_matrix <- function() {
  matrix(rep("_", 30), ncol = 5)
}

#' Makes an empty matrix to hold assessment of guesses
#'
#' @return Empty guess assessment matrix.
make_empty_assess_matrix <- function() {
  matrix(rep("not_in_word", 30), ncol = 5)
}


#' Play a game of Wordle in R
#'
#' @export
play_wordler <- function(){

  # Select a target word
  all_words <- qdap_dict
  word <- sample(all_words, 1)

  # Initialise game state
  game_over <- FALSE
  guess_num <- 1

  # Matrices to hold guesses
  guess_matrix <- make_empty_guess_matrix()
  # ... and the assessment of the guesses
  assess_matrix <- make_empty_assess_matrix()

  # Introductory instructions
  cat("Guess the WORDLE in 6 tries.\n\n")
  cat("After each guess, the color of the letters will change to show how close your guess was to the word. e.g.\n\n")
  cat(crayon::green("W"), "E A R Y\n")
  cat("The letter W is in the word and in the correct spot\n\n")
  cat("P I", crayon::yellow("L"), "O T\n")
  cat("The letter L is in the word but in the wrong spot\n\n")
  cat("V A G U E\n")
  cat("None of the letters are in the word\n\n")

  # Keep asking for guesses until game over
  while(!game_over){

    # Print the current guess history
    print_guesses(guess_matrix, assess_matrix)

    # End if all guesses are used up
    if(guess_num > 6){
      message("You have used all your guesses but have not solved the WORDLE.")
      message("The word you were looking for is ", word)
      game_over <- TRUE
      next
    }

    # Ask player to guess a word
    new_guess <- readline("Enter a word: ")
    new_guess <- toupper(new_guess)

    # Guess must be 5 letters long
    if(nchar(new_guess) != 5){
      message("Your word must have five letters. Try again.")
      next
    }

    # Guess must be in word list
    if(!(new_guess %in% all_words)){
      message("Your word isn't in the list of valid words. Try again.")
      next
    }

    # Is the guess correct?
    guess_is_right <- new_guess == word

    # Add guess to matrix
    new_guess <- unlist(strsplit(new_guess, ""))
    new_guess <- matrix(new_guess, nrow = 1)
    guess_matrix[guess_num, ] <- new_guess

    # Add assessment of guess to matrix
    assess_matrix[guess_num, ] <- matrix(assess_guess(new_guess, word), nrow = 1)

    # Did they win?
    if(guess_is_right){
      print_guesses(guess_matrix, assess_matrix)
      message(paste0("Congratulations. You got in in ", guess_num, " attempts."))
      game_over <- TRUE
      next
    }

    # Player has used another guess
    guess_num <- guess_num + 1
  }
}
