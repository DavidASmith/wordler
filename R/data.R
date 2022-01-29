#' All five-letter words from the Nettalk Corpus Syllable Data Set.
#'
#' A dataset containing all five-letter words from the Nettalk Corpus Syllable
#' Data Set as returned by qdapDictionaries::dictionaries().
#'
#' @format A character vector of length 2488.
#' @source \url{https://CRAN.R-project.org/package=qdapDictionaries/}
"qdap_dict"

#' All five-letter words from the Ubuntu dictionary.
#'
#' A dataset containing all five-letter words from Ubuntu dictionary
#' `/usr/share/dict/words`.
#'
#' @format A character vector of length 4594.
#' @source \url{https://ubuntu.com/}
"ubuntu_dict"

#' All words used as potential answers by the original WORDLE game.
#'
#' A dataset containing all words which can be used as answers to the original
#' WORDLE game.
#'
#' @format A character vector of length 2315.
#' @source \url{https://gist.github.com/cfreshman/a03ef2cba789d8cf00c08f767e0fad7b/}
"wordle_answers"

#' All words used to validate guesses by the original WORDLE game.
#'
#' A dataset containing all words which are used to validate guesses by the
#' original WORDLE game. Note that this does not include the words which can be
#' answers. Theses are held in ?wordle_answers.
#'
#' @format A character vector of length 10657.
#' @source \url{https://gist.github.com/cfreshman/cdcdf777450c5b5301e439061d29694c}
"wordle_allowed"

#' Keyboard layouts for printing a wordler game at the console.
#'
#' A list of keyboard layouts used to show letters known to be not in target
#' word, in the target word, and in the right position in the target word.
#' Each element must be a list having 3 items, each representing a row of a
#' keyboard layout.
#'
#' @format A list of length 1.
#' @source \url{https://gist.github.com/cfreshman/cdcdf777450c5b5301e439061d29694c}
"keyboards"
