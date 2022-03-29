library(qdapDictionaries)
library(stringr)

# Get all five letter words from qdap DICTIONARY --------------------------

qdap_dict <- DICTIONARY$word
qdap_dict <- qdap_dict[nchar(qdap_dict) == 5]
qdap_dict <- toupper(qdap_dict)
qdap_dict


# Get all words from Ubuntu dictionary ------------------------------------

ubuntu_dict <- readLines("/usr/share/dict/words")
ubuntu_dict <- ubuntu_dict[!str_detect(ubuntu_dict, fixed("'"))]
ubuntu_dict <- ubuntu_dict[!str_detect(ubuntu_dict, "[A-Z]")]
ubuntu_dict <- ubuntu_dict[!str_detect(ubuntu_dict, "é")]
ubuntu_dict <- ubuntu_dict[!str_detect(ubuntu_dict, "ê")]
ubuntu_dict <- ubuntu_dict[!str_detect(ubuntu_dict, "ó")]
ubuntu_dict <- ubuntu_dict[nchar(ubuntu_dict) == 5]
ubuntu_dict <- toupper(ubuntu_dict)
ubuntu_dict


# Load words used by original WORDLE game ---------------------------------

wordle_answers <- toupper(readLines("data-raw/wordle-answers-alphabetical.txt"))
wordle_allowed <- toupper(readLines("data-raw/wordle-allowed-guesses.txt"))



# Keyboard layouts for printing game to console

keyboards = list(qwerty = list(row1 = c("Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P"),
                           row2 = c("A", "S", "D", "F", "G", "H", "J", "K", "L"),
                           row3 = c("Z", "X", "C", "V", "B", "N", "M"))
                 )


# Add to package external data --------------------------------------------
usethis::use_data(qdap_dict,
                  ubuntu_dict,
                  wordle_answers,
                  wordle_allowed,
                  keyboards,
                  overwrite = TRUE)
