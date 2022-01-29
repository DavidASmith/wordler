test_that("Repeated guess letters are compared to target without replacement", {
  game <- new_wordler()
  game$target <- "SOLAR"
  first_letter_in_pos <- have_a_guess("SOULS", game, allowed_words = c("SOULS"))
  # The repeated `S` should not be counted as in the target
  expect_equal(first_letter_in_pos$assess[[1]][[5]], "not_in_word")
  game$target <- "POURS"
  last_letter_in_pos <- have_a_guess("CLASS", game, allowed_words = c("CLASS"))
  # The first `S` should not be counted as in the target
  expect_equal(last_letter_in_pos$assess[[1]][[4]], "not_in_word")
})

test_that("Custom dictionary used to validate guess", {
  # Words unlikely to be in any default dictionary
  game <- new_wordler(target = "XXXXX")
  game <- have_a_guess("ZZZZZ", game, "ZZZZZ")
  expect_equal(game$guess_count, 1)
  game <- have_a_guess("ZZZZZ", game, "XXXXX")
  expect_equal(game$guess_count, 1)
})

test_that("Letters known not to be in word are correctly identified", {
  game <- new_wordler(target = "STAMP")
  game <- have_a_guess("TROUT", game)
  expect_equal(game$letters_known_not_in_word, c("R", "O", "U"))
  game <- have_a_guess("STAGE", game)
  expect_equal(game$letters_known_not_in_word, c("R", "O", "U", "G", "E"))
})
