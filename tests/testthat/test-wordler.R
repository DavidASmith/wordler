test_that("Repeated guess letters are compared to target without replacement", {
  game <- make_new_game()
  game$target <- "SOLAR"
  first_letter_in_pos <- have_a_guess("SOULS", game, allowed_words = c("SOULS"))
  # The repeated `S` should not be counted as in the target
  expect_equal(first_letter_in_pos$assess[[1]][[5]], "not_in_word")
  game$target <- "POURS"
  last_letter_in_pos <- have_a_guess("CLASS", game, allowed_words = c("CLASS"))
  # The first `S` should not be counted as in the target
  expect_equal(last_letter_in_pos$assess[[1]][[4]], "not_in_word")
})

test_that("Custom dictionary used for target words", {
  # Words unlikely to be in any default dictionary
  custom_dict <- c("XXXXX", "ZZZZZ")
  game <- make_new_game(target_words = custom_dict)
  expect_true(game$target %in% custom_dict)
})

test_that("Custom dictionary used to validate guess", {
  # Words unlikely to be in any default dictionary
  game <- make_new_game(target_words = c("XXXXX"))
  game <- have_a_guess("ZZZZZ", game, "ZZZZZ")
  expect_equal(game$guess_count, 1)
  game <- have_a_guess("ZZZZZ", game, "XXXXX")
  expect_equal(game$guess_count, 1)
})
