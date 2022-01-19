test_that("Repeated guess letters are compared to target without replacement", {
  game <- make_new_game()
  game$target <- "SOLAR"
  first_letter_in_pos <- have_a_guess("SOULS", game)
  # The repeated `S` should not be counted as in the target
  expect_equal(first_letter_in_pos$assess[[1]][[5]], "not_in_word")
  game$target <- "POURS"
  last_letter_in_pos <- have_a_guess("CLASS", game)
  # The first `S` should not be counted as in the target
  expect_equal(last_letter_in_pos$assess[[1]][[4]], "not_in_word")
})
