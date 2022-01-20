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

test_that("check_guess_hard_mode works", {
  game <- make_new_game(TRUE)
  # First guess always valid
  expect_true(check_guess_hard_mode(rep("A", 5), game))
  game$guess[[1]] <- c("S", "O", "U", "L", "S")
  game$assess[[1]] <- c("in_word", "in_position", rep("not_in_word", 3))
  game$guess_count <- 1
  # Invalid as O not in_position
  expect_false(check_guess_hard_mode(c("S", "U", "O", "L", "S"), game))
  expect_message(
    check_guess_hard_mode(c("S", "U", "O", "L", "S"), game),
    "2nd letter must be 'O'",
  )
  # Invalid as missing an S
  expect_false(check_guess_hard_mode(c("F", "O", "I", "L", "F"), game))
  expect_message(
    check_guess_hard_mode(c("F", "O", "I", "L", "F"), game),
    "'S' must be in word"
  )
  # Valid guess
  expect_true(check_guess_hard_mode(c("T", "O", "A", "S", "T"), game))
})
