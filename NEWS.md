# Wordler 0.3.1
- Updates for CRAN resubmission:
    - The list representing game state now has an S3 class - "wordler".
    - All exported functions now have a documented return value.
    - All functions which accept and return a "wordler" object are documented 
    regarding their effects on the "wordler" object.
    - Documented the wordler class (in the constructor `new_wordler()`).
- Fixed issue with detection of letters known to be in, or known not to be in, 
  the target word.
- `make_new_game()` superseded by `new_wordler()`. 

# Wordler 0.3.0
- Updates for CRAN resubmission:
    - URL in DESCRIPTION enclosed in angle brackets.
- Default word list is now that used by the original wordle game.
- User can specify word list to be used to both select a target and validate 
  guesses.  

# Wordler 0.2.1
- Updates to support CRAN resubmission:
    - Use canonical form of URL for link to qdapDictionaries package on CRAN.
    - Added link to original game in package DESCRIPTION.
- Repeated letters in a guess are now only counted as being 'in_word' as many 
  times as the letter appears in the word. See example at
  https://www.reddit.com/r/wordle/comments/ry49ne/illustration_of_what_happens_when_your_guess_has/

# wordler 0.2.0

- Game is now playable programmatically, rather than via console only.
- Added keyboard style display of letters known to be in word, known to be in 
  right position, or known not to be in the word.

# wordler 0.1.0

- Initial release.
