# policy errors show the public argument and reject abbreviations

    Code
      read_qualtrics("unused.csv", preview = "ex")
    Condition
      Error in `read_qualtrics()`:
      ! `preview` must be one of "error", "exclude", or "include", not "ex".
      i Did you mean "exclude"?

