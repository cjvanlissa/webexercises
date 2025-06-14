# webexercises 2.0.1

* Add support for webexercises in package vignettes
* Add support for PDF versions of webexercises

# webexercises 1.1.0

* quarto support (`create_quarto_doc()` and `add_to_quarto()`)
* fenced .webex-check sections for self-checking quizzes
* webex.hide knit hooks get set up .onLoad instead of in webex.R
* remove js dependency on jquery
* `total_correct()` deprecated
* PDF rendering is better (but webexercises is really built for HTML)

# webexercises 1.0.0

* new package name: webexercises
* updated method to remove smart quotes when rmarkdown >= 2.2
* new styles for correct and incorrect answers
* `longmcq()` function for MCQs with long answers (creates a radiobutton interface)
* `add_webex_to_bookdown()` new function to add helper functions to bookdown books

# webex 0.9.2

## Bug fixes

* (#12, @debruine) updated css to prevent a conflict with the new bookdown
* (#11, @Benjou) MCQs where correct answer contains an apostrophe now
  parsed correctly
  
## Misc

* added `regex` argument example to `fitb()` on the template
* fixed `ignore_ws` argument example in the template
* removed note about need to "Open in Browser" since RStudio now
  includes a JS-enabled browser
* Added this `NEWS.md` file to track changes to the package.

# webex 0.9.1

* On CRAN!
