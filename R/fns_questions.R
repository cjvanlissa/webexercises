#' Create a fill-in-the-blank question
#'
#' @param answer The correct answer (can be a vector if there is more
#'   than one correct answer).
#' @param width Width of the input box in characters. Defaults to the
#'   length of the longest answer.
#' @param num Whether the input is numeric, in which case allow for
#'   leading zeroes to be omitted. Determined from the answer data
#'   type if not specified.
#' @param tol The tolerance within which numeric answers will be
#'   accepted; i.e. if \code{abs(response - true.answer) < tol}, the
#'   answer is correct (implies \code{num=TRUE}).
#' @param ignore_case Whether to ignore case (capitalization).
#' @param ignore_ws Whether to ignore whitespace.
#' @param regex Whether to use regex to match answers (concatenates
#'   all answers with `|` before matching).
#' @details Writes html code that creates an input box widget. Call
#'   this function inline in an RMarkdown document. See the Web
#'   Exercises RMarkdown template for examples of its use in
#'   RMarkdown.
#'
#' @return A character string with HTML code to generate an input box.
#'
#' @examples
#' # What is 2 + 2?
#' fitb(4, num = TRUE)
#'
#' # What was the name of the Beatles drummer?
#' fitb(c("Ringo", "Ringo Starr"), ignore_case = TRUE)
#'
#' # What is pi to three decimal places?
#' fitb(pi, num = TRUE, tol = .001)
#' @export
fitb <- function(answer,
                 width = calculated_width,
                 num = NULL,
                 ignore_case = FALSE,
                 tol = NULL,
                 ignore_ws = TRUE,
                 regex = FALSE) {
  # make sure answer is a numeric or character vector
  answer <- unlist(answer)
  if (!is.vector(answer) ||
      (!is.numeric(answer) && !is.character(answer))) {
    stop("The answer must be a vector of characters or numbers.")
  }
  print_answers <- paste0(escape_regex(answer), collapse = ", ")
  # set numeric based on data type if num is NULL
  if (is.null(num)) num <- is.numeric(answer)

  # if tol is set, assume numeric
  if (!is.null(tol)) num <- TRUE

  # add zero-stripped versions if numeric
  if (num) {
    answer2 <- strip_lzero(answer)
    answer <- union(answer, answer2)
  }

  # if width not set, calculate it from max length answer, up to limit of 100
  calculated_width <- min(100, max(nchar(answer)))

  answers <- jsonlite::toJSON(as.character(answer))
  answers <- gsub("\'", "&apos;", answers, fixed = TRUE)

  # html format
  if(knitr::is_html_output()){
    return(paste0("<input class='webex-solveme",
                   ifelse(ignore_ws, " nospaces", ""),
                   ifelse(!is.null(tol), paste0("' data-tol='", tol, ""), ""),
                   ifelse(ignore_case, " ignorecase", ""),
                   ifelse(regex, " regex", ""),
                   "' size='", width,
                   "' data-answer='", answers, "'/>"))
  }


  # pdf / other format
  if(knitr::is_latex_output()){
    pdf <- paste(rep("_", width), collapse = "")

    return(paste0(pdf, "^[", print_answers, "]"))
  }
}

#' Create a multiple-choice question
#'
#' @param opts Vector of alternatives. The correct answer is the
#'   element(s) of this vector named 'answer'.
#' @details Writes html code that creates an option box widget, with one or
#'   more correct answers. Call this function inline in an RMarkdown document.
#'   See the Web Exercises RMarkdown template for further examples.
#'
#' @return A character string with HTML code to generate a pull-down
#'   menu.
#'
#' @examples
#' # How many planets orbit closer to the sun than the Earth?
#' mcq(c(1, answer = 2, 3))
#'
#' # Which actor played Luke Skywalker in the movie Star Wars?
#' mcq(c("Alec Guinness", answer = "Mark Hamill", "Harrison Ford"))
#' @export
mcq <- function(opts) {

  ix <- which(names(opts) == "answer")
  if (length(ix) == 0) {
    stop("MCQ has no correct answer")
  }

  # html format
  options <- sprintf("<option value='%s'>%s</option>", names(opts), opts)
  html <- sprintf("<select class='webex-select'><option value='blank'></option>%s</select>",
          paste(options, collapse = ""))
  if(knitr::is_html_output()){
    return(html)
  }
  # pdf / other format
  pdf_opts <- sprintf("* (%s) %s  ", LETTERS[seq_along(opts)], opts)
  pdf <- paste0("^[", opts["answer"], "]\n\n", paste(pdf_opts, collapse = "\n"), "\n\n")
  if(knitr::is_latex_output()){
    return(pdf)
  }
  ""
}

#' Create a true-or-false question
#'
#' @param answer Logical value TRUE or FALSE, corresponding to the correct answer.
#' @details Writes html code that creates an option box widget with TRUE or FALSE as alternatives. Call this function inline in an RMarkdown document. See the Web Exercises RMarkdown template for further examples.
#'
#' @return A character string with HTML code to generate a pull-down
#'   menu with elements TRUE and FALSE.
#'
#' @examples
#' # True or False? 2 + 2 = 4
#' torf(TRUE)
#'
#' # True or False? The month of April has 31 days.
#' torf(FALSE)
#' @export
torf <- function(answer) {
  opts <- c("TRUE", "FALSE")
  names(opts)[2L-answer] <- "answer"

  # check type of knitting
  if(knitr::is_html_output()){
    return(mcq(opts))
  }
  if(knitr::is_latex_output()){
    return(paste0("TRUE / FALSE", "^[", opts["answer"], "]"))
  }
  ""
}


#' Longer MCQs with Radio Buttons
#'
#' @param opts Vector of alternatives. The correct answer is the
#'   element(s) of this vector named 'answer'.
#' @details Writes html code that creates a radio button widget, with a
#'   single correct answer. This is more suitable for longer answers. Call this function inline in an RMarkdown
#'   document. See the Web Exercises RMarkdown template for further
#'   examples.
#'
#' @return A character string containing HTML code to create a set of
#'   radio buttons.
#'
#' @examples
#' # What is a p-value?
#' opts <- c(
#'   "the probability that the null hypothesis is true",
#'   answer = paste("the probability of the observed, or more extreme, data",
#'                  "under the assumption that the null-hypothesis is true"),
#'   "the probability of making an error in your conclusion"
#' )
#'
#' longmcq(opts)
#'
#' @export
longmcq <- function(opts) {
  ix <- which(names(opts) == "answer")
  if (length(ix) == 0) {
    stop("The question has no correct answer")
  }

  opts2 <- gsub("\'", "&apos;", opts, fixed = TRUE)

  # make up a name to group them
  qname <- paste0("radio_", paste(sample(LETTERS, 10, T), collapse = ""))
  options <- sprintf('<label><input type="radio" autocomplete="off" name="%s" value="%s"></input> <span>%s</span></label>', qname, names(opts), opts2)

  # html format
  html <- paste0("<div class='webex-radiogroup' id='", qname, "'>",
         paste(options, collapse = ""),
         "</div>\n")
  if(knitr::is_html_output()){
    return(html)
  }
  # pdf / other format
  pdf_opts <- sprintf("* (%s) %s  ", LETTERS[seq_along(opts2)], opts2)
  pdf <- paste0("^[", opts["answer"], "]\n\n", paste(pdf_opts, collapse = "\n"), "\n\n")
  if(knitr::is_latex_output()){
    return(pdf)
  }
  ""
}


#' Create button revealing hidden content
#'
#' @param button_text Text to appear on the button that reveals the hidden content.
#' @seealso \code{unhide}
#'
#' @details Writes HTML to create a content that is revealed by a
#'   button press. Call this function inline in an RMarkdown
#'   document. Any content appearing after this call up to an inline
#'   call to \code{unhide()} will only be revealed when the user
#'   clicks the button. See the Web Exercises RMarkdown Template for
#'   examples.
#'
#' @return A character string containing HTML code to create a button
#'   that reveals hidden content.
#'
#' @examples
#' # default behavior is to generate a button that says "Solution"
#' hide()
#'
#' # or the button can display custom text
#' hide("Click here for a hint")
#' @export
hide <- function(button_text = "Solution") {
  rmd <- !is.null(getOption("knitr.in.progress"))
  if(knitr::is_html_output()){
    if (rmd) {
      return(paste0("\n<div class='webex-solution'><button>", button_text, "</button>\n"))
    } else {
      return(paste0("\n::: {.callout-note collapse='true'}\n## ", button_text, "\n\n"))
    }
  }
  if(knitr::is_latex_output()) {
    return(knitr::raw_latex(
      c("", paste0("\\begin{tcolorbox}[colback=red!5!white,colframe=red!75!black,title=", button_text, "]"),
        "")
    ))
  }
  ""
}

#' End hidden HTML content
#'
#' @seealso \code{hide}
#'
#' @details Call this function inline in an RMarkdown document to mark
#'   the end of hidden content (see the Web Exercises RMarkdown
#'   Template for examples).
#'
#' @return A character string containing HTML code marking the end of
#'   hiddent content.
#'
#' @examples
#' # just produce the closing </div>
#' unhide()
#' @export
unhide <- function() {
  rmd <- !is.null(getOption("knitr.in.progress"))

  if(knitr::is_html_output()){
    if (rmd) {
      return("\n</div>\n")
    } else {
      return("\n:::\n\n")
    }
  }
  if(knitr::is_latex_output()){
    #return(paste0("\\newpage \\n##", button_text, "\\n\\n"))
    return(knitr::raw_latex(c("", "\\end{tcolorbox}")))

  }
  ""

}

#' Change webexercises widget style
#'
#' @param incorrect The colour of the widgets when the answer is incorrect (defaults to pink #983E82).
#'
#' @param correct The colour of the widgets when the correct answer
#'   not filled in (defaults to green #59935B).
#'
#' @param highlight The colour of the borders around hidden blocks and
#'   checked sections (defaults to blue #467AAC).
#'
#' @return A character string containing HTML code to change the CSS
#'   style values for widgets.
#'
#' @details Call this function in an RMarkdown document to
#'   change the feedback colours using R colour names (see `colours()`)
#'   or any valid CSS colour specification (e.g., red, rgb(255,0,0),
#'   hsl(0, 100%, 50%) or #FF0000).
#'
#' If you want more control over the widget styles, please edit the
#'   webex.css file directly.
#'
#' @examples
#' style_widgets("goldenrod", "purple")
#' @export
style_widgets <- function(incorrect = "#983E82",
                          correct = "#59935B",
                          highlight = "#467AAC") {
  # default if not R colour or hex
  i_border <- incorrect
  i_bg <- incorrect
  c_border <- correct
  c_bg <- correct
  h_border <- highlight

  if (highlight %in% grDevices::colours()) {
    hrgb <- grDevices::col2rgb(highlight, alpha = FALSE)
    h_border <- paste0("rgb(", paste(hrgb, collapse = ", "), ")")
  }

  if (incorrect %in% grDevices::colours()) {
    irgb <- grDevices::col2rgb(incorrect, alpha = FALSE)
    i_border <- paste0("rgb(", paste(irgb, collapse = ", "), ")")
    i_bg <- paste0("rgba(", paste(irgb, collapse = ", "), ", 0.25)")
  } else if (substr(incorrect, 1, 1) == "#") {
    d_bg <- paste0(incorrect, "DD")
  }

  if (correct %in% grDevices::colours()) {
    crgb <- grDevices::col2rgb(correct, alpha = FALSE)
    c_border <- paste0("rgb(", paste(crgb, collapse = ", "), ")")
    c_bg <- paste0("rgba(", paste(crgb, collapse = ", "), ", 0.25)")
  } else if (substr(correct, 1, 1) == "#") {
    c_bg <- paste0(correct, "DD")
  }

  style <- paste0(
    "\n<style>\n",
    ":root {\n",
    "    --incorrect: ", i_border, ";\n",
    "    --incorrect_alpha: ", i_bg, ";\n",
    "    --correct: ", c_border, ";\n",
    "    --correct_alpha: ", c_bg, ";\n",
    "    --highlight: ", h_border, ";\n",
    "}\n",
    "  .webex-incorrect, input.webex-solveme.webex-incorrect,\n",
    "  .webex-radiogroup label.webex-incorrect {\n",
    "    border: 2px dotted var(--incorrect);\n",
    "    background-color: var(--incorrect_alpha);\n",
    "  }\n",
    "  .webex-correct, input.webex-solveme.webex-correct,\n",
    "  .webex-radiogroup label.webex-correct {\n",
    "    border: 2px dotted var(--correct);\n",
    "    background-color: var(--correct_alpha);\n",
    "  }\n",
    "  .webex-box, .webex-solution.open {\n",
    "    border: 2px solid var(--highlight);n",
    "  }\n",
    "  .webex-solution button, .webex-check-button {\n",
    "    background-color: var(--highlight);\n",
    "  }\n",
    "</style>\n\n"
  )

  cat(style)
}

#' Display total correct
#'
#' @param elem The html element to display (e.g., div, h3, p, span)
#' @param args Optional arguments for css classes or styles
#'
#' @return A string with the html for displaying a total correct element.
#'
#' @export
#' @keywords internal

total_correct <- function(elem = "span", args = "") {
  .Deprecated(".webex-check sections",
              package = "webexercises",
              old = "total_correct",
              msg = "The function webexercises::total_correct() is deprecated. Use sections with the class 'webex-check' to set up self-checking mini-quizzes with total correct.")
  #sprintf("<%s %s class=\"webex-total_correct\"></%s>\n\n",
  #            elem, args, elem)
}

#' Round up from .5
#'
#' @param x A vector of numeric values.
#'
#' @param digits Integer indicating the number of decimal places (`round`) or significant digits (`signif`) to be used.
#'
#' @details Implements rounding using the "round up from .5" rule,
#'   which is more conventional than the "round to even" rule
#'   implemented by R's built-in \code{\link{round}} function. This
#'   implementation was taken from
#'   \url{https://stackoverflow.com/a/12688836}.
#'
#' @return A vector of rounded numeric values.
#'
#' @examples
#' round2(c(2, 2.5))
#'
#' # compare to:
#' round(c(2, 2.5))
#' @export
round2 <- function(x, digits = 0) {

  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

#' Strip leading zero from numeric string
#'
#' @param x A numeric string (or number that can be converted to a string).
#'
#' @return A string with leading zero removed.
#'
#' @examples
#' strip_lzero("0.05")
#' @export
strip_lzero <- function(x) {
  sub("^([+-]*)0\\.", "\\1.", x)
}

#' Escape a string for regex
#'
#' @param string A string to escape.
#'
#' @return A string with escaped characters.
#' @export
#'
#' @examples
#' escape_regex("library(tidyverse)")
escape_regex <- function(string) {
  gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", string)
}
