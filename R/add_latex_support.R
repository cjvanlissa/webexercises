add_latex_support <- function(filename){
  worcs:::with_cli_try("Adding webexercises support to {.value {filename}}.", {
  if(!file.exists(filename)) stop("File does not exist.")

  lnz <- readLines(filename)

  # Extract YAML frontmatter ------------------------------------------------

  delim_lnz <- which(lnz == "---")
  if(!isTRUE(length(delim_lnz) >= 2 & delim_lnz[1] == 1)){
    stop()
  }
  yml <- lnz[(delim_lnz[1]+1L):(delim_lnz[2]-1L)]
  yml <- yaml::read_yaml(text = yml)

  if(endsWith(tolower(filename), ".qmd")){

    # Add to the YAML frontmatter ---------------------------------------------
    if(is.null(yml[["format"]][["pdf"]])|isTRUE(yml[["format"]][["pdf"]] == "default")){
      yml[["format"]][["pdf"]] <- list(`include-in-header` = list(list(text = "\\usepackage[most]{tcolorbox}")))
    } else {
      if(!isTRUE(any(unlist(yml[["format"]][["pdf"]]) == "\\usepackage[most]{tcolorbox}"))){
        yml[["format"]][["pdf"]][["include-in-header"]] <- c(
          yml[["format"]][["pdf"]][["include-in-header"]],
          list(list(text = "\\usepackage[most]{tcolorbox}")))
      }
    }
  }

  if(endsWith(tolower(filename), ".rmd")){
    # Add to the YAML frontmatter ---------------------------------------------

    if(!isTRUE("tcolorbox" %in% unlist(yml[["output"]][["pdf_document"]][["extra_dependencies"]]))){

      extra_dependencies =

      yml[["output"]][["pdf_document"]][["extra_dependencies"]] <- c(yml[["output"]][["pdf_document"]][["extra_dependencies"]], list(tcolorbox = "most"))
    }
  }

    # Put YAML back in place --------------------------------------------------
    format_yml <- yaml::as.yaml(yml)
    format_yml <- gsub(": yes", ": true", format_yml, fixed = TRUE)
    format_yml <- gsub(": no", ": false", format_yml, fixed = TRUE)
    lnz <- c("---", gsub("\\n$", "", format_yml), lnz[delim_lnz[2]:length(lnz)])
    writeLines(lnz, filename)
  })
}

#' Add webexercises 'YAML' metadata
#'
#' Adds the necessary 'YAML' metadata to an existing 'Quarto' or 'Rmarkdown'
#' file.
#' @param filename The name of the file to edit.
#' @return No return value, called for side effects.
#' @export
#' @rdname add_to_quarto_file
add_to_quarto_file <- function(filename){
  add_latex_support(filename = filename)
  add_html_support(filename = filename)
}

#' @export
#' @rdname add_to_quarto_file
add_to_rmarkdown_file <- add_to_quarto_file
