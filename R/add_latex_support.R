add_latex_support <- function(rmd_file){
  worcs:::with_cli_try("Adding webexercises support for PDF output to Rmarkdown file.", {
    lnz <- readLines(rmd_file)

    # Extract YAML frontmatter ------------------------------------------------

    delim_lnz <- which(lnz == "---")
    if(!isTRUE(length(delim_lnz) >= 2 & delim_lnz[1] == 1)){
      stop()
    }
    yml <- lnz[(delim_lnz[1]+1L):(delim_lnz[2]-1L)]
    yml <- yaml::read_yaml(text = yml)


    # Add to the YAML frontmatter ---------------------------------------------


    if(!isTRUE("preamble.tex" %in% yml[["output"]][["pdf_document"]][["includes"]][["in_header"]])){
      yml[["output"]][["pdf_document"]][["includes"]][["in_header"]] <- c(yml[["output"]][["pdf_document"]][["includes"]][["in_header"]], "preamble.tex")
    }
    if(!isTRUE("awesomebox" %in% yml[["output"]][["pdf_document"]][["extra_dependencies"]])){
      yml[["output"]][["pdf_document"]][["extra_dependencies"]] <- c(yml[["output"]][["pdf_document"]][["extra_dependencies"]], "awesomebox")
    }

    # Write preamble.tex ------------------------------------------------------


    if(file.exists("preamble.tex")){
      prmbl <- readLines("preamble.tex")
      add_lines <- c("\\usepackage{color}", "\\usepackage{framed}", "\\setlength{\\fboxsep}{.8em}")
      for(l in add_lines){
        if(!any(grepl(l, prmbl, fixed = TRUE))){
          prmbl <- c(prmbl, l)
        }
      }
      if(!any(grepl("\\newenvironment{quizzbox}{", prmbl, fixed = TRUE))){
        prmbl <- c(prmbl,
                   "\\newenvironment{quizzbox}{",
                   "  \\definecolor{shadecolor}{rgb}{0, 0, 0}  % black",
                   "  \\color{white}",
                   "  \\begin{shaded}}"
                   , "{\\end{shaded}}")
      }
    } else {
      writeLines(c("\\usepackage{color}", "\\usepackage{framed}", "\\setlength{\\fboxsep}{.8em}",
                   "", "\\newenvironment{quizzbox}{", "  \\definecolor{shadecolor}{rgb}{0, 0, 0}  % black",
                   "  \\color{white}", "  \\begin{shaded}}", "{\\end{shaded}}"),
                 "preamble.tex")
    }

    # Put YAML back in place --------------------------------------------------

    lnz <- c("---", gsub("\\n$", "", yaml::as.yaml(yml)), lnz[delim_lnz[2]:length(lnz)])
    writeLines(lnz, rmd_file)
  })
}
