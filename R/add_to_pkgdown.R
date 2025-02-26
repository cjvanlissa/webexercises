#' Add webexercises helper files to pkgdown
#'
#' Adds the necessary helper files to an existing pkgdown project.
#'
#' @param pkgdown_dir The base directory for your pkgdown project
#' @param include_dir The directory where you want to put the css and
#'   js files (defaults to "pkgdown")
#' @return No return value, called for side effects.
#' @export
add_to_pkgdown <- function(pkgdown_dir = ".", include_dir = "pkgdown"){
  if (pkgdown_dir == "") pkgdown_dir <- "."
  if(!include_dir == "pkgdown") cli_msg("i" = "Note that {.code pkgdown} does not recognize other directories than {.val pkgdown}.")
  css <- system.file("reports/default/webex.css", package = "webexercises")
  css_lines <- readLines(css)
  js <- system.file("reports/default/webex.js", package = "webexercises")
  js_lines <- readLines(js)
  js_lines <- js_lines[-grep("</?script>", js_lines)]
  if(!dir.exists(file.path(pkgdown_dir, include_dir))){
    dir.create(file.path(pkgdown_dir, include_dir))
  }
  # Write extra.css
  if(file.exists(file.path(pkgdown_dir, include_dir, "extra.css"))){
    css_existing <- readLines(file.path(pkgdown_dir, include_dir, "extra.css"))
    if(!any(grepl("webex-check", css_existing, fixed = TRUE))){
      writeLines(c(css_existing, css_lines), file.path(pkgdown_dir, include_dir, "extra.css"))
    }
  } else {
    writeLines(css_lines, file.path(pkgdown_dir, include_dir, "extra.css"))
  }
  # Write extra.js
  if(file.exists(file.path(pkgdown_dir, include_dir, "extra.js"))){
    js_existing <- readLines(file.path(pkgdown_dir, include_dir, "extra.js"))
    if(!any(grepl("webex-solution", js_existing, fixed = TRUE))){
      writeLines(c(js_existing, js_lines), file.path(pkgdown_dir, include_dir, "extra.js"))
    }
  } else {
    writeLines(js_lines, file.path(pkgdown_dir, include_dir, "extra.js"))
  }

  # Write extra.js
  if(file.exists(file.path(pkgdown_dir, "_pkgdown.yml"))){
    pkgdown_location <- file.path(pkgdown_dir, "_pkgdown.yml")
  } else {
    if(file.exists(file.path(pkgdown_dir, include_dir, "_pkgdown.yml"))){
      pkgdown_location <- file.path(pkgdown_dir, "_pkgdown.yml")
    } else {
      stop("No _pkgdown.yml found.")
    }
  }
  yml_existing <- yaml::read_yaml(pkgdown_location)
  if(!is.null(yml_existing[["template"]][["include"]][["after_body"]])){
    if(!grepl("extra.js", yml_existing[["template"]][["include"]][["after_body"]])){
      yml_existing[["template"]][["include"]][["after_body"]] <- paste0(yml_existing[["template"]][["include"]][["after_body"]], '<script scr="extra.js"></script>')
    }
  } else {
    yml_existing[["template"]][["include"]][["after_body"]] <- '<script scr="extra.js"></script>'
  }
  yaml::write_yaml(yml_existing, pkgdown_location)
  pkgdown::init_site()
  invisible(NULL)
}
