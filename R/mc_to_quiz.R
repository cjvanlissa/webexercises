answer <- function(x){
  hide("Answer")
  cat(x)
  unhide()
}

quiz_from_cvs <- function(filename){
  qs <- read.csv(filename, stringsAsFactors = FALSE, header = FALSE, na.strings = c("NA", ''))
  A_args <- Q_args <- vector("list", length = nrow(qs))
  for(i in 1:nrow(qs)){
    content <- as.character(na.omit(unlist(qs[i,])))
    names(content)[2] <- "answer"
    Q_args[[i]] <- sample(content[-c(1, length(content))], size = (length(content)-2L))
    A_args[[i]] <- paste0("Q", i, ": ", tail(content, 1))
  }
  names(Q_args) <- as.character(qs[, 1, drop = TRUE])
  Q_args[["title"]] <- "Formative Assessment"
  do.call(quiz, Q_args)
  # cat('\n\n')
  # answer(paste0(A_args, collapse = "\n\n"))
}
