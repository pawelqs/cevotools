#' Not in operator
#' @param x left-hand side argument
#' @param y right-hand side argument
'%not in%' <- function(x,y)!('%in%'(x,y))


join_aes <- function(aes_default, aes_2) {
  aes <- c(as.list(aes_default[names(aes_default) %not in% names(aes_2)]), aes_2)
  class(aes) <- 'uneval'
  aes
}


get_verbosity <- function() {
  v <- verbose::verbose("cevoverse")
  if (is.null(v)) {
    0
  } else {
    v
  }
}


msg <- function(...,
                collapse = "",
                col = "steelblue3",
                new_line = TRUE,
                verbose = get_verbosity()) {
  msg <- str_c(list(...), collapse = collapse)
  if (verbose && new_line) {
    cli::cat_line(msg, col = col)
  } else if (verbose) {
    cat(crayon::blue(msg))
  }
}


require_packages <- function(...) {
  pkgs <- list(...)
  missing <- !map_lgl(pkgs, requireNamespace, quietly = TRUE)
  if (any(missing)) {
    stop(
      paste0("Package '", pkgs[missing], "' must be installed to use this function.\n"),
      call. = FALSE
    )
  }
}


verbose_down <- function(verbose) {
  if (isTRUE(verbose) || isFALSE(verbose) || verbose == 0) {
    FALSE
  } else if (is.numeric(verbose) && verbose > 0) {
    verbose - 1
  } else {
    stop("Verbose should be logical or positive")
  }
}
