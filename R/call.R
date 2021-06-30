
## Call modifier/cleaner




#' Recursive 'call cleaner'
#'
#' @param cc R object (does nothing if cc is not a call)
#' 
#' @description Recursively 'cleans' a call. Can substantially reduce object size. 
#' The returned code may in principle be a very tiny bit faster than the original code.
#' 
#' @details This function assumes that you are a slightly normal being who does not overwrite `(` and `{`
#' Sometimes gives an error due to constructs in code; this has not been investigated. 
#'
#' @return A functionally and superficially similar version of cc. Returns cc if cc is not a call.
#' @export
#'
callclean <- function(cc) {
  
  if (!is.call(cc)) {
    return (cc)
  }
  if (cc[[1]] == quote(`(`) || (cc == quote(`{`) && length(cc) == 2)) {
    return(callclean(cc[[2]]))
  }
  for (i in 2:length(cc)) cc[[i]] <- callclean(cc[[i]])
  return (cc)
}

#' Smart call cleaner
#'
#' @param f function
#' 
#' @description Cleans a function by calling \code{callclean} on its body. See \code{callclean}.
#'
#' @return f, with its body cleaned. 
#' @export
#'
callfun <- function(f) {
  cc <- body(f)
  body(f) <- callclean(body(f))
  f
}

## Apply to self
callclean <- callfun(callclean)
callfun <- callfun(callfun)
