



#' Residual comparison plots
#' 
#' @description Replace residuals with random values and show diagnostics plots. 
#'
#' @param L Output from \code{lm}
#' @param main Plot title (passed to plot.lm)
#' @param ... Optional parameters to be passed to \code{plot.lm}
#' 
#' @details It is well-known that residuals are not i.i.d. nomrally distributed. This is an issue when doing model validation.
#' This function simulates values based on an fit from \code{lm}, fits model to new values and returns diagnosics plot.
#' Used for comparing observed diagnostics with 'true' diagnostics. 
#'
#' @export
#'
#' @examples
#' 
#' ## Example copied from lm function
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' lm.D9 <- lm(weight ~ group)
#' 
#' 
#' plot(lm.D9) ## Residual plots for data
#' 
#' resplot(lm.D9) ##
#' 
rescompplot <- function(L, main = "Comparison validation plot", ...) {
  if (class(L) != "lm") stop("L must be a linear model output fitted!")
  if (is.null(L$model)) stop("L must be fitted with model = TRUE")
  
  .newdata <- rnorm(n = length(L$residuals), mean = fitted(L), sd = sqrt(sum(L$residuals^2) / L$df.residual))
  
  r <- L$call
  r$formula[[2]] <- quote(.newdata) # replace variable
  r$data <- NULL # just in case
  
  L.ny <- eval(r, envir = L$model)
  plot(L.ny, main = main, ...)
}





