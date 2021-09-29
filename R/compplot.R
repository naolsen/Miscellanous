

#' Residual comparison plots
#'
#' @param L Output from \code{lm} or \code{glm}
#' @param main Plot title (passed to \code{plot.lm})
#' @param ... Optional parameters to be passed to \code{plot.lm}
#'
#' @details It is well-known that residuals are not i.i.d. normally distributed. 
#' This is often an issue when doing model validation because we are told to look for i.i.d. normality.
#' 
#' Using \code{simulate}, this function simulates values based on the model fit, fits model to new values and returns diagnosics plot.
#' @return
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
#' par(mfrow = c(2,2))
#' plot(lm.D9) ## Residual plot for data
#' 
#' rescompplot(lm.D9) ## Comparison plot for simulated data
#' 
#' 
#' ## Poisson regression fitted to the Covid-19 death toll in the United States 
#' in the initial stage of the pandemic
#'
#' ## Daily count of fatalities in the United States, March 7 - March 22. 
#' usa.deaths <- c(1, 3, 4, 5, 7, 4, 7, 7, 6, 13, 21, 26, 52, 55, 65, 106)
#' times <- 7:22 ## Day numbers
#' 
#' Lp <- glm(usa.deaths ~ times, family = poisson)
#' 
#' par(mfrow = c(2,2))
#' ## Residual plot
#' plot(Lp) 
#' 
#' ## Comparison plot for simulated data
#' rescompplot(Lp)
#' 
rescompplot <- function(L, main = "Comparison validation plot", ...) {
  if (!inherits(L, 'lm')) stop("L must be a glm/lm output!")
  
  data <- L$model
  data[[1]] <- simulate(L, nsim = 1)[[1]]
  
  r <- L$call
  L.ny <- eval(r, envir = data)
  plot(L.ny, main = main, ...)
}




