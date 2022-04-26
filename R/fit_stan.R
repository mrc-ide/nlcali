#' Title
#'
#' @param data
#' @param ...
#'
#' @return
#' @importFrom rstan sampling
#' @export
#'
#' @examples
fit_stan <- function(data, ...){

 out <- rstan::sampling(stanmodels$reg,
                 data = data, ...)

 return(out)
}
