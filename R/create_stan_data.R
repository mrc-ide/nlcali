#' Title
#'
#' @param sims
#' @param target
#'
#' @return
#' @export
#'
#' @examples
create_stan_data <- function(sims, target){

 prevmat <- matrix(NA, ncol = length(target), nrow = nrow(sims) / length(target))

 for(i in 1:length(target)){
  prevmat[, i] <- sims[timepoint == i, prev]
 }

 out <- list()
 out$N_obs = nrow(prevmat)
 out$prev <- prevmat
 out$eir <- sims[timepoint == 1, eir]
 out$tar <- array(target, dim = length(target))
 out$N_pred <- length(target)

 if(out$N_pred == 1){
  out$y <- matrix(seq(0.01, 0.99, 0.03), ncol = 1, byrow = FALSE)
  out$N_samp <- nrow(out$y)
 }else if(out$N_pred == 2){
  out$y <- expand.grid(seq(0.01, 0.99, 0.03), seq(0.01, 0.99, 0.03))
  out$N_samp <- nrow(out$y)
 }else{
  out$N_samp <- 0
  out$y <- matrix(nrow = 0, ncol = out$N_pred)
 }

 return(out)
}
