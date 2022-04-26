#' Title
#'
#' @param parameters
#' @param target
#' @param target_tt
#' @param ncores
#' @param nsims
#'
#' @return
#' @export
#'
#' @examples
nlcali <- function(parameters, target, target_tt,
                   ncores = 4,
                   nsims = 1000) {


 test_EIRs <- rexp(nsims, rate = 1/60)

 sim_data <- run_simulations(parameters = parameters,
                             target = target,
                             target_tt = target_tt,
                             ncores = ncores,
                             test_EIRs = test_EIRs)


 stan_dat <- create_stan_data(sims = sim_data,
                              target = target)


 stan_res <- fit_stan(data = stan_dat,
                      cores = ncores)

 stan_out <- rstan::extract(stan_res)

 return(stan_out)
}
