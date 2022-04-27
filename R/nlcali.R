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
                   nsims = 150) {

 test_EIRs <- c(rexp(n = round(3 * nsims / 10), rate = 2),
                rexp(n = round(7 * nsims / 10), rate = 1/50))

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

 return(list(sims = sim_data, stan = stan_out, stan_dat = stan_dat))
}
