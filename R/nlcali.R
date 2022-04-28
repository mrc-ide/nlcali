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
                   ncores = 1,
                   nsims = 150) {

 test_EIRs <- exp(seq(-3, 6, length.out = nsims))

 sim_data <- nlcali::run_simulations(parameters = parameters,
                             target = target,
                             target_tt = target_tt,
                             ncores = ncores,
                             test_EIRs = test_EIRs)

 mod_res <- nlcali::fit_spline(sim_data = sim_data,
                               target = target)

 return(list(sims = sim_data, eir_pred = mod_res$pred, fit = mod_res$spline, mod = mod_res$mod,
             target = target, target_tt = target_tt))
}
