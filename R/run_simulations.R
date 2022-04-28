#' Run simulations for a range of starting_EIR values
#'
#' @return
#' @importFrom parallel detectCores makeCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach getDoParRegistered foreach %dopar%
#' @importFrom data.table data.table rbindlist
#' @importFrom malariasimulation set_equilibrium run_simulation
#' @importFrom cali summary_pfpr_2_10
#' @export
#'
#' @examples
run_simulations <- function(parameters, target, target_tt, test_EIRs, ncores) {

 print("Setting up cores for simulation runs")
 my.cluster <- parallel::makeCluster(
  ncores,
  type = "PSOCK"
 )

 print(my.cluster)
 doParallel::registerDoParallel(cl = my.cluster)

 print(paste0("Cluster registered: ", foreach::getDoParRegistered()))
 print(paste0("Simulating from ", foreach::getDoParWorkers(), " cores"))

 run_func <- function(x){
  p <- malariasimulation::set_equilibrium(parameters, init_EIR = x)
  raw_output <- malariasimulation::run_simulation(timesteps = max(target_tt), parameters = p)
  out <- cali::summary_pfpr_2_10(raw_output)[target_tt]
  return(out)
 }

 ll <- foreach::foreach(i = 1:length(test_EIRs)) %dopar% {
  data.table::data.table(eir = test_EIRs[i],
                         prev = run_func(test_EIRs[i]),
                         timepoint = 1:length(target_tt))
 }

 out <- data.table::rbindlist(ll)

 # Can't have fully 0 prevalence into logit model
 out[prev == 0, prev := 0.005]

 return(out)
}
