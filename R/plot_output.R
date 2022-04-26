#' Title
#'
#' @param out
#' @param data
#' @param target_tt
#' @param test_EIRs
#'
#' @return
#' @importFrom data.table data.table
#' @importFrom ggplot2 ggplot geom_tile labs scale_x_continuous scale_y_continuous
#' @importFrom viridis scale_fill_viridis
#' @export
#'
#' @examples
plot_res_3d <- function(out, data, test_EIRs){

 plot_data <- data.table::data.table(med = apply(out$prev_out, 2, median),
            lq = apply(out$prev_out, 2, quantile, prob = 0.025),
            uq = apply(out$prev_out, 2, quantile, prob = 0.975),
            prev1 = data$y[,1],
            prev2 = data$y[,2])

 p <- ggplot2::ggplot(data = plot_data, ggplot2::aes(x = prev1, y = prev2, fill = med)) +
  ggplot2::geom_tile() +
  ggplot2::labs(x = "Prevalence at timepoint 1 (%)", y = "Prevalence at timepoint 2 (%)") +
  viridis::scale_fill_viridis(name = "Starting\nEIR", breaks = seq(0, max(test_EIRs), 25)) +
  ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.1), labels = seq(0, 100, 10)) +
  ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.1), labels = seq(0, 100, 10))

 return(p)
}

plot_res_2d <- function(stan_samples, sim_samples, stan_data){

 plot_data <- data.table::data.table(med = apply(stan_samples$prev_out, 2, median),
                                     lq = apply(stan_samples$prev_out, 2, quantile, prob = 0.025),
                                     uq = apply(stan_samples$prev_out, 2, quantile, prob = 0.975),
                                     prev = stan_data$y[,1])

 p <- ggplot2::ggplot() +
  ggplot2::geom_ribbon(data = plot_data,
                       ggplot2::aes(x = prev,
                                    y = med,
                                    ymin = lq,
                                    ymax = uq),
                       alpha = 0.2) +
  ggplot2::geom_point(data = sim_samples,
                      ggplot2::aes(x = prev, y = eir)) +
  ggplot2::geom_line(data = plot_data,
                     ggplot2::aes(x = prev,
                                  y = med),
                     col = "red") +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "Observed prevalence (%)", y = "Starting EIR") +
  ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.1), labels = seq(0, 100, 10))

 return(p)
}
