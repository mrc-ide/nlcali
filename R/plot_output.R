#' Title
#'
#' @param out
#' @param data
#' @param target_tt
#' @param test_EIRs
#'
#' @return
#' @importFrom data.table data.table
#' @importFrom ggplot2 ggplot aes geom_tile labs scale_x_continuous scale_y_continuous
#' @importFrom viridis scale_fill_viridis
#' @export
#'
#' @examples
plot_res_3d <- function(output){

 plot_data <- data.table::data.table(med = apply(output$stan$prev_out, 2, median),
            lq = apply(output$stan$prev_out, 2, quantile, prob = 0.025),
            uq = apply(output$stan$prev_out, 2, quantile, prob = 0.975),
            prev1 = output$stan_dat$y[,1],
            prev2 = output$stan_dat$y[,2])[med <= max(output$sims$eir)]

 p <- ggplot2::ggplot(data = plot_data, ggplot2::aes(x = prev1, y = prev2, fill = med)) +
  ggplot2::geom_tile() +
  ggplot2::labs(x = "Prevalence at timepoint 1 (%)", y = "Prevalence at timepoint 2 (%)") +
  viridis::scale_fill_viridis(name = "Starting\nEIR", breaks = seq(0, max(output$sims$eir), 50)) +
  ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.1), labels = seq(0, 100, 10)) +
  ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.1), labels = seq(0, 100, 10))

 return(p)
}

#' Title
#'
#' @param stan_samples
#' @param sim_samples
#' @param stan_data
#'
#' @return
#' @importFrom data.table data.table
#' @importFrom ggplot2 ggplot geom_ribbon aes geom_point geom_line labs scale_x_continuous theme_bw geom_errorbar
#' @importFrom boot logit
#' @import patchwork
#' @export
#'
#' @examples
plot_res_2d <- function(output){

 plot_data <- data.table::data.table(med = apply(output$stan$prev_out, 2, median),
                                     lq = apply(output$stan$prev_out, 2, quantile, prob = 0.025),
                                     uq = apply(output$stan$prev_out, 2, quantile, prob = 0.975),
                                     prev = output$stan_dat$y[,1])[med <= max(output$sims$eir)]

 p <- ggplot2::ggplot() +
  ggplot2::geom_ribbon(data = plot_data,
                       ggplot2::aes(x = prev,
                                    y = med,
                                    ymin = lq,
                                    ymax = uq),
                       alpha = 0.2) +
  ggplot2::geom_point(data = output$sims,
                      ggplot2::aes(x = prev, y = eir)) +
  ggplot2::geom_line(data = plot_data,
                     ggplot2::aes(x = prev,
                                  y = med),
                     col = "red") +
  ggplot2::geom_point(data = data.frame(x = output$stan_dat$tar,
                                        y = median(output$stan$pred)),
                      ggplot2::aes(x = x, y = y),
                      col = "dodgerblue", size = 2) +
  ggplot2::geom_errorbar(data = data.frame(x = output$stan_dat$tar,
                                           ymin = quantile(output$stan$pred, prob = 0.025),
                                           ymax = quantile(output$stan$pred, prob = 0.975)),
                         ggplot2::aes(x = x, ymin = ymin, ymax = ymax),
                         col = "dodgerblue", width = 0.025) +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "Observed prevalence (%)", y = "Starting EIR") +
  ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.1), labels = seq(0, 100, 10))

 p2 <- ggplot2::ggplot() +
  ggplot2::geom_point(data = output$sims,
                       ggplot2::aes(x = boot::logit(prev), y = log(eir))) +
  ggplot2::geom_line(data = plot_data,
                     ggplot2::aes(x = boot::logit(prev), y = log(med)),
                     col = "red") +
  ggplot2::geom_ribbon(data = plot_data,
                       ggplot2::aes(x = boot::logit(prev),
                                    ymin = log(lq),
                                    ymax = log(uq)),
                       alpha = 0.2) +
  ggplot2::labs(x = "logit(prevalence)", y = "log(EIR)") +
  ggplot2::theme_bw() +
  ggplot2::geom_point(data = data.frame(x = output$stan_dat$tar,
                                        y = median(output$stan$pred)),
                      ggplot2::aes(x = boot::logit(x), y = log(y)),
                      col = "dodgerblue", size = 2) +
  ggplot2::geom_errorbar(data = data.frame(x = output$stan_dat$tar,
                                           ymin = quantile(output$stan$pred, prob = 0.025),
                                           ymax = quantile(output$stan$pred, prob = 0.975)),
                         ggplot2::aes(x = boot::logit(x),
                                      ymin = log(ymin),
                                      ymax = log(ymax)),
                         col = "dodgerblue", width = 0.025)

 pout <- p + p2

 return(pout)
}
