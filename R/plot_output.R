#' Plot model fit with 2 observations
#'
#' @param output
#'
#' @return
#' @importFrom data.table data.table
#' @importFrom ggplot2 ggplot aes geom_tile labs scale_x_continuous scale_y_continuous scale_fill_distiller theme_classic
#' @importFrom mgcViz plot.mgcv.smooth.1D l_fitLine l_ciLine l_points sm
#' @export
#'
#' @examples
plot_res_3d <- function(output){

 p <- ggplot2::ggplot(data = output$fit,
                      ggplot2::aes(x = prev1, y = prev2, fill = eir)) +
  ggplot2::geom_tile() +
  ggplot2::labs(x = "Prevalence at timepoint 1 (%)", y = "Prevalence at timepoint 2 (%)") +
  ggplot2::scale_fill_distiller(palette = "Spectral",
                                breaks = seq(0, max(output$fit$eir), 50),
                                name = "Starting\nEIR") +
  ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.1), labels = seq(0, 100, 10)) +
  ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.1), labels = seq(0, 100, 10)) +
  ggplot2::geom_point(data = data.frame(x = output$target[1], y = output$target[2]),
                      ggplot2::aes(x = x, y = y),
                      inherit.aes = FALSE, shape = 0)

 b <- mgcViz::getViz(output$mod)

 p2 <- mgcViz::plot.mgcv.smooth.1D(mgcViz::sm(b, 1)) +
  mgcViz::l_fitLine(colour = "red") +
  mgcViz::l_ciLine(mul = 5, colour = "blue", linetype = 2) +
  mgcViz::l_points(shape = 19, size = 1, alpha = 0.3) +
  ggplot2::theme_classic()


 p3 <- mgcViz::plot.mgcv.smooth.1D(mgcViz::sm(b, 2)) +
  mgcViz::l_fitLine(colour = "red")+
  mgcViz::l_ciLine(mul = 5, colour = "blue", linetype = 2) +
  mgcViz::l_points(shape = 19, size = 1, alpha = 0.3) +
  ggplot2::theme_classic()

 return(list(heatmap = p, prev1 = p2, prev2 = p3))
}

#' Plot model fit with 1 baseline observation
#'
#' @param output
#'
#' @return
#' @importFrom data.table data.table
#' @importFrom ggplot2 ggplot geom_ribbon aes geom_point geom_line labs scale_x_continuous theme_bw geom_errorbar
#' @importFrom boot logit
#' @importFrom ggpubr ggarrange
#' @export
#'
#' @examples
plot_res_2d <- function(output){

 p <- ggplot2::ggplot() +
  ggplot2::geom_point(data = output$sims,
                      ggplot2::aes(x = prev, y = eir)) +
  ggplot2::geom_line(data = output$fit,
                     ggplot2::aes(x = prev1,
                                  y = eir),
                     col = "red") +
  ggplot2::geom_point(data = data.frame(x = output$target,
                                        y = output$eir_pred),
                      ggplot2::aes(x = x, y = y),
                      col = "dodgerblue", size = 2) +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "Observed prevalence (%)", y = "Starting EIR") +
  ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.1), labels = seq(0, 100, 10)) +
  ggplot2::coord_cartesian(xlim = c(0, output$target + 0.05),
                           ylim = c(0, output$fit[prev1 <= output$target + 0.05, max(eir)])) +
  ggplot2::geom_vline(xintercept = output$target, lty = 2,
                      col = "dodgerblue") +
  ggplot2::geom_hline(yintercept = output$eir_pred, lty = 2,
                      col = "dodgerblue")

 p2 <- ggplot2::ggplot() +
  ggplot2::geom_point(data = output$sims,
                       ggplot2::aes(x = boot::logit(prev), y = log(eir))) +
  ggplot2::geom_line(data = output$fit,
                     ggplot2::aes(x = boot::logit(prev1), y = log(eir)),
                     col = "red") +
  ggplot2::labs(x = "logit(prevalence)", y = "log(EIR)") +
  ggplot2::theme_bw() +
  ggplot2::geom_point(data = data.frame(x = output$target,
                                        y = output$eir_pred),
                      ggplot2::aes(x = boot::logit(x), y = log(y)),
                      col = "dodgerblue", size = 2)

 pout <- ggpubr::ggarrange(p, p2)

 return(pout)
}
