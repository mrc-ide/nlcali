#' Fit spline model to simulation data
#'
#' @param sims
#' @param target
#'
#' @return
#' @export
#' @importFrom mgcv gam
#' @importFrom data.table data.table copy as.data.table
#' @importFrom boot logit inv.logit
#'
#' @examples
fit_spline <- function(sim_data, target){

 dat <- data.table::copy(sim_data)

 cols <- paste0("prev", 1:length(target))

 dat <- dat[, (cols) := as.list(.SD[,prev]), by = "eir"]
 dat <- dat[timepoint == 1, ]
 dat[, prev := NULL]
 dat[, timepoint := NULL]


 dat[, (cols) := lapply(.SD, boot::logit), .SDcols = cols]
 dat[, eir := log(eir)]

 form <- paste0("eir ~ s(prev1)",
                ifelse(length(target) > 1,
                       paste0(" + s(prev",
                              2:length(target),
                              ")"), ""))

 m <- mgcv::gam(data = dat, formula = as.formula(form))


 xseq <- boot::logit(seq(0.01, 0.8, 0.01))

 if(length(target) == 1){
  pred_df <- data.frame(prev1 = xseq)
  tar_df <- data.frame(prev1 = boot::logit(target))
 }else if(length(target)  == 2){
  pred_df <- data.frame(expand.grid(xseq, xseq))
  colnames(pred_df) <- cols
  tar_df <- pred_df[1,]
  tar_df[1, ] <- boot::logit(target)
 }

 eir <- exp(predict(m, pred_df))
 out <- data.table::as.data.table(cbind(pred_df, eir))
 out[, (cols) := lapply(.SD, boot::inv.logit), .SDcols = cols]
 pred <- exp(predict(m, tar_df))

 return(list(spline = out, pred = pred, mod = m))
}
