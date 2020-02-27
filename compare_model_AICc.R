#' @param x is a list of models

compare_models <- function(x) {
  if (!requireNamespace("tidyverse", quietly = TRUE)) {
    stop(paste0("Package 'tidyverse' needed for this function to work.",
                "Please install it."),
         call. = FALSE)
  }
  if (!requireNamespace("MuMIn", quietly = TRUE)) {
    stop(paste0("Package 'MuMIn' needed for this function to work.",
                "Please install it."),
         call. = FALSE)
  }
  AICc_min <- min(sapply(x, function(m) {
    MuMIn::AICc(m)
  }))
  ans <- anova(m1, m2, m3, m4, m5)[, c(1, 8)]
  ans <- as.data.frame(ans)
  ans$model <- as.character(c(m1@call$formula,
                              m2@call$formula,
                              m3@call$formula,
                              m4@call$formula,
                              m5@call$formula))
  ans$dAICc <- c(AICc(m1),
                 AICc(m2),
                 AICc(m3),
                 AICc(m4),
                 AICc(m5)) - AICc_min
  ans$weight <- round(exp(-0.5 * ans$dAICc) / sum(exp(-0.5 * ans$dAICc)), 2)
  ans <- ans[order(ans[, "dAICc"]), ]
  return(ans)
}
