################################################################################
#
# HELPER FUNCTION
#
# Description: Calibrate simulated temperature data with historical observations
#
# Source: adapted from https://github.com/PierreMasselot/EUcityProj/blob/main/functions/isimip3.R
# Note: the code has been adapted to match the DIPC requirements and handle NAs
#
################################################################################

isimip3 <- function(obshist, simhist, simfut,
  yearobshist, yearsimhist, yearsimfut, detrend = T,
  uc, var, sc, ml)
{

  # SAFETY CHECK: If we have no data, return NAs instead of crashing
  if (length(na.omit(obshist)) < 2 || length(na.omit(simhist)) < 2) {
    cat(sprintf("%s ERROR ISIMIP3 function: %s - %s - %s - %s\n",
                as.character(Sys.time()), uc, var, sc, ml),
        file = sprintf("%s/06_trace.txt", tdir), append = TRUE) |> try()
    return(rep(NA_real_, length(simfut)))
  }


  #----- Step 3: detrend series
  if (detrend){

    # Estimate trends
    obstrend_raw <- lm(obshist ~ yearobshist, na.action = na.exclude) |>
      predict()
    obstrend <- obstrend_raw - mean(obstrend_raw, na.rm = TRUE)
    simhisttrend_raw <- lm(simhist ~ yearsimhist, na.action = na.exclude) |>
      predict()
    simhisttrend <- simhisttrend_raw - mean(simhisttrend_raw, na.rm = TRUE)
    simfuttrend_raw <- lm(simfut ~ yearsimfut, na.action = na.exclude) |>
      predict()
    simfuttrend <- simfuttrend_raw - mean(simfuttrend_raw, na.rm = TRUE)

    # Detrend
    obshist <- obshist - obstrend
    simhist <- simhist - simhisttrend
    simfut <- simfut - simfuttrend
  }

  #----- Step 5: Map the climate change signal of sim to obs

  # Compute empirical distribution function of observed series
  ecdfobs <- ecdf(obshist)(obshist)

  # Compute transfer function
  deltaadd <- quantile(simfut, ecdfobs) - quantile(simhist, ecdfobs)

  # Mapped future observed values
  obsfut <- deltaadd + obshist

  #----- Step 6: Quantile mapping

  # Fit Gaussian distributions to future series
  simfutcdf <- pnorm(simfut, mean(simfut, na.rm = T), sd(simfut, na.rm = T))

  # Map using "future observed"
  calsimfut <- qnorm(p = simfutcdf, mean = mean(obsfut, na.rm = T),
    sd = sd(obsfut, na.rm = T))

  #----- Step 7: add back trend
  if (detrend){
    calsimfut <- calsimfut + simfuttrend
  }

  # Return
  calsimfut
}
