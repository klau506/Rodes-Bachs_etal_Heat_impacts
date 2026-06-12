################################################################################
#
# Heat-related health impacts across national mitigation and urban adaptation scenarios in European cities
#
# R Code Step 7: Estimate future (simulated) UHI
# R Code Step 7a: Time-related calibration of GCM with CERRA observed data --> RURAL TEMPERATURES
#
# Clàudia Rodés-Bachs
#
################################################################################

cat(sprintf("%s Running 07a_sim_uhi_bias_temporal\n
            ==============================\n
            ==============================\n
            ==============================\n",
            as.character(Sys.time())),
    file = sprintf("%s/00_trace.txt", tdir), append = TRUE) |> try()

# initialize trace
dir.create(tdir, recursive = T, showWarnings = FALSE)
writeLines(c(""), sprintf("%s/07a_trace.txt", tdir))
cat(sprintf("================================\n%s\n", as.character(Sys.time())),
    file = sprintf("%s/07a_trace.txt", tdir), append = FALSE) |> try()



# -------------------------------------------------------------------------------
# Calibrate simulated data
# -------------------------------------------------------------------------------
cat(sprintf("%s Apply temporal bias correction\n
            ==============================\n
            ==============================\n",
            as.character(Sys.time())),
    file = sprintf("%s/07a_trace.txt", tdir), append = TRUE) |> try()


# loop for each variable
for (var in variables) {
  temp_obs_city <- get(paste0(var, "_obs_city"))
  temp_sim_city <- get(paste0(var, "_sim_city"))

  for (uc in urau_codes) { #ES002C) {

    for (sc in scenarios) { #scenarios

      for (ml in models) {

        cat(sprintf("%s QM Step1 Processing urban sim data: %s - %s - %s - %s\n",
                    as.character(Sys.time()), uc, var, sc, ml),
            file = sprintf("%s/07a_trace.txt", tdir), append = TRUE) |> try()


        # restrict data to loop parameters
        obs_city_hist <- temp_obs_city[URAU_CODE == uc,
                                       .(date, tobs = get(var))]

        sim_city <- temp_sim_city[URAU_CODE == uc & scen == sc & model == ml,]

        if (nrow(sim_city) == 0 || nrow(obs_city_hist) == 0) {
          cat(sprintf("%s ERROR: empty obs or sim data for %s - %s - %s\n",
                      as.character(Sys.time()), as.character(uc), as.character(sc), as.character(ml)),
              file = sprintf("%s/07a_trace.txt", tdir), append = TRUE) |> try()
          next
        }


        sim_city$year <- year(sim_city$date)
        sim_city$month <- month(sim_city$date)
        sim_city$day <- day(sim_city$date)
        sim_city_hist <- sim_city[year %between% histrange,
                                  .(date, day, month, year, tsim = get(var), model)]

        # melt historical data
        t_hist <- merge(obs_city_hist,
                        sim_city_hist,
                        by = "date", all.y = T)
        t_hist <- dcast(t_hist,
                        date + tobs + day + month + year ~ model,
                        value.var = "tsim",
                        sep = "_")
        old_model_names <- ml
        new_model_names <- paste0("tsim_", old_model_names)
        setnames(t_hist, old = old_model_names, new = new_model_names)


        # create periods of calibration and 5y periods
        sim_city_df <- sim_city[year %in% min(histrange):(max(projrange)+perlen), .(date, day, month, year, tsim = get(var), model)]
        sim_city_df[, calperiod := cut(year, c(histrange[1], projrange), right = FALSE,
                                       labels = c("hist",
                                                  paste(projrange[-length(projrange)], projrange[-1] - 1, sep = "-")))]
        setkey(sim_city_df, calperiod, date, model)



        # create 5y periods
        t_hist[, year5 := (year %/% 5) * 5]
        sim_city_df[, year5 := (year %/% 5) * 5]



        # calibrate projections: for full climate change
        sim_city_df[, full := isimip3(
          obshist = t_hist[month == .BY$month, tobs],
          simhist = t_hist[month == .BY$month, .SD,
                           .SDcols = sprintf("tsim_%s", .BY$model)][[1]],
          simfut = tsim,
          yearobshist = t_hist[month == .BY$month, year],
          yearsimhist = t_hist[month == .BY$month, year],
          yearsimfut = year,
          uc = uc, var = var, sc = sc, ml = ml),
          by = .(month, calperiod, model)]


        # create the no climate change series --- attribution
        # recalibrate each 5y period on last 5y of historical period
        sim_city_df[, demo := isimip3(
          obshist = sim_city_df[month == .BY$month & model == .BY$model &
                                  year5 == (min(projrange) - perlen), full],
          simhist = full, simfut = full,
          yearobshist = sim_city_df[month == .BY$month & model == .BY$model &
                                      year5 == (min(projrange) - perlen), year],
          yearsimhist = year, yearsimfut = year,
          uc = uc, var = var, sc = sc, ml = ml),
          by = .(month, year5, model)]


        # add model, scenario, variable data
        sim_city_df[, `:=`(URAU_CODE = uc, scen = sc, model = ml, variable = var)]

        # save
        idir <- sprintf("%s/07_sim_calibStep1/%s/%s/%s/%s",
                        tdir, var, sc, ml, uc)
        dir.create(idir, recursive = T, showWarnings = FALSE)
        write_parquet(sim_city_df, sprintf("%s/res.gz.parquet", idir))




        # distributions to assess calibration
        # a) full distribution for historical period
        tsumhist <- sim_city_df[calperiod == "hist", c(list(perc = predper),
                                                       lapply(.SD, function(x) fquantile(x, predper / 100))),
                                .SDcols = c("tsim", "full", "demo"),
                                by = model]
        # b) reduced summary for other periods
        redper <- c(0, 1, 25, 50, 75, 99, 100)
        tsumproj <- sim_city_df[calperiod != "hist", c(list(perc = c(redper, "mean")),
                                                       lapply(.SD, function(x)
                                                         c(fquantile(x, redper / 100), mean(x)))),
                                by = .(calperiod, model),
                                .SDcols = c("tsim", "full", "demo")]
        # c) binding of both datasets
        tsum0 <- rbind(tsumhist[, calperiod := "hist"], tsumproj)
        tsum0[, `:=`(city = uc, scen = sc)]

        # plot_bias_correction_rsme_evaluation()


        # save
        idir <- sprintf("%s/07_sim_calibStep1_tsum/%s/%s/%s/%s",
                        tdir, var, sc, ml, uc)
        dir.create(idir, recursive = T, showWarnings = FALSE)
        write_parquet(tsum0, sprintf("%s/res.gz.parquet", idir))



        # CLEAN UP
        tmpFiles(remove = TRUE)
        gc()

      }
    }
  }


  # AGGREGATE by variable
  cat(sprintf("%s Aggregating temporal bias corrected sim data\n
              =======================================\n
              =======================================\n",
              as.character(Sys.time())),
      file = sprintf("%s/07a_trace.txt", tdir), append = TRUE) |> try()


  # read and stack - calibrated data
  sim_calib_data <- open_dataset(sprintf("%s/07_sim_calibStep1/%s",
                                             tdir, var)) %>%
    collect() %>%
    as.data.table()

  # save
  write_parquet(sim_calib_data, sprintf("%s/07_sim_calibStep1_%s.gz.parquet", adir, var))



  # read and stack - tsum data
  tsum_data <- open_dataset(sprintf("%s/07_sim_calibStep1_tsum/%s",
                                        tdir, var)) %>%
    collect() %>%
    as.data.table()

  # save
  write_parquet(tsum_data, sprintf("%s/07_sim_calibStep1_%s_tsum.gz.parquet", adir, var))



  # CLEAN UP
  tmpFiles(remove = TRUE)
  gc()

}



#------------------------
# CLEAN
#------------------------

cat(as.character(Sys.time()), "07a_sim_uhi_bias_temporal.R run succesfully\n",
    file = sprintf("%s/07a_trace.txt", tdir), append = TRUE) |> try()
cat(as.character(Sys.time()), "Cleaning temporal files and directories\n
    =======================================\n
    =======================================\n",
    file = sprintf("%s/07a_trace.txt", tdir), append = TRUE) |> try()


# delete all created temporary files
if (deleteTempFiles) unlink(sprintf("%s/%s", tdir, c("07_sim_calibStep1", "07_sim_calibStep1_tsum")), recursive = T)
tmpFiles(remove = TRUE)
gc()



