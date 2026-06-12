################################################################################
#
# Heat-related health impacts across national mitigation and urban adaptation scenarios in European cities
#
# R Code Step 9a: Perform Health Impact Assessment (HIA) at the URAU CODE level
#
# NOTE: to be run in parallel
#
# Clàudia Rodés-Bachs
#
################################################################################

cat(sprintf("%s Running 09a_hia\n
            ==============================\n
            ==============================\n
            ==============================\n",
            as.character(Sys.time())),
    file = sprintf("%s/00_trace.txt", tdir), append = TRUE) |> try()


# initialize trace
dir.create(tdir, recursive = T, showWarnings = FALSE)
writeLines(c(""), sprintf("%s/09_trace.txt", tdir))
cat(sprintf("================================\n%s\n", as.character(Sys.time())),
    file = sprintf("%s/09_trace.txt", tdir), append = T) |> try()


# -------------------------------------------------------------------------------
# 1. Perform HIA
# -------------------------------------------------------------------------------

## -- load 08 output
results_temperature <- open_dataset(sprintf("%s/08_final_datasets", tdir),
                                    partitioning = c("URAU_CODE","scen")) %>%
  mutate(settl_temp = settl) %>%
  select(-settl)

print(i)
# loop through URAU CODEs and scenarios
for (iuc in urau_codes[(i*25+1):(min((i+1)*25,length(urau_codes)))]) {
  for (isc in scenarios) {
    print(iuc)
    # check if the loop is required for that uc - sc combination
    time_limit_22 <- as.POSIXct("2026-07-09 08:00:00")

    idir <- sprintf("%s/09_hia_city_period/%s/%s/all/res.parquet", tdir, iuc, isc)
    if (file.exists(idir)) {
      info <- file.info(idir)
      date_modification <- info$mtime
      if (date_modification >= time_limit_22) {
        cat(sprintf("%s Skipping HIA1: %s - %s\n",
                    as.character(Sys.time()), as.character(iuc), as.character(isc)),
            file = sprintf("%s/09_trace.txt", tdir), append = TRUE) |> try()
        next
      }
    }

    cat(sprintf("%s Performing HIA3: %s - %s\n",
                as.character(Sys.time()), as.character(iuc), as.character(isc)),
        file = sprintf("%s/09_trace.txt", tdir), append = TRUE) |> try()

    # -------------------------------------------------------------------------------
    # 1. Load data
    # -------------------------------------------------------------------------------

    ## -- projection data

    # load
    tmeandf <- results_temperature %>%
      filter(URAU_CODE == iuc & scen == isc &
               !is.na(tas)) %>%
      select(!c(scen, URAU_CODE)) %>%
      collect() %>%
      as.data.table()

    if (nrow(tmeandf) == 0 || length(unique(tmeandf$adapt)) < 2) {
      cat(sprintf("%s ERROR: empty simulated data for %s - %s\n",
                  as.character(Sys.time()), as.character(iuc), as.character(isc)),
          file = sprintf("%s/09_trace.txt", tdir), append = TRUE) |> try()
      next
    }

    # restrict to project years
    setkey(tmeandf, year, month, day)
    tmeandf <- tmeandf[year < max(projrange)]




    ## -- historical data

    # load
    tmeanhist <- raw_era5_temp |>
      filter(URAU_CODE == iuc) |> collect() |>
      rename(tmeanobs = era5landtmean) |>
      dplyr::select(!URAU_CODE) |> as.data.table()
    tmeanhist[, year := year(date)]

    # extract quantiles before doing any selection
    tper <- quantile(tmeanhist$tmeanobs, predper / 100)

    # add GCMs for historical period
    tmeanhist <- merge(tmeanhist,
                       tmeandf[year %between% histrange, ],
                       by = c("date","year"), all.y = T)
    # reshape dataset
    imodels <- unique(tmeanhist$gcm)
    tmeanhist <- dcast(tmeanhist,
                       date + tmeanobs + day + month + year + year5 + calperiod + settl_temp + tree_cover + adapt ~ gcm,
                       value.var = "tas")
    setnames(tmeanhist, old = imodels, new = paste0("tas_", imodels))




    # -------------------------------------------------------------------------------
    # 2. Calibrate data
    # -------------------------------------------------------------------------------


    # -- calibration of projections
    setkey(tmeandf, calperiod, date, gcm, settl_temp)

    # calibrate projections: for full climate change
    tmeandf[, full := isimip3(
      obshist = tmeanhist[month == .BY$month, tmeanobs],
      simhist = tmeanhist[month == .BY$month, .SD,
                          .SDcols = sprintf("tas_%s", .BY$gcm)][[1]],
      simfut = tas,
      yearobshist = tmeanhist[month == .BY$month, year],
      yearsimhist = tmeanhist[month == .BY$month, year],
      yearsimfut = year,
      uc = iuc, var = 'tg', sc = isc, ml = 'ml'),
      by = .(month, calperiod, gcm, settl_temp)]

    # create the no climate change series: recalibrate each
    # 5y period on last 5y of historical period
    tmeandf[, demo := isimip3(
      obshist = tmeandf[month == .BY$month & gcm == .BY$gcm &
                          year5 == (min(projrange) - perlen), full],
      simhist = full, simfut = full,
      yearobshist = tmeandf[month == .BY$month & gcm == .BY$gcm &
                              year5 == (min(projrange) - perlen), year],
      yearsimhist = year, yearsimfut = year,
      uc = iuc, var = 'tg', sc = isc, ml = 'ml'),
      by = .(month, year5, gcm, settl_temp)]



    # -- export summary of temperature

    # full distribution for historical period (to assess calibration)
    tsumhist <- tmeandf[calperiod == "hist", c(list(perc = predper),
                                               lapply(.SD, function(x) fquantile(x, predper / 100))),
                        .SDcols = c("tas", "full", "demo"),
                        by = gcm]

    # reduced summary for other periods
    redper <- c(0, 1, 25, 50, 75, 99, 100)
    tsumproj <- tmeandf[calperiod != "hist", c(list(perc = c(redper, "mean")),
                                               lapply(.SD, function(x)
                                                 c(fquantile(x, redper / 100), mean(x)))),
                        by = .(calperiod, gcm),
                        .SDcols = c("tas", "full", "demo")]

    # save
    idir <- sprintf("%s/09_calibStep1_tsum/%s/%s", tdir, iuc, isc)
    dir.create(idir, recursive = T, showWarnings = FALSE)
    rbind(tsumhist[, calperiod := "hist"], tsumproj) |>
      write_parquet(sprintf("%s/tsum.parquet",idir))




    # -------------------------------------------------------------------------------
    # 3. Apply ERFs
    # -------------------------------------------------------------------------------

    # -- preprocess ERF

    # remove part of the historical period for which we don't want ANs
    tmeandf <- tmeandf[year >= (min(projrange) - perlen),]

    # prepare basis parameters (common to all age groups)
    # tper extracted above (after reading tmeanhist)
    varknots <- tper[paste0(varper, ".0%")]
    varbound <- range(tper)
    argvar <- list(fun = varfun, degree = vardegree, knots = varknots,
                   Bound = varbound)

    # loop on age groups
    for (a in agelabs){

      # -- load coefficients

      # point estimate
      ptcoef <- subset(coefs, URAU_CODE == iuc & agegroup == a) |>
        select(matches("b[[:digit:]]"))

      # load simulations
      simcoef <- raw_coef_hia |>
        filter(URAU_CODE == iuc & agegroup == a, sim <= nsim) |>
        select(matches("b[[:digit:]]")) |>
        collect()

      # merge
      allcoefs <- rbind(ptcoef, simcoef) |> t()
      colnames(allcoefs) <- c("est", sprintf("sim%i", 1:nsim))

      # -- prepare the basis

      # evaluate MMT
      bper <- suppressWarnings(do.call(onebasis, c(list(x = tper), argvar)))
      ind <- tper %between% tper[c("25.0%", "99.0%")] # to avoid 0s and negative values
      mmt <- tper[ind][which.min(drop(bper[ind,] %*% allcoefs[,1]))]

      # create centred basis
      cenvec <- do.call(onebasis, c(list(x = mmt), argvar))

      # extract death rate projections
      agedf <- tmeandf[, .(year, year5, gcm, settl_temp, adapt, full, demo)]

      # add settlement type population percentage
      settl_temp_pop_map <- data.frame(
        settl_temp  = c('rural', 'rural', 'urban'),
        settl_pop   = c('total', 'rural', 'urban')
      )

      agedf <- merge(agedf, settl_temp_pop_map, allow.cartesian = T)

      agedf <- merge(
        agedf,
        projdata[URAU_CODE == iuc & agegroup == a,
                 .(year5, death, pop, settl_pop, settl_pop_perc)],
        by = c("year5", "settl_pop"), all.x = T)

      # apply settlement type population percentage
      agedf[, ":="(pop = pop * settl_pop_perc, death = death * settl_pop_perc)]
      agedf[, settl_pop_perc := NULL]

      # melt for more efficient computation
      agedf <- melt(agedf, measure.vars = c("full", "demo"),
                    variable.name = "sc", value.name = "tas")
      agedf[, range := factor(tas > mmt, lab = c("cold", "heat"))]



      # -- compute annual AN

      # compute total AN for each year & temp range
      setkey(agedf, sc, range, year, gcm, settl_temp, settl_pop, adapt)
      resy <- agedf[, .(pop = pop[1], death = death[1],
                        res = c("est", sprintf("sim%i", 1:nsim)), an = {

                          # compute RR
                          bcen <- do.call(onebasis, c(list(x = tas), argvar)) |>
                            scale(center = cenvec, scale = F) |>
                            suppressWarnings()
                          rr <- pmax(exp(bcen %*% allcoefs), 1)

                          # compute AN
                          an <- (1 - 1 / rr) * death / 365

                          # aggregate
                          colSums(an)

                        }), by = c("sc", "range", "year", "gcm", "settl_temp", "settl_pop", "adapt")]

      # fill missing years with zero
      keys <- unique(agedf[, c("range", "year", "gcm", "settl_temp", "settl_pop", "adapt")])
      res_vector <- c("est", sprintf("sim%i", 1:nsim))
      sc_vector <- c("full","demo")
      resy <- keys[, as.list(CJ(res = res_vector, sc = sc_vector)), by = .(range, year, gcm, settl_temp, settl_pop, adapt)] |>
        join(resy, how = "left") |>
        as.data.table()
      resy[, ":="(death = unique(na.omit(death)),
                  pop = unique(na.omit(pop))),
           by = c("year","settl_temp","settl_pop")]
      setnafill(resy, fill = 0, cols = "an")


      # save
      idir <- sprintf("%s/09_hia_raw/%s/%s/%s", tdir, iuc, isc, a)
      dir.create(idir, recursive = T, showWarnings = FALSE)
      write_parquet(resy, sprintf("%s/res.parquet",idir))



      # -- compute impacts

      # total tmean range
      resy <- impact_aggregate(resy, agg = c(range = "tot"), vars = "an",
                               by = c("sc", "year", "gcm", "settl_temp", "settl_pop", "adapt", "range", "res", "pop", "death"))

      # compute part due to climate change
      resy <- dcast(resy, range + year + res + gcm + settl_temp + settl_pop + adapt + death + pop ~ sc,
                    value.var = "an")
      resy[, clim := full - demo]
      # save raw data to calculate Global CI
      idir <- sprintf("%s/09_hia_global/%s/%s/%s", tdir, iuc, isc, a)
      dir.create(idir, recursive = T, showWarnings = FALSE)
      write_parquet(resy, sprintf("%s/res.parquet",idir))


      # compute impact measures and rename
      resy <- impact_measures(resy, vars = c("full", "demo", "clim"),
                              by = c("year", "adapt", "settl_temp", "settl_pop", "gcm", "range", "res"))
      setnames(resy, c("full", "demo", "clim"),
               sprintf("an_%s", c("full", "demo", "clim")))
      allvars <- lapply(c("full", "demo", "clim"), grep, names(resy),
                        value = T) |> unlist()

      # aggregate by period
      resy[, period := floor(year / perlen) * perlen]
      periodres <- impact_summarise(resy, vars = allvars,
                                    by = c("period", "range", "settl_temp", "settl_pop", "adapt"))
      yearres <- impact_summarise(resy, vars = allvars,
                                    by = c("year", "range", "settl_temp", "settl_pop", "adapt"))

      # save
      idir <- sprintf("%s/09_hia_city_period/%s/%s/%s",
                      tdir, iuc, isc, a)
      dir.create(idir, recursive = T, showWarnings = F)
      write_parquet(periodres, sprintf("%s/res.parquet", idir))

      idir <- sprintf("%s/09_hia_city_year/%s/%s/%s",
                      tdir, iuc, isc, a)
      dir.create(idir, recursive = T, showWarnings = F)
      write_parquet(yearres, sprintf("%s/res.parquet", idir))

      # clean
      rm(resy); gc()
    }



    # -- aggregate impacts for all ages

    # sum results for all ages
    resall <- open_dataset(sprintf("%s/09_hia_raw/%s/%s", tdir, iuc, isc),
                           partitioning = c("agegroup")) |>
      group_by(year, gcm, adapt, range, res, sc, settl_pop) |>
      summarise(
        across(all_of(c("an", "pop", "death")), sum),
        settl_temp = min(settl_temp),
        .groups = "drop"
      ) |>
      collect() |>
      as.data.table()

    # total tmean range
    resall <- impact_aggregate(resall, agg = c(range = "tot"), vars = "an",
                               by = c("sc", "year", "gcm", "settl_temp", "settl_pop", "adapt", "range", "res", "pop", "death"))

    # compute part due to climate change
    resall <- dcast(resall, range + year + adapt + res + gcm + death + pop + settl_temp + settl_pop ~ sc,
                  value.var = "an")
    resall[, clim := full - demo]

    # save raw data to calculate Global CI
    idir <- sprintf("%s/09_hia_global/%s/%s/all", tdir, iuc, isc)
    dir.create(idir, recursive = T, showWarnings = FALSE)
    write_parquet(resall, sprintf("%s/res.parquet",idir))


    # compute impact measures and rename
    resall <- impact_measures(resall, vars = c("full", "demo", "clim"),
                            by = c("year", "gcm", "settl_temp", "settl_pop", "adapt", "range", "res"))
    setnames(resall, c("full", "demo", "clim"),
             sprintf("an_%s", c("full", "demo", "clim")))
    allvars <- lapply(c("full", "demo", "clim"), grep, names(resall),
                      value = T) |> unlist()

    # aggregate by period
    resall[, period := floor(year / perlen) * perlen]
    periodresall <- impact_summarise(resall, vars = allvars,
                                     by = c("period", "range", "settl_temp", "settl_pop", "adapt"))

    yearresall <- impact_summarise(resall, vars = allvars,
                                   by = c("year", "range", "settl_temp", "settl_pop", "adapt"))


    # save
    idir <- sprintf("%s/09_hia_city_period/%s/%s/all",
                    tdir, iuc, isc)
    dir.create(idir, recursive = T, showWarnings = F)
    write_parquet(periodresall, sprintf("%s/res.parquet", idir))

    idir <- sprintf("%s/09_hia_city_year/%s/%s/all",
                    tdir, iuc, isc)
    dir.create(idir, recursive = T, showWarnings = F)
    write_parquet(yearresall, sprintf("%s/res.parquet", idir))


  }
}


# -------------------------------------------------------------------------------
# 2. Aggregate data
# -------------------------------------------------------------------------------

# -- HIA by year
# read and stack
hia_data <- open_dataset(sprintf("%s/09_hia_city_year", tdir),
                         partitioning = c("URAU_CODE", "scen", "agegroup")) %>%
  filter(!URAU_CODE %in% c('Regionl','Regional_age','Global','Global_age')) %>%
  collect() %>%
  as.data.table()

# reshape
metrics <- c("an", "af", "rate", "cuman")
res <- melt(hia_data,
            id.vars = c("year", "range", "URAU_CODE", "scen", "settl_temp", "settl_pop", "adapt", "agegroup"),
            measure.vars = patterns(
              est  = "_est$",
              low  = "_low$",
              high = "_high$"
            ),
            variable.name = "group_idx")

# get the names of the columns that matched the 'est' pattern
all_est_cols <- names(hia_data)[names(hia_data) %like% "_est$"]
# extract the middle part (e.g., 'full', 'demo', or 'clim')
res[, climate := sub("^[a-z]+_(.*)_est$", "\\1", all_est_cols)[group_idx]]
# extract the metric part (e.g., 'an', 'af')
res[, metric := sub("^([a-z]+)_.*_est$", "\\1", all_est_cols)[group_idx]]
# clean
res[, group_idx := NULL]


# save
write_parquet(res, sprintf("%s/09_hia_city_year.gz.parquet", adir))

# plot_hia()



# -- HIA by period
# read and stack
hia_data <- open_dataset(sprintf("%s/09_hia_city_period", tdir),
                         partitioning = c("URAU_CODE", "scen", "agegroup")) %>%
  filter(!URAU_CODE %in% c('Regionl','Regional_age','Global','Global_age')) %>%
  collect() %>%
  as.data.table()


# reshape
metrics <- c("an", "af", "rate", "cuman")
res <- melt(hia_data,
            id.vars = c("period", "range", "URAU_CODE", "scen", "settl_temp", "settl_pop", "adapt", "agegroup"),
            measure.vars = patterns(
              est  = "_est$",
              low  = "_low$",
              high = "_high$"
            ),
            variable.name = "group_idx")

# get the names of the columns that matched the 'est' pattern
all_est_cols <- names(hia_data)[names(hia_data) %like% "_est$"]
# extract the middle part (e.g., 'full', 'demo', or 'clim')
res[, climate := sub("^[a-z]+_(.*)_est$", "\\1", all_est_cols)[group_idx]]
# extract the metric part (e.g., 'an', 'af')
res[, metric := sub("^([a-z]+)_.*_est$", "\\1", all_est_cols)[group_idx]]
# clean
res[, group_idx := NULL]


# save
write_parquet(res, sprintf("%s/09_hia_city_period.gz.parquet", adir))




# -- tsum
# read and stack
tsum_data <- open_dataset(sprintf("%s/09_calibStep1_tsum", tdir),
                          partitioning = c("URAU_CODE", "scen")) %>%
  collect() %>%
  as.data.table()

# save
write_parquet(tsum_data, sprintf("%s/09_hia_city_tsum.gz.parquet", adir))


# plot_tsum_rsme()

# plot_hia_temp_calib()



#------------------------
# CLEAN
#------------------------

cat(as.character(Sys.time()), i, "09_hia.R run succesfully\n",
    file = sprintf("%s/09_trace.txt", tdir), append = TRUE) |> try()
cat(as.character(Sys.time()), "Cleaning temporal files and directories\n
    =======================================\n
    =======================================\n",
    file = sprintf("%s/09_trace.txt", tdir), append = TRUE) |> try()


# delete all created temporary files
if (deleteTempFiles) {
  unlink(sprintf("%s/%s", tdir, c("09_calibStep1_tsum",
                                  "09_hia_city_year",
                                  "09_hia_city_period")), recursive = T)
}
tmpFiles(remove = TRUE)
gc()

