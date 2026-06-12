################################################################################
#
# Heat-related health impacts across national mitigation and urban adaptation scenarios in European cities
#
# R Code Step 7: Estimate future (simulated) UHI
# R Code Step 7b: Spatial-related calibration of step 7a output with CERRA observed data --> URBAN TEMPERATURES
#
# NOTE: to be run in parallel
#
# Clàudia Rodés-Bachs
#
################################################################################

cat(sprintf("%s Running 07b_sim_uhi_bias_spatial\n
            ==============================\n
            ==============================\n
            ==============================\n",
            as.character(Sys.time())),
    file = sprintf("%s/00_trace.txt", tdir), append = TRUE) |> try()

# initialize trace
dir.create(tdir, recursive = T, showWarnings = FALSE)
writeLines(c(""), sprintf("%s/07b_trace.txt", tdir))
cat(sprintf("================================\n%s\n", as.character(Sys.time())),
    file = sprintf("%s/07b_trace.txt", tdir), append = TRUE) |> try()



# -------------------------------------------------------------------------------
# Estimate historical urban & rural temperatures
# -------------------------------------------------------------------------------
cat(sprintf("%s Estimate historical urban & rural temperatures\n
    =======================================\n
    =======================================\n",
            as.character(Sys.time())),
    file = sprintf("%s/07b_trace.txt", tdir), append = TRUE) |> try()

urau_map_07b <- urau_map[urau_map$URAU_CODE %in% urau_codes, ]

# loop for each year
y_range <- histrange[1]:histrange[2]
for (yr in y_range[1+(i*5)]:y_range[(i*5)+5]) {
  for (mn in 1:12) {

    cat(sprintf("%s Processing UHI obs data: %s - %s\n",
                as.character(Sys.time()), as.character(yr), as.character(mn)),
        file = sprintf("%s/07b_trace.txt", tdir), append = TRUE) |> try()

    # load data
    obs_grid <- rast(paste0("data/urbClim/Cerra/cerra_",yr,".grib"))


    # get the indices for THIS specific year and month
    all_times <- time(obs_grid)
    idx <- which(year(all_times) == yr & month(all_times) == mn)

    # skip if the month doesn't exist
    if (length(idx) == 0) next



    # SUBSET
    # creates a pointer to just those layers
    obs_grid_mn <- obs_grid[[idx]]
    names(obs_grid_mn) <- format(time(obs_grid_mn), "%b %d %H:%M")

    # transform the urau map to match that SAME standard CRS
    urau_map_proj <- st_transform(urau_map_07b, st_crs(obs_grid_mn))

    # simplify the urau map by selecting the URAU_CODE & geom
    urau_map_proj <- urau_map_proj[, "URAU_CODE"]




    # COMPUTE 1
    # overlap with urban-rural mask and compute the urban % per temperature grid cell

    # compute urban & rural fraction by temperature grid cell
    if (!exists("urban_fraction") || !exists("rural_fraction")) {
      rural_fraction <- project(urau_land_cover_binary_clean , obs_grid_mn[[1]], method = "average")
      urban_fraction <- 1 - rural_fraction
    }

    # extract urban & rural temperature per URAU CODE
    city_temp_urban <- exact_extract(obs_grid_mn, urau_map_proj,
                                     function(values, coverage_fraction, weights) {
                                       # values is Pixels x Layers
                                       # weights is a vector (Pixels)
                                       # multiply values by weights (NAs will propagate here)
                                       weighted_vals <- values * weights
                                       # sum only the non-NA weighted values for each layer
                                       numerator <- colSums(weighted_vals, na.rm = TRUE)
                                       # sum weights only where values are NOT NA
                                       # use values[,1] to get a single vector of NA positions
                                       denominator <- sum(weights[!is.na(values[, 1])], na.rm = TRUE)
                                       # return the weighted mean (if denominator is 0, returns NaN but stays in matrix)
                                       return(numerator / denominator)
                                     },
                                     weights = urban_fraction)
    city_temp_rural <- exact_extract(obs_grid_mn, urau_map_proj,
                                     function(values, coverage_fraction, weights) {
                                       # values is Pixels x Layers
                                       # weights is a vector (Pixels)
                                       # multiply values by weights (NAs will propagate here)
                                       weighted_vals <- values * weights
                                       # sum only the non-NA weighted values for each layer
                                       numerator <- colSums(weighted_vals, na.rm = TRUE)
                                       # sum weights only where values are NOT NA
                                       # use values[,1] to get a single vector of NA positions
                                       denominator <- sum(weights[!is.na(values[, 1])], na.rm = TRUE)
                                       # return the weighted mean (if denominator is 0, returns NaN but stays in matrix)
                                       return(numerator / denominator)
                                     },
                                     weights = rural_fraction)

    tmpFiles(remove = TRUE)
    gc()



    # COMPUTE 2
    # reshape datasets and add date info

    # convert to data.table and pivot long - URBAN
    urban_dt <- as.data.table(t(city_temp_urban))
    setnames(urban_dt, names(obs_grid_mn))
    urban_dt[, URAU_CODE := urau_map_07b$URAU_CODE]

    urban_long_dt <- melt(urban_dt,
                          id.vars = "URAU_CODE",
                          variable.name = "hour",
                          value.name = "urban_temp")
    setkey(urban_long_dt, URAU_CODE, hour)


    # convert to data.table and pivot long - RURAL
    rural_dt <- as.data.table(t(city_temp_rural))
    setnames(rural_dt, names(obs_grid_mn))
    rural_dt[, URAU_CODE := urau_map_07b$URAU_CODE]

    rural_long_dt <- melt(rural_dt,
                          id.vars = "URAU_CODE",
                          variable.name = "hour",
                          value.name = "rural_temp")
    setkey(rural_long_dt, URAU_CODE, hour)


    # merge datasets
    city_long_dt <- urban_long_dt[rural_long_dt]



    city_long_dt[, hour := sub("mean\\.", "", hour)]
    city_long_dt[, datetime := as.POSIXct(paste(yr, hour),
                                          format = "%Y %b %d %H:%M",
                                          tz = "UTC")]
    city_long_dt[, date := as.IDate(datetime)]
    city_long_dt[, hour := as.ITime(datetime)]
    city_long_dt[, datetime := NULL]


    # add season column
    city_long_dt[, season := fcase(
      month(date) %in% c(12, 1, 2),  "DJF",
      month(date) %in% c(3, 4, 5),   "MAM",
      month(date) %in% c(6, 7, 8),   "JJA",
      month(date) %in% c(9, 10, 11), "SON"
    )]



    # COMPUTE 3
    # compute daily average
    city_long_day_dt <- city_long_dt[, .(
      urban_tmean = mean(urban_temp, na.rm = TRUE),
      rural_tmean = mean(rural_temp, na.rm = TRUE)
    ),
    by = .(URAU_CODE, date, season)]



    # SAVE
    idir <- sprintf("%s/07_obs_calibStep2_hourly_yr_mn/%s/%s",
                    tdir, yr, mn)
    dir.create(idir, recursive = T, showWarnings = FALSE)
    write_parquet(city_long_dt, sprintf("%s/res.parquet", idir))

    idir <- sprintf("%s/07_obs_calibStep2_daily_yr_mn/%s/%s",
                    tdir, yr, mn)
    dir.create(idir, recursive = T, showWarnings = FALSE)
    write_parquet(city_long_day_dt, sprintf("%s/res.parquet", idir))


    rm(obs_grid_mn, city_long_day_dt,
       city_long_dt, rural_dt, urban_dt,
       city_temp_urban, city_temp_rural)
    tmpFiles(remove = TRUE)
    gc()

  }
}


# AGGREGATE by year
cat(sprintf("%s Aggregating urban & rural obs data\n
            =======================================\n
            =======================================\n",
            as.character(Sys.time())),
    file = sprintf("%s/07b_trace.txt", tdir), append = TRUE) |> try()

for (yr in histrange[1]:histrange[2]) {
  # read and stack
  city_obs_data <- open_dataset(sprintf("%s/07_obs_calibStep2_daily_yr_mn/%s",
                                             tdir, yr)) %>%
    collect() %>%
    as.data.table()

  # save
  idir <- sprintf("%s/07_obs_calibStep2_daily_yr/%s", tdir, yr)
  dir.create(idir, recursive = T, showWarnings = FALSE)
  write_parquet(city_obs_data, sprintf("%s/res.parquet", idir))

}


# AGGREGATE all annual datasets
# read and stack
city_obs_data <- open_dataset(sprintf("%s/07_obs_calibStep2_daily_yr",
                                      tdir)) %>%
  collect() %>%
  as.data.table()

# save
write_parquet(city_obs_data, sprintf("%s/07_obs_calibStep2.gz.parquet", adir))



# CLEAN UP
tmpFiles(remove = TRUE)
gc()





# -------------------------------------------------------------------------------
# Apply spatial bias correction
# -------------------------------------------------------------------------------
cat(sprintf("%s Apply spatial bias correction\n
    =======================================\n
    =======================================\n",
            as.character(Sys.time())),
    file = sprintf("%s/07b_trace.txt", tdir), append = TRUE) |> try()

# load `07a - calibStep1` data -- GCM temporally-calibrated temperature
calibStep1 <- read_parquet(sprintf("%s/07_sim_calibStep1_tg.gz.parquet", adir)) %>%
  collect()
gc()

# load `07b - calibStep2.1` data -- OBS urban & rural daily data
city_obs_data <- read_parquet(sprintf("%s/07_obs_calibStep2.gz.parquet", adir)) %>%
  collect()
gc()


# loop for URAU CODE & scenario
for (uc in urau_codes) { # uc = 'ES002C'
  for (sc in scenarios) {       # sc = 'BASE'

    # check if the loop is required for that uc - sc combination
    date_limit <- as.Date("2026-04-21")
    idir <- sprintf("%s/07_sim_calibStep2/%s/%s/res.parquet", tdir, uc, sc)
    if (file.exists(idir)) {
      info <- file.info(idir)
      date_modification <- as.Date(info$mtime)
      if (date_modification >= date_limit) next
    }

    cat(sprintf("%s QM Step2 Processing urban sim data: %s - %s\n",
                as.character(Sys.time()), as.character(uc), as.character(sc)),
        file = sprintf("%s/07b_trace.txt", tdir), append = TRUE) |> try()


    # SUBSET
    city_obs_data_uc <- city_obs_data[URAU_CODE == uc]
    city_obs_data_uc$year <- year(city_obs_data_uc$date)
    sim_step1_uc <- calibStep1[URAU_CODE == uc & scen == sc & variable == 'tg',]


    # COMPUTE
    # apply mean daily OBS urban VS rural data to temporally calibrated GCM
    # calibrate projections: for full climate change
    sim_step1_uc[, full_urban := isimip3(
      obshist = city_obs_data_uc[month == .BY$month, urban_tmean],
      simhist = city_obs_data_uc[month == .BY$month, rural_tmean],
      simfut = full,
      yearobshist = city_obs_data_uc[month == .BY$month, year],
      yearsimhist = city_obs_data_uc[month == .BY$month, year],
      yearsimfut = year,
      uc = uc, var = var, sc = sc, ml = ml),
      by = .(month, calperiod, model)]

    # calibrate projections: for demo, i.e., no climate change
    sim_step1_uc[, demo_urban := isimip3(
      obshist = city_obs_data_uc[month == .BY$month, urban_tmean],
      simhist = city_obs_data_uc[month == .BY$month, rural_tmean],
      simfut = demo,
      yearobshist = city_obs_data_uc[month == .BY$month, year],
      yearsimhist = city_obs_data_uc[month == .BY$month, year],
      yearsimfut = year,
      uc = uc, var = var, sc = sc, ml = ml),
      by = .(month, calperiod, model)]


    # SAVE
    idir <- sprintf("%s/07_sim_calibStep2/%s/%s",
                    tdir, uc, sc)
    dir.create(idir, recursive = T, showWarnings = FALSE)
    write_parquet(sim_step1_uc, sprintf("%s/res.parquet", idir))

    rm(sim_step1_uc)
    tmpFiles(remove = TRUE)
    gc()
}
}




# AGGREGATE
cat(sprintf("%s Aggregating spatial bias corrected sim data\n
    =======================================\n
    =======================================\n",
            as.character(Sys.time()), var),
    file = sprintf("%s/07b_trace.txt", tdir), append = TRUE) |> try()

# read and stack
city_sim_data <- open_dataset(sprintf("%s/07_sim_calibStep2", tdir)) %>%
  collect() %>%
  as.data.table()


# save
write_parquet(city_sim_data, sprintf("%s/07_sim_calibStep2.gz.parquet", adir))




#------------------------
# CLEAN
#------------------------

cat(as.character(Sys.time()), "07b_sim_uhi_bias_spatial.R run succesfully\n",
    file = sprintf("%s/07b_trace.txt", tdir), append = TRUE) |> try()
cat(as.character(Sys.time()), "Cleaning temporal files and directories\n
    =======================================\n
    =======================================\n",
    file = sprintf("%s/07b_trace.txt", tdir), append = TRUE) |> try()


# delete all created temporary files
if (deleteTempFiles) {
  unlink(sprintf("%s/%s", tdir, c("07_obs_calibStep2_hourly_yr_mn", "07_obs_calibStep2_daily_yr_mn",
                                  "07_obs_calibStep2_daily_yr", "07_sim_calibStep2")), recursive = T)
}
tmpFiles(remove = TRUE)
gc()


