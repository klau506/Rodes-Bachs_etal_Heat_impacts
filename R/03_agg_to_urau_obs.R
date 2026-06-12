################################################################################
#
# Heat-related health impacts across national mitigation and urban adaptation scenarios in European cities
#
# R Code Step 3: Aggregate observed gridded temperature at the URAU CODE level
#
# Clàudia Rodés-Bachs
#
################################################################################

cat(sprintf("%s Running 03_agg_to_urau_obs\n
            ==============================\n
            ==============================\n
            ==============================\n",
            as.character(Sys.time())),
    file = sprintf("%s/00_trace.txt", tdir), append = TRUE) |> try()


# initialize trace
dir.create(tdir, recursive = T, showWarnings = FALSE)
writeLines(c(""), sprintf("%s/03_trace.txt", tdir))
cat(sprintf("================================\n%s\n", as.character(Sys.time())),
    file = sprintf("%s/03_trace.txt", tdir), append = TRUE)


#------------------------
# COMPUTE daily means
#------------------------

# list available files
obs_files <- list.files(path = 'data/urbClim/Cerra',
                        pattern = "cerra_.*\\.grib$", recursive = TRUE, full.names = TRUE)


# variables accounted
variable_fun <- c('mean') # ('mean','min','max')


# for (ff_path in obs_files[(5*task_id+1):min(5*task_id+6,length(obs_files))]) {
for (ff_path in obs_files) {

  # load raster
  obs_raster <- rast(ff_path)

  # get time metadata
  all_times <- time(obs_raster)
  years  <- unique(as.numeric(format(all_times, "%Y")))
  months <- 1:12


  # nested loop for each var - year - month combination
  for (var in variable_fun) {
    for (yr in years) {
      for (mn in months) {

        # get the indices for THIS specific year and month
        idx <- which(year(all_times) == yr & month(all_times) == mn)

        # skip if the month doesn't exist
        if (length(idx) == 0) next

        cat(sprintf("%s Processing: %s - %s - %s\n",
                    as.character(Sys.time()), var, yr, mn),
            file = sprintf("%s/03_trace.txt", tdir), append = TRUE) |> try()


        # SUBSET
        # creates a pointer to just those layers
        obs_month_raster <- obs_raster[[idx]]

        # transform the urau map to match that SAME standard CRS
        urau_map_proj <- st_transform(urau_map, st_crs(obs_month_raster))

        # simplify the urau map by selecting the URAU_CODE & geom
        urau_map_proj <- urau_map_proj[, "URAU_CODE"]



        # CALCULATE
        # estimate daily [average / min / max] temperature
        daily_index <- format(time(obs_month_raster), "%Y-%m-%d")
        daily_raster <- tapp(obs_month_raster, index = daily_index, fun = var)
        names(daily_raster) <- unique(daily_index)

        # extract daily mean [average / min / max] temperature per URAU CODE
        city_temp <- exact_extract(daily_raster, urau_map_proj, "mean")

        # create dataset
        final_output <- cbind(st_drop_geometry(urau_map_proj["URAU_CODE"]), city_temp)

        # store the original dates
        dates <- as.Date(names(daily_raster))

        # rename columns from "mean.temp_i" to the actual dates
        colnames(final_output)[-1] <- as.character(dates)

        # convert the data.frame/sf object to a data.table
        setDT(final_output)

        # reshape from Wide to Long
        long_obs_data <- melt(final_output,
                               id.vars = "URAU_CODE",
                               variable.name = "date",
                               value.name = "temp")

        wide_obs_data <- long_obs_data[, .(URAU_CODE, date, temp)]

        # format columns to avoid Arrow errors
        wide_obs_data[, URAU_CODE := as.character(URAU_CODE)]
        wide_obs_data[, date := as.Date(date)]

        # add scenario column
        wide_obs_data[, scen := as.character('OBS')]



        # SAVE
        idir <- sprintf("%s/03_obs_temp_var_yr_mn/temp_%s/%s/%s",
                        tdir, var, yr, mn)
        dir.create(idir, recursive = T, showWarnings = FALSE)
        write_parquet(wide_obs_data, sprintf("%s/res.parquet", idir))



        # CLEAN UP
        rm(daily_raster, obs_month_raster, wide_obs_data,
           long_obs_data, final_output)
        tmpFiles(remove = TRUE)
        gc()


      }
    }
  }

}


#------------------------
# AGGREGATE annually
#------------------------

cat(as.character(Sys.time()), "Aggregate annual results by variable\n
    =======================================\n
    =======================================\n",
    file = sprintf("%s/03_trace.txt", tdir), append = TRUE) |> try()

for (var in variable_fun) {

  for (yr in seq(1990,2019)) {

    cat(sprintf("%s Aggregating: %s - %s\n",
                as.character(Sys.time()), var, yr),
        file = sprintf("%s/03_trace.txt", tdir), append = TRUE) |> try()


    # read and stack
    yr_obs_data <- open_dataset(sprintf("%s/03_obs_temp_var_yr_mn/temp_%s/%s",
                                        tdir, var, yr)) %>%
      collect() %>%
      as.data.table()

    # save
    idir <- sprintf("%s/03_obs_temp_var_yr/%s/%s",
                    tdir, var, yr)
    dir.create(idir, recursive = T, showWarnings = FALSE)
    write_parquet(yr_obs_data, sprintf("%s/res.parquet", idir))

  }
}

#------------------------
# AGGREGATE whole period
#------------------------

cat(as.character(Sys.time()), "Aggregate whole OBS period results by variable\n
    =======================================\n
    =======================================\n",
    file = sprintf("%s/03_trace.txt", tdir), append = TRUE) |> try()


for (var in variable_fun) {

  cat(sprintf("%s Aggregating: %s\n",
              as.character(Sys.time()), var),
      file = sprintf("%s/03_trace.txt", tdir), append = TRUE) |> try()


  # read and stack
  tt_obs_data <- open_dataset(sprintf("%s/03_obs_temp_var_yr/%s",
                                      tdir, var)) %>%
    collect() %>%
    as.data.table()
  # from K to C
  tt_obs_data[, temp := temp - 273.15]

  # rename temperature variable
  setnames(tt_obs_data, "temp", variables_map[var])

  # save
  dir.create(adir, recursive = T, showWarnings = FALSE)
  write_parquet(tt_obs_data, sprintf("%s/03_obs_data_%s.gz.parquet", adir, variables_map[var]))

}




#------------------------
# CLEAN
#------------------------

cat(as.character(Sys.time()), "03_agg_to_urau_obs.R run succesfully\n",
    file = sprintf("%s/03_trace.txt", tdir), append = TRUE) |> try()
cat(as.character(Sys.time()), "Cleaning temporal files and directories\n
    =======================================\n
    =======================================\n",
    file = sprintf("%s/03_trace.txt", tdir), append = TRUE) |> try()


# delete all created temporary files
if (deleteTempFiles) unlink(sprintf("%s/%s", tdir, c("03_obs_temp_var_yr", "03_obs_temp_var_yr_mn")), recursive = T)
tmpFiles(remove = TRUE)
gc()
