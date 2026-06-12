################################################################################
#
# TITLE
#
# R Code Step 4: Aggregate simulated gridded temperature at the URAU CODE level
#
# Clàudia Rodés-Bachs
#
################################################################################

cat(sprintf("%s Running 04_agg_to_urau_sim\n
            ==============================\n
            ==============================\n
            ==============================\n",
            as.character(Sys.time())),
    file = sprintf("%s/00_trace.txt", tdir), append = TRUE) |> try()


# initialize trace
dir.create(tdir, recursive = T, showWarnings = FALSE)
writeLines(c(""), sprintf("%s/04_trace.txt", tdir))
cat(sprintf("================================\n%s\n", as.character(Sys.time())),
    file = sprintf("%s/04_trace.txt", tdir), append = FALSE) |> try()



#------------------------
# COMPUTE daily means
#------------------------

# list available files
tg_files <- list.files(path = 'data/fromDIPC/',
                       pattern = "tas_global_daily.*_2064\\.nc$", recursive = TRUE, full.names = TRUE)

# store the paths in a list
file_list <- list(
  tg = tg_files
)


# load dummy simulated data to do reprojections
dummy_raster <- rast(tg_files[1])

# align the Coordinate Reference Systems (CRS) to EPSG:4326
# move the urau map to match the temp raster
urau_map_proj <- st_transform(urau_map, crs(dummy_raster))

# simplify the urau map by selecting the URAU_CODE & geom
urau_map_proj <- urau_map_proj[, "URAU_CODE"]

# clean
rm(dummy_raster); gc()

# nested loop for each available variable
for (var in variables) {

  # get the SPECIFIC file path
  current_files <- file_list[[var]]


  for (ff_path in current_files) {

    # get the SPECIFIC data file
    ff_data <- rast(ff_path)
    ext(ff_data) <- c(-180, 180, -90, 90)
    crs(ff_data) <- "EPSG:4326"

    # skip if the file is empty
    if (nrow(ff_data) == 0) next

    ff_name  <- basename(ff_path)
    model    <- sub("_.*", "", ff_name)
    scenario <- regmatches(ff_path, regexec("W5E5v2_(.*?)_tas", ff_path))[[1]][2]

    cat(sprintf("%s Processing: %s - %s - %s\n",
                as.character(Sys.time()), var, model, scenario),
        file = sprintf("%s/04_trace.txt", tdir), append = TRUE) |> try()




    # CALCULATE
    # extract daily mean temperature per URAU CODE
    city_temp <- exact_extract(ff_data, urau_map_proj, 'mean',
                               max_cells_in_memory = maxmem)

    # create dataset
    final_output <- cbind(st_drop_geometry(urau_map_proj["URAU_CODE"]), city_temp)

    # store the original dates
    dates <- as.Date(time(ff_data))

    # rename columns from "mean.tas_i" to the actual dates
    colnames(final_output)[-1] <- as.character(dates)

    # convert the data.frame/sf object to a data.table
    setDT(final_output)

    # reshape from Wide to Long
    long_sim_data <- melt(final_output,
                          id.vars = "URAU_CODE",
                          variable.name = "date",
                          value.name = "temp_k")
    # from K to C
    long_sim_data[, temp := temp_k - 273.15]

    # simplify dataset
    wide_sim_data <- long_sim_data[, .(URAU_CODE, date, temp)]

    # format columns to avoid Arrow errors
    wide_sim_data[, URAU_CODE := as.character(URAU_CODE)]
    wide_sim_data[, date := as.Date(date)]

    # add scenario and model column
    wide_sim_data[, scen := as.character(scenario)]
    wide_sim_data[, model := as.character(model)]



    # SAVE
    idir <- sprintf("%s/04_sim_temp_model_scen/%s/%s/%s",
                    tdir, var, model, scenario)
    dir.create(idir, recursive = T, showWarnings = FALSE)
    write_parquet(wide_sim_data, sprintf("%s/res.parquet", idir))



    # CLEAN UP
    rm(ff_data, long_sim_data, wide_sim_data, final_output)
    tmpFiles(remove = TRUE)
    gc()

  }
}


#------------------------
# AGGREGATE by variable
#------------------------

cat(as.character(Sys.time()), "Aggregate results by variable\n
    =======================================\n
    =======================================\n",
    file = sprintf("%s/04_trace.txt", tdir), append = TRUE) |> try()

for (var in variables) {

  # read and stack
  tt_sim_data <- open_dataset(sprintf("%s/04_sim_temp_model_scen/%s",
                                      tdir, var)) %>%
    collect() %>%
    as.data.table()

  # rename temperature variable
  setnames(tt_sim_data, "temp", var)

  # save
  dir.create(adir, recursive = T, showWarnings = FALSE)
  write_parquet(tt_sim_data, sprintf("%s/04_sim_data_%s.gz.parquet", adir, var))

}


#------------------------
# CLEAN
#------------------------

cat(as.character(Sys.time()), "04_agg_to_urau_sim.R run succesfully\n",
    file = sprintf("%s/04_trace.txt", tdir), append = TRUE) |> try()
cat(as.character(Sys.time()), "Cleaning temporal files and directories\n
    =======================================\n
    =======================================\n",
    file = sprintf("%s/04_trace.txt", tdir), append = TRUE) |> try()


# delete all created temporary files
if (deleteTempFiles) unlink(sprintf("%s/%s", tdir, c("04_sim_temp_yr", "04_sim_temp_yr_mn")), recursive = T)
tmpFiles(remove = TRUE)
gc()
