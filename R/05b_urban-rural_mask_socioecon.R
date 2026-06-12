################################################################################
#
# Heat-related health impacts across national mitigation and urban adaptation scenarios in European cities
#
# R Code Step 5: Create urban-rural mask by socioeconomic item
# R Code Step 5b: Create a urban-rural(-excluded) mask based on SSP2 projections
#
# Clàudia Rodés-Bachs
#
################################################################################

cat(sprintf("%s Running 05b_urban-rura_mask_socioecon\n
            ==============================\n
            ==============================\n
            ==============================\n",
            as.character(Sys.time())),
    file = sprintf("%s/00_trace.txt", tdir), append = TRUE) |> try()

cat(sprintf("%s Running 05b_urban-rura_mask_socioecon\n
            ==============================\n
            ==============================\n",
            as.character(Sys.time())),
    file = sprintf("%s/05_trace.txt", tdir), append = TRUE) |> try()


#------------------------
# Process data
#------------------------

# select necessary columns "> colnames(head(raw_pop_eur))"
target_cols <- c("T", "M", "F", "Y_1564", "Y_GE65")

# create a template raster based on the vector's extent
r_template <- rast(raw_pop_eur, res = 1000)

# transform to rasters
for (tc in target_cols) {
  print(tc)
  pop_rast <- rasterize(raw_pop_eur, r_template, field = tc)
  # add UK data
  if (tc == 'T') {
    uk_projected <- project(raw_pop_uk, crs(pop_rast))
    pop_rast <- merge(pop_rast, uk_projected)
  }
  writeRaster(pop_rast, filename = paste0('data/tmp/05_pop_rasters/',tc,'.tif'),
              overwrite = T)
}

## -- compute
# compute percentage by settl type for each socioeconomic item
for (tc in target_cols) {
  cat(sprintf("%s Computing socioeconomic percentage by settl type: %s\n
              ======================================================\n
              ======================================================\n",
              as.character(Sys.time()), tc),
      file = sprintf("%s/05_trace.txt", tdir), append = TRUE) |> try()


  # -- load data
  # load socioeconomic raster for the desired item
  pop_rast <- rast(paste0('data/tmp/05_pop_rasters/',tc,'.tif'))

  # initialize dataset to save the data
  dat_settl_full <- data.table()

  # -- loop
  for (uc in urau_codes) {
    cat(sprintf("%s For item %s - uc %s\n",
                as.character(Sys.time()), tc, uc),
        file = sprintf("%s/05_trace.txt", tdir), append = TRUE) |> try()



    # get uc geometry
    uc_geom <- urau_map %>%
      filter(URAU_CODE == uc) %>%
      st_as_sf()

    if (nrow(uc_geom) == 0)  {
      cat(sprintf("%s ERROR: Empty geometry: %s\n",
                  as.character(Sys.time()), uc),
          file = sprintf("%s/06_trace.txt", tdir), append = TRUE) |> try()
      next
    }

    uc_vect <- vect(uc_geom)

    # grid cell by settl type
    uc_projected <- project(uc_vect, crs(urau_land_cover_binary))
    uc_raster_settl <- crop(urau_land_cover_binary, uc_projected, mask = TRUE)

    # grid cell by socioecon data
    uc_projected <- project(uc_vect, crs(pop_rast))
    uc_raster_masked <- crop(pop_rast, uc_projected, mask = TRUE)

    # reproject population (use "sum" to preserve total population when resampling)
    pop_laea <- project(uc_raster_masked, uc_raster_settl,
                        method = "sum")

    # extract population by cell type
    cell_types <- values(uc_raster_settl)
    pop_values <- values(pop_laea)

    # sum population by cell type
    pop_by_type <- tapply(pop_values, cell_types, sum, na.rm = TRUE)

    # cell types: 0 = URBAN, 1 = RURAL, 2 = EXCLUDED (or NA)
    dat_settl <- data.table(
      type       = c("urban", "rural", "excluded"),
      cell_value = c(0, 1, 2),
      population = c(pop_by_type["0"], pop_by_type["1"], pop_by_type["2"]),
      percentage_all = c(pop_by_type["0"]/(pop_by_type["0"]+pop_by_type["1"]+pop_by_type["2"]),
                         pop_by_type["1"]/(pop_by_type["0"]+pop_by_type["1"]+pop_by_type["2"]),
                         pop_by_type["2"]/(pop_by_type["0"]+pop_by_type["1"]+pop_by_type["2"])),
      percentage_urb_rur = c(pop_by_type["0"]/(pop_by_type["0"]+pop_by_type["1"]),
                     pop_by_type["1"]/(pop_by_type["0"]+pop_by_type["1"]),
                     0)
    )

    # rbind
    dat_settl_full <- rbind(
      dat_settl_full,
      dat_settl[, URAU_CODE := uc]
    )

  }

  # -- save
  idir <- sprintf("%s/05_pop_percentage/%s",tdir,tc)
  dir.create(idir, recursive = T, showWarnings = FALSE)
  write_parquet(dat_settl_full, sprintf("%s/res.parquet", idir))

  rm(dat_settl_full, pop_rast)
  gc()

}



## -- aggregate
cat(as.character(Sys.time()), "Aggregate socioeconomic percentages by settl type\n
    =======================================\n
    =======================================\n",
    file = sprintf("%s/05_trace.txt", tdir), append = TRUE) |> try()


dat_settl <- open_dataset(sprintf("%s/05_pop_percentage", tdir),
                          partitioning = c("item")) %>%
  collect() %>%
  as.data.table()

write_parquet(dat_settl, sprintf("%s/05_urau_city_socioecon_percentage.gz.parquet", adir))


#------------------------
# CLEAN
#------------------------

cat(as.character(Sys.time()), "05b_urban-rural_mask_socioecon.R run succesfully\n",
    file = sprintf("%s/05_trace.txt", tdir), append = TRUE) |> try()
cat(as.character(Sys.time()), "Cleaning temporal files and directories\n
    =======================================\n
    =======================================\n",
    file = sprintf("%s/05_trace.txt", tdir), append = TRUE) |> try()


# delete all created temporary files
tmpFiles(remove = TRUE)
gc()

