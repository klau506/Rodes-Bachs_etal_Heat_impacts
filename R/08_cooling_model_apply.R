################################################################################
#
# Heat-related health impacts across national mitigation and urban adaptation scenarios in European cities
#
# R Code Step 8: Apply tree cooling empirical model at the URAU CODE level
#
# Clàudia Rodés-Bachs
#
################################################################################

cat(sprintf("%s Running 08_cooling_model_apply\n
            ==============================\n
            ==============================\n
            ==============================\n",
            as.character(Sys.time())),
    file = sprintf("%s/00_trace.txt", tdir), append = TRUE) |> try()

# initialize trace
dir.create(tdir, recursive = T, showWarnings = FALSE)
writeLines(c(""), sprintf("%s/08_trace.txt", tdir))
cat(sprintf("================================\n%s\n", as.character(Sys.time())),
    file = sprintf("%s/08_trace.txt", tdir), append = FALSE) |> try()

# -------------------------------------------------------------------------------
# 1. Preprocess data
# -------------------------------------------------------------------------------

# load simulated temperatures
raw_sim_temp <- open_dataset(sprintf("%s/07_sim_calibStep2.gz.parquet", adir))

# load Global pooled mixed effects model
model_mixed <- readRDS(sprintf("%s/06_lm_cooling_citygrid/model_GLOBAL.rds", adir))

# load LST to Ambient temperautre model
model_lst_to_tg <- readRDS(sprintf("%s/06_lst_to_tg/model_lst_to_tg.rds", adir))

# -------------------------------------------------------------------------------
# 2. Cooling model application function
# -------------------------------------------------------------------------------
# urau_codes2 = urau_codes
# urau_codes2 = (urau_codes[500:730])
urau_codes2 = urau_codes[(i*25+1):(min((i+1)*25,length(urau_codes)))]
for (uc in urau_codes2) {
  for (sc in scenarios) {
    print(uc)

    idir <- sprintf("%s/08_final_datasets/%s/%s/res.parquet",
                    tdir, uc, sc)
    if (file.exists(idir)) {
      next
    }

    # -------------------------------------------------------------------------------
    # 1. Load data
    # -------------------------------------------------------------------------------

    # -- load model
    if (!file.exists(sprintf("%s/06_lm_cooling_citygrid/model_%s.rds", adir, uc))) {
      cat(sprintf("%s ERROR cooling model does not exist: %s\n",
                  as.character(Sys.time()), uc),
          file = sprintf("%s/08_trace.txt", tdir), append = TRUE) |> try()
      next
    }
    model_mnt <- readRDS(sprintf("%s/06_lm_cooling_citygrid/model_%s.rds", adir, uc))

    # -- load simulated temperatures
    tmeandf <- raw_sim_temp %>%
      filter(URAU_CODE == uc, scen == sc) %>%
      select(!c(URAU_CODE)) %>%
      collect() %>%
      as.data.table() %>%
      rename(tas_rural = full,
             tas_urban = full_urban,
             gcm = model) %>%
      select(-demo, -demo_urban, -tsim, -variable)

    setkey(tmeandf, year, month, day)
    tmeandf <- tmeandf[year < max(projrange)]

    # reshape dataset
    tmeandf_long <- melt(tmeandf,
                         id.vars = c("date", "day", "month", "year", "gcm", "scen", "calperiod", "year5"),
                         measure.vars = c("tas_rural", "tas_urban"),
                         variable.name = "settl",
                         value.name = "tas")
    tmeandf_long[, settl := sub("tas_", "", settl)]


    # -- preprocess tree coverage data
    # subset to uc and reproject
    uc_geom <- urau_map %>%
      filter(URAU_CODE == uc) %>%
      st_as_sf()
    uc_vect <- vect(uc_geom)
    uc_laea <- st_transform(uc_geom, crs(treecover))
    uc_raster_tree <- crop(treecover, uc_laea)
    uc_numeric <- as.numeric(uc_raster_tree)
    uc_raster_masked_tree <- mask(uc_numeric, uc_laea)

    # extract the mean tree cover by urban - 0 and rural - 1
    land_cover_city <- crop(urau_land_cover_binary_clean, uc_raster_masked_tree, snap = "near")
    land_cover_city <- resample(land_cover_city, uc_raster_masked_tree, method = "near")
    land_cover_city <- mask(land_cover_city, uc_raster_masked_tree)
    results <- zonal(uc_raster_masked_tree, land_cover_city, fun = "mean", na.rm = TRUE) %>%
      as.data.table()


    # -------------------------------------------------------------------------------
    # 2. Preprocess data
    # -------------------------------------------------------------------------------

    # -- join simulated temperatures and tree cover data
    tmeandf_long[, tree_cover := as.numeric(NA)]
    tmeandf_long[settl == "urban", tree_cover := results[LABEL3 == 0, 2]]
    tmeandf_long[settl == "rural", tree_cover := results[LABEL3 == 1, 2]]


    # -------------------------------------------------------------------------------
    # 3. Apply cooling model
    # -------------------------------------------------------------------------------

    # model1: AirTemp_adapt = AirTemp_raw + DeltaAir_cooling;
    # where DeltaAir_cooling = DeltaLST_cooling through model2;
    # where DeltaLST_cooling = TreeCoverCoef * DeltaTreeCover
    # model2: AirTemp = a + bLST + cLATITUDE;

    # tree_cover coefficient
    coef_tree <- coef(model_mnt)["tree_cover"]
    # check tree_cover coef < 0. Otherwise, use pooled mixed effects Global model
    if (coef_tree > 0) {
      coef_tree <- fixef(model_mixed)["tree_cover"]
    }

    # lst to tas coefficient
    coef_lst <- coef(model_lst_to_tg)["lst_mean"]


    tmeandf_final <- tmeandf_long[, adapt := 'perc0']
    tmeandf_final <- tmeandf_final[, lat := cities %>% filter(URAU_CODE == uc) %>% pull(lat)]

    # sensitivity on the adapt threshold value
    for (adapt_th in c(25,30,35)) {

      tmeandf_adapt <- copy(tmeandf_long)
      tmeandf_adapt <- tmeandf_adapt[settl == 'urban',]
      tmeandf_adapt <- tmeandf_adapt[, adapt := paste0('perc',adapt_th)]

      # enforce min 30% of tree cover in urban areas
      tmeandf_adapt[, tree_cover_adapt := pmax(tree_cover, adapt_th)]

      # compute DeltaTreeCover
      tmeandf_adapt[, delta_tree := tree_cover_adapt - tree_cover]

      # apply cooling model
      lat_uc = cities %>% filter(URAU_CODE == uc) %>% pull(lat)
      tmeandf_adapt[, LST_cooling   := coef_tree * delta_tree]
      tmeandf_adapt[, air_cooling   := coef_lst * LST_cooling]
      tmeandf_adapt[, tg_adapt     := tas + air_cooling]


      # -------------------------------------------------------------------------------
      # 4. Unify datasets
      # -------------------------------------------------------------------------------

      cols <- names(tmeandf_long)

      tmeandf_final <- rbind(
        tmeandf_final,
        tmeandf_adapt[, .(
          # select all columns from 'cols' but swap 'tas'
          # for 'LST_adapt' and 'tree_cover' for 'tree_cover_adapt'
          .SD[, !c("tas", "tree_cover"), with = FALSE],
          tas = tg_adapt,
          tree_cover = tree_cover_adapt
        )][, ..cols])

    }

    ## -- save
    idir <- sprintf("%s/08_final_datasets/%s/%s",
                    tdir, uc, sc)
    dir.create(idir, recursive = T, showWarnings = FALSE)
    write_parquet(tmeandf_final, sprintf("%s/res.parquet", idir))


    ## -- clean
    rm(tmeandf_final, tmeandf_adapt, tmeandf_long)
    gc()
  }
}


# AGGREGATE
cat(sprintf("%s Aggregating data\n
    =======================================\n
    =======================================\n",
            as.character(Sys.time()), var),
    file = sprintf("%s/08_trace.txt", tdir), append = TRUE) |> try()

# read and stack
data_08 <- open_dataset(sprintf("%s/08_final_datasets", tdir),
                        partitioning = c("URAU_CODE","scen"))
dataa_08 <- data.table()
for(ml in models) {
  for(sc in scenarios) {
    print(paste(ml, sc))
    # annual average by country
    data <- data_08 %>%
      filter(gcm == ml, scen == sc) %>%
      collect() %>%
      as.data.table()
    # annual average by city
    tmeandf_final <- tmeandf_final[, .(tas = mean(tas)),
                 by = .(year, gcm, scen, calperiod, year5, settl, adapt, tree_cover, URAU_CODE)]
    # add to dataset
    dataa_08 <- rbindlist(list(dataa_08, data))

    rm(data); gc()
  }
}
setDT(dataa_08)
write_parquet(dataa_08, sprintf("%s/08_results_temperature.gz.parquet", adir))


#------------------------
# CLEAN
#------------------------

cat(as.character(Sys.time()), "08_cooling_model_apply.R run succesfully\n",
    file = sprintf("%s/08_trace.txt", tdir), append = TRUE) |> try()
cat(as.character(Sys.time()), "Cleaning temporal files and directories\n
    =======================================\n
    =======================================\n",
    file = sprintf("%s/08_trace.txt", tdir), append = TRUE) |> try()


# delete all created temporary files
if (deleteTempFiles) unlink(sprintf("%s/%s", tdir, c("08_final_datasets")), recursive = T)
tmpFiles(remove = TRUE)
gc()

