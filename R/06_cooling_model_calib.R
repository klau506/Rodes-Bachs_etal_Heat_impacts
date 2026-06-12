################################################################################
#
# Heat-related health impacts across national mitigation and urban adaptation scenarios in European cities
#
# R Code Step 6: Create tree cooling empirical model
#
# Clàudia Rodés-Bachs
#
################################################################################

cat(sprintf("%s Running 06_tree_empirical_model_citygrid_calib\n
            ==============================\n
            ==============================\n
            ==============================\n",
            as.character(Sys.time())),
    file = sprintf("%s/00_trace.txt", tdir), append = TRUE) |> try()

# initialize trace
dir.create(tdir, recursive = T, showWarnings = FALSE)
writeLines(c(""), sprintf("%s/06_trace.txt", tdir))
cat(sprintf("================================\n%s\n", as.character(Sys.time())),
    file = sprintf("%s/06_trace.txt", tdir), append = T) |> try()


# Tree coverage:      raster_masked_tree             100m2            Tree Coverage Density
# LST:                raster_masked_evapotrans       1km2             Land Surface Temperature
# Ec:                 raster_masked_Ec               1km2             Canopy transpiration
# Ei:                 raster_masked_Ei               1km2             Interception evaporation
# Et:                 raster_masked_Et               1km2             Total land evapotranspiration (Ec + Ei)

## NOTE: currently one city model approach for gridded data
## OTHER OPTIONS TO BE EXPLORED:
#   - mixed effects model
#   - model by URAU CODE (so gridded data is aggregated)

# -------------------------------------------------------------------------------
# 1. Evapotranspiration data
# -------------------------------------------------------------------------------

# list files
et_files <- list.files("data/PML_V2//",
                       pattern = "PMLV2_latest2023.*\\.tif$",
                       full.names = TRUE)

# extract dates from filenames
et_dates <- as.Date(stringr::str_extract(et_files, "\\d{4}-\\d{2}-\\d{2}"))

# stack
evapo_stack <- rast(et_files)
terra::time(evapo_stack) <- rep(et_dates, each = 5)

# extract only ET components across all dates
# --> ET = Ec + Ei, total land evapotranspiration
# GPP - Gross Primary Production — carbon flux,
# Ec - Canopy transpiration — water released through plant leaves
# Es - Soil evaporation — water evaporated from soil
# Ei - Interception evaporation — water evaporated from wet canopy
# ET_water - Total ET over water bodies - only for water surfaces
# qc - Quality control flag — just a mask
Ec <- evapo_stack[[names(evapo_stack) == "Ec"]]
Ei <- evapo_stack[[names(evapo_stack) == "Ei"]]

# -------------------------------------------------------------------------------
# 2. Tree cover data
# -------------------------------------------------------------------------------

# load data -- loaded in 02.R

# -------------------------------------------------------------------------------
# 3. LST data
# -------------------------------------------------------------------------------

# list files
lst_files <- list.files("data/LST_Modis//",
                        pattern = "LST_8day2023.*\\.tif$",
                        full.names = TRUE)

# extract dates from filenames
lst_dates <- str_extract(lst_files, "\\d{4}_\\d{2}_\\d{2}")
lst_dates <- as.Date(lst_dates, format = "%Y_%m_%d")

# stack
lst_stack <- rast(lst_files)
terra::time(lst_stack) <- lst_dates
names(lst_stack) <- lst_dates


# -------------------------------------------------------------------------------
# 4. Constants and parameters
# -------------------------------------------------------------------------------

# target projection
target_crs <- crs(lst_stack)


cat(sprintf("%s All data loaded\n
              =======================================\n
              =======================================\n",
            as.character(Sys.time())),
    file = sprintf("%s/06_trace.txt", tdir), append = TRUE) |> try()

# -------------------------------------------------------------------------------
# 5. Cooling model function
# -------------------------------------------------------------------------------

# ## -- arrange the necessary data
# df_all <- data.table()
# for(uc in urau_codes) {
#   print(uc)
#
#   dest_file <- sprintf("%s/model_%s.rds", sprintf("%s/06_lm_cooling_citygrid", adir), uc)
#   if (file.exists(dest_file)) {
#     cat(sprintf("%s SKIPPING: Cooling model already created: %s\n",
#                 as.character(Sys.time()), uc),
#         file = sprintf("%s/06_trace.txt", tdir), append = TRUE) |> try()
#     next
#   }
#
#   cat(sprintf("%s Creating cooling model: %s\n",
#               as.character(Sys.time()), uc),
#       file = sprintf("%s/06_trace.txt", tdir), append = TRUE) |> try()
#
#
#   # -------------------------------------------------------------------------------
#   # 1. URAU CODE geometry
#   # -------------------------------------------------------------------------------
#   uc_geom <- urau_map %>%
#     filter(URAU_CODE == uc) %>%
#     st_as_sf()
#
#   if (nrow(uc_geom) == 0)  {
#     cat(sprintf("%s ERROR: Empty geometry: %s\n",
#                 as.character(Sys.time()), uc),
#         file = sprintf("%s/06_trace.txt", tdir), append = TRUE) |> try()
#     next
#   }
#
#   uc_vect <- vect(uc_geom)
#   uc_projected <- project(uc_vect, target_crs)
#
#
#   # -------------------------------------------------------------------------------
#   # 2. Subset data
#   # -------------------------------------------------------------------------------
#
#   ### -- A: Evapotranspiration data
#   uc_raster_masked_Ec <- crop(Ec, uc_projected, mask = TRUE)
#   uc_raster_masked_Ei <- crop(Ei, uc_projected, mask = TRUE)
#
#   uc_raster_masked_Et <- uc_raster_masked_Ec + uc_raster_masked_Ei
#
#   ### -- B: Tree cover data
#   uc_laea <- st_transform(uc_geom, crs(treecover))
#   uc_raster_tree <- crop(treecover, uc_laea)
#   uc_numeric <- as.numeric(uc_raster_tree)
#   uc_raster_masked_tree <- mask(uc_numeric, uc_laea)
#
#   ### -- C: LST data
#   uc_raster_masked_lst <- crop(lst_stack, uc_projected, mask = TRUE)
#
#
#   # -------------------------------------------------------------------------------
#   # 3. Reproject to target grid
#   # -------------------------------------------------------------------------------
#   # define target grid (resolution, crs, projection...)
#   target_grid <- uc_raster_masked_lst
#
#   # LST: simply rename the raster
#   lst_1km <- uc_raster_masked_lst
#
#   # Tree cover: reproject from LAEA to WGS84, resample to 1km
#   # aggregate 100m -> 1km (mean tree cover fraction)
#   tree_1km <- project(uc_raster_masked_tree, target_grid, method = "average")
#
#   # ET: already in WGS84, just resample to match exactly
#   et_et_1km <- resample(uc_raster_masked_Et, target_grid, method = "bilinear")
#   et_ec_1km <- resample(uc_raster_masked_Ec, target_grid, method = "bilinear")
#   et_ei_1km <- resample(uc_raster_masked_Ei, target_grid, method = "bilinear")
#
#   # verify all match
#   compareGeom(target_grid, tree_1km, et_et_1km, et_ec_1km, et_ei_1km)
#
#
#   # -------------------------------------------------------------------------------
#   # 4. Build space-time dataframe
#   # -------------------------------------------------------------------------------
#
#   # get common dates
#   dates_lst <- time(lst_1km)
#   dates_et  <- time(et_et_1km)
#   common_dates <- intersect(dates_lst, dates_et)
#   non_common_dates <- c(setdiff(dates_lst, dates_et), setdiff(dates_et, dates_lst))
#
#   df <- as.data.frame(c(target_grid, tree_1km, et_et_1km, et_ec_1km, et_ei_1km), xy = TRUE, na.rm = FALSE)
#   names(df) <- c("x", "y", "LST", "tree_cover", "ET", "EC", "EI")
#
#
#   df_list <- lapply(common_dates, function(d) {
#
#     # extract single time step for LST and ET
#     lst_t    <- lst_1km[[which(dates_lst == d)]]
#     et_et_t  <- et_et_1km[[which(dates_et  == d)]]
#     et_ec_t  <- et_ec_1km[[which(dates_et  == d)]]
#     et_ei_t  <- et_ei_1km[[which(dates_et  == d)]]
#
#     # stack all items
#     stack_t <- c(lst_t, tree_1km, et_et_t, et_ec_t, et_ei_t)
#     names(stack_t) <- c("LST", "tree_cover", "ET", "EC", "EI")
#
#     # set to dataframe
#     df_t <- as.data.frame(stack_t, xy = TRUE, na.rm = FALSE)
#     df_t$date   <- as.Date(d)
#     df_t$month  <- month(df_t$date)
#     df_t$doy    <- yday(df_t$date)
#     df_t$season <- case_when(
#       month(df_t$date) %in% c(12, 1, 2)  ~ "DJF",
#       month(df_t$date) %in% c(3, 4, 5)   ~ "MAM",
#       month(df_t$date) %in% c(6, 7, 8)   ~ "JJA",
#       month(df_t$date) %in% c(9, 10, 11) ~ "SON"
#     )
#
#     df_t
#   })
#
#   df <- rbindlist(df_list)
#   df <- na.omit(df)
#
#   available_complt_obs <- nrow(df) # available observations for modelling (cells x dates)
#
#   df_all <- rbind(df_all, df)
# }
# save(df_all, file = 'data/artifacts/06_df.RData')
load('data/artifacts/06_df.RData')


## -- define the model for each URAU_CODE
for (uc in urau_codes) {
  # -------------------------------------------------------------------------------
  # 5. Fit the model
  # -------------------------------------------------------------------------------

  ### -- fit
  model_mnt <- lm(LST ~ tree_cover + ET + factor(month), data = df)


  ### -- save
  idir <- sprintf("%s/06_lm_cooling_citygrid", adir)
  dir.create(idir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(model_mnt, sprintf("%s/model_%s.rds", idir, uc))



  # -------------------------------------------------------------------------------
  # 6. Model diagnostics
  # -------------------------------------------------------------------------------


  ### -- summary
  df$LST_pred <- predict(model_mnt, newdata = df)
  df$resid    <- df$LST - df$LST_pred

  summary_R2    = summary(model_mnt)$r.squared
  summary_RMSE  = sqrt(mean(df$resid^2))



  ### -- spatial cross-validation
  train_idx <- sample(nrow(df), size = 0.8 * nrow(df))
  train <- df[train_idx, ]
  test  <- df[-train_idx, ]

  model_cv <- lm(LST ~ tree_cover + ET + factor(month), data = train)
  pred_cv  <- predict(model_cv, newdata = test)

  spatialcv_R2    = if(sum(!is.na(pred_cv)) > 1) cor(test$LST, pred_cv, use = "complete.obs")^2 else NA_real_
  spatialcv_RMSE  = sqrt(mean((test$LST - pred_cv)^2))



  ### -- temporal cross-validation (leave-one-date-out)
  tempcv_results <- rbindlist(lapply(common_dates, function(d) {

    df$month <- factor(df$month, levels = 1:12)

    train <- df[date != as.Date(d)]
    test  <- df[date == as.Date(d)]

    m    <- lm(LST ~ tree_cover + ET + factor(month), data = train)
    pred <- predict(m, newdata = test)

    data.table(
      tempcv_date = as.Date(d),
      tempcv_R2   = if(sum(!is.na(pred)) > 1) cor(test$LST, pred, use = "complete.obs")^2 else NA_real_,
      tempcv_RMSE = sqrt(mean((test$LST - pred)^2, na.rm = TRUE)),
      tempcv_MAE  = mean(abs(test$LST - pred),     na.rm = TRUE)
    )
  }))



  ### -- save results
  diagnostic_results <- tempcv_results[, `:=`(
    run_time             = Sys.time(),
    model_name           = 'model_mnt',
    model_formula        = 'lm: LST ~ tree_cover + ET + factor(month)',
    urau_code            = uc,
    non_common_dates     = length(non_common_dates),
    available_complt_obs = available_complt_obs,
    summary_R2           = summary_R2,
    summary_RMSE         = summary_RMSE,
    spatialcv_R2         = spatialcv_R2,
    spatialcv_RMSE       = spatialcv_RMSE
  )]
  setcolorder(diagnostic_results,
              c(setdiff(names(diagnostic_results), grep("^tempcv_", names(diagnostic_results), value = TRUE)),
                        grep("^tempcv_", names(diagnostic_results), value = TRUE)))

  fwrite(diagnostic_results, diagnostics_citygrid_output, append = TRUE) |> try()



  rm(model_mnt)
  gc()

}






## -- check sd and correlation values
coef_summary <- rbindlist(lapply(urau_codes, function(uc) {
  d <- df_all[URAU_CODE == uc]
  m <- lm(LST ~ tree_cover + ET + factor(month), data = d)
  data.table(
    URAU_CODE  = uc,
    b_tree     = coef(m)["tree_cover"],
    b_ET       = coef(m)["ET"],
    n_obs      = nrow(d),
    sd_tree    = sd(d$tree_cover, na.rm = TRUE),
    cor_tree_ET = cor(d$tree_cover, d$ET)
  )
}))
save(coef_summary, file = 'data/artifacts/06_coef_summary.RData')
a = coef_summary[b_tree > 0]
summary(a)
# URAU_CODE             b_tree               b_ET               n_obs          sd_tree        cor_tree_ET
# Length:154         Min.   :6.820e-06   Min.   :-0.019515   Min.   :  642   Min.   : 1.442   Min.   :-0.37858
# Class :character   1st Qu.:1.088e-02   1st Qu.:-0.005991   1st Qu.: 4438   1st Qu.: 6.158   1st Qu.:-0.15479
# Mode  :character   Median :2.648e-02   Median : 0.001046   Median : 8104   Median : 7.909   Median :-0.07586
# Mean   :3.542e-02   Mean   : 0.003138   Mean   :11100   Mean   : 8.385   Mean   :-0.02263
# 3rd Qu.:4.894e-02   3rd Qu.: 0.009495   3rd Qu.:14711   3rd Qu.:10.268   3rd Qu.: 0.06091
# Max.   :1.718e-01   Max.   : 0.055325   Max.   :60473   Max.   :31.389   Max.   : 0.59705

b = coef_summary[b_tree < 0]
summary(b)
# URAU_CODE             b_tree                b_ET                n_obs           sd_tree         cor_tree_ET
# Length:700         Min.   :-0.9121249   Min.   :-0.0273736   Min.   :   519   Min.   : 0.5092   Min.   :-0.2746
# Class :character   1st Qu.:-0.0367331   1st Qu.:-0.0061396   1st Qu.:  4226   1st Qu.:11.9885   1st Qu.: 0.0933
# Mode  :character   Median :-0.0269285   Median :-0.0022257   Median :  6849   Median :18.9801   Median : 0.2088
# Mean   :-0.0322687   Mean   :-0.0006144   Mean   : 13482   Mean   :18.9883   Mean   : 0.2149
# 3rd Qu.:-0.0176103   3rd Qu.: 0.0020264   3rd Qu.: 12599   3rd Qu.:25.8581   3rd Qu.: 0.3231
# Max.   :-0.0000129   Max.   : 0.0627065   Max.   :309970   Max.   :40.1431   Max.   : 0.7089


## -- define the pooled mixed effects Global model
model_mixed <- lmer(
  LST ~ tree_cover + ET + factor(month) + (1 | URAU_CODE),
  data = df_all, REML = TRUE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

idir <- sprintf("%s/06_lm_cooling_citygrid", adir)
dir.create(idir, recursive = TRUE, showWarnings = FALSE)
saveRDS(model_mixed, sprintf("%s/model_GLOBAL.rds", idir))

# Marginal and Conditional R2
r2(model_mnt)
# Conditional R2: 0.861
# Marginal R2: 0.717


#------------------------
# CLEAN
#------------------------

cat(as.character(Sys.time()), "06_tree_empirical_model_citygrid_calib.R run succesfully\n",
    file = sprintf("%s/06_trace.txt", tdir), append = TRUE) |> try()
cat(as.character(Sys.time()), "Cleaning temporal files and directories\n
    =======================================\n
    =======================================\n",
    file = sprintf("%s/06_trace.txt", tdir), append = TRUE) |> try()


# delete all created temporary files
tmpFiles(remove = TRUE)
gc()

