################################################################################
#
# Heat-related health impacts across national mitigation and urban adaptation scenarios in European cities
#
# R Code Step 6: Create LST to ambient temperature "translation" empirical model
#
# Clàudia Rodés-Bachs
#
################################################################################

cat(sprintf("%s Running 06_lst_to_ambient\n
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

# Ambient air temperature=β0e2 + β1e2(land surface  temperature) + β2e2(latitude)
# -------------------------------------------------------------------------------
# 1. LOAD DATA - LST data
# -------------------------------------------------------------------------------

# list files
lst_files <- list.files("data/LST/",
                        full.names = TRUE,
                        recursive = T)

# extract dates from filenames
lst_dates <- str_extract(lst_files, "\\d{4}_\\d{2}_\\d{2}")
lst_dates <- as.Date(lst_dates, format = "%Y_%m_%d")

# stack
lst_stack <- rast(lst_files)
terra::time(lst_stack) <- lst_dates
names(lst_stack) <- lst_dates


# -------------------------------------------------------------------------------
# 2. LOAD DATA - Ambient air temperature data
# -------------------------------------------------------------------------------

# NOTE: for the moment, we consider average 8-day mean temperature. If the model performs well, keep going. Otherwise consider other metrics.
# NOTE: consider different timeframes (so that obs and lst data matches)
obs_temp <- tg_obs_city[date %between% range(lst_dates)]

# create modis_date column and average data among it (same time structure than LST data)
obs_dates <- lst_dates
obs_dates <- as.Date(obs_dates)
modis_windows <- data.table(modis_start = obs_dates)
modis_windows[, modis_window_name := modis_start]

setkey(obs_temp, date)
setkey(modis_windows, modis_start)
obs_temp[, modis_date := modis_windows[obs_temp, modis_window_name, roll = TRUE]]

obs_temp <- obs_temp[, .(tg_avg = mean(tg, na.rm = TRUE)),
                     by = .(URAU_CODE, scen, modis_date)]

# -------------------------------------------------------------------------------
# 3. Empirical model function
# -------------------------------------------------------------------------------

## -- arrange the necessary data
df_all <- data.table()
for(uc in urau_codes) {
  print(uc)

  # -------------------------------------------------------------------------------
  # 1. URAU CODE geometry
  # -------------------------------------------------------------------------------
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

  # -------------------------------------------------------------------------------
  # 2. Subset data
  # -------------------------------------------------------------------------------

  ### -- A: LST data
  uc_masked_lst <- crop(lst_stack, uc_vect, mask = TRUE)
  uc_masked_lst_means <- global(uc_masked_lst, fun = "mean", na.rm = TRUE) %>%
    as.data.frame() %>%
    rownames_to_column(var = "date") %>%
    mutate(date = as.Date(date)) %>%
    rename(lst_mean = mean,
           date_window = date)

  ### -- B: OBS data
  uc_masked_obs <- obs_temp %>%
    filter(URAU_CODE == uc) %>%
    rename(date_window = modis_date)

  ### -- C: city data
  uc_city <- cities %>%
    filter(URAU_CODE == uc)


  # -------------------------------------------------------------------------------
  # 3. Build space-time dataframe
  # -------------------------------------------------------------------------------

  df <- cities %>%
    merge(uc_masked_obs, by = 'URAU_CODE') %>%
    merge(uc_masked_lst_means, by = 'date_window')

  df_all <- rbind(df_all, df)
}
save(df_all, file = 'data/artifacts/06_df_lst_to_tg.RData')
load('data/artifacts/06_df_lst_to_tg.RData')


# -------------------------------------------------------------------------------
# 5. Fit the model
# -------------------------------------------------------------------------------

df = df_all[rowSums(is.na(df_all)) == 0,]

### -- fit
model_lst_to_tg <- lm(tg_avg ~ lst_mean + lat, data = df) # + factor(URAU_CODE)
par(mfrow = c(2, 2))
plot(model_lst_to_tg)



# -------------------------------------------------------------------------------
# 6. Model diagnostics
# -------------------------------------------------------------------------------


### -- summary
df$tg_new <- predict(model_lst_to_tg, newdata = df)
df$resid    <- df$tg_new - df$tg_avg

summary_R2    = summary(model_lst_to_tg)$r.squared
summary_RMSE  = sqrt(mean(df$resid^2))


### -- refine
# df <- df %>%
#   mutate(month = month(date_window))
#
# model_lst_to_tg <- lm(tg_avg ~ lst_mean + lat + factor(month), data = df)
# summary(model_lst_to_tg)
# plot(model_lst_to_tg)
# df$tg_new <- predict(model_lst_to_tg, newdata = df)
# df$resid    <- df$tg_new - df$tg_avg
#
# ggplot(df, aes(tg_new, tg_avg, color = month)) + geom_point()
#
# summary_R2    = summary(model_lst_to_tg)$r.squared
# summary_RMSE  = sqrt(mean(df$resid^2))


df$cooksd <- cooks.distance(model_lst_to_tg)
thresh <- 4 / nrow(df)
df_refined <- subset(df, cooksd < thresh)
model_lst_to_tg <- lm(tg_avg ~ lst_mean + lat, data = df_refined)

plot(model_lst_to_tg)
df$tg_new <- predict(model_lst_to_tg, newdata = df)
df$resid    <- df$tg_new - df$tg_avg

ggplot(df, aes(tg_new, tg_avg, colour = region)) + geom_point()

summary_R2    = summary(model_lst_to_tg)$r.squared
summary_RMSE  = sqrt(mean(df$resid^2))
# [1] 0.9004128
# [1] 2.638824


### -- save
idir <- sprintf("%s/06_lst_to_tg", adir)
dir.create(idir, recursive = TRUE, showWarnings = FALSE)
saveRDS(model_lst_to_tg, sprintf("%s/model_lst_to_tg.rds", idir))

write.csv(df, sprintf("%s/06_lst_to_ambient_diagnostics.csv", adir), row.names = F)


#------------------------
# CLEAN
#------------------------

cat(as.character(Sys.time()), "06_lst_to_tg.R run succesfully\n",
    file = sprintf("%s/06_trace.txt", tdir), append = TRUE) |> try()
cat(as.character(Sys.time()), "Cleaning temporal files and directories\n
    =======================================\n
    =======================================\n",
    file = sprintf("%s/06_trace.txt", tdir), append = TRUE) |> try()


# delete all created temporary files
tmpFiles(remove = TRUE)
gc()

