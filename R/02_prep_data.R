################################################################################
#
# Heat-related health impacts across national mitigation and urban adaptation scenarios in European cities
#
# R Code Step 2: Load data & set constants
#
# Clàudia Rodés-Bachs
#
################################################################################

cat(sprintf("%s Running 02_prep_data\n
            ==============================\n
            ==============================\n
            ==============================\n",
            as.character(Sys.time())),
    file = sprintf("%s/00_trace.txt", tdir), append = TRUE) |> try()


#------------------------
# Geographical Data
#------------------------
print('Geographical Data')

# load data
mapping_urau_codes <- xlsx::read.xlsx("data/mappings/CITIES.xlsx", sheetIndex = 1) %>%
  dplyr::select(URAU_CODE = OLD_CODE, NEW_URAU_CODE = NEW_CODE) %>%
  dplyr::distinct()
ctry <- sf::st_read("data/mappings/World_Countries.gpkg")
urau_map <- sf::st_read("data/mappings/URAU_RG_2020_4326_PM/URAU_RG_2020_4326.shp")
cityage <- read.csv("data/from_Masselot_NatMed_2025/city_results.csv")
cities <- unique(subset(cityage,
                        select = c("URAU_CODE", "LABEL", "CNTR_CODE",
                                   "cntr_name", "region", "lon", "lat", "pop")))

# get countries info
countries <- summarise(cities,
                       ncities = length(URAU_CODE), lat = mean(lat), region = unique(region),
                       .by = CNTR_CODE) %>%
  mutate(region = factor(region)) %>%
  arrange(region, desc(lat))

# arrange cities info
cities <- cities %>%
  mutate(CNTR_CODE = factor(CNTR_CODE, levels = countries$CNTR_CODE)) %>%
  arrange(region, CNTR_CODE, URAU_CODE)


# fix mismatch between URAU CODEs 2020 vs 2021
# - add new URAU CODEs (relative to 2021)
urau_map <- urau_map %>%
  dplyr::left_join(mapping_urau_codes,
                   by = 'URAU_CODE') %>%
  dplyr::mutate(NEW_URAU_CODE = ifelse(is.na(NEW_URAU_CODE), URAU_CODE, NEW_URAU_CODE))

# - in 2021 sf, subset to URAU CODEs required for `cities` but not available in urau_map
missing_codes <- setdiff(unique(cities$URAU_CODE), unique(urau_map$NEW_URAU_CODE))
# stop(length(missing_codes) != 0)

# check
urau_codes <- sort(unique(cities$URAU_CODE))
urau_codes <- cities %>%
  pull(URAU_CODE) %>% unique() %>% sort()


# rm cities with missing geometries
cities <- cities %>%
  dplyr::filter(URAU_CODE %in% unique(urau_map$URAU_CODE))


#------------------------
# MAPPINGS Data
#------------------------
print('MAPPINGS Data')
# European map data
euromap <- gisco_get_countries(year = "2024", cache_dir = "data")

# European lon-lat limits
crop_xmin <- -22
crop_xmax <- 36
crop_ymin <- 34
crop_ymax <- 72


# iso2 - iso3 - GCAM-Europe regions mapping
gcamreg_mapping <- cities %>%
  select(URAU_CODE, iso2 = CNTR_CODE) %>%
  mutate(iso3 = countrycode(iso2,
                            origin = "iso2c",
                            destination = "iso3c",
                            # handle manually Greece & Great Britain
                            custom_match = c("EL" = "GRC", "UK" = "GBR"))) %>%
  left_join(read.csv('data/mappings/GCAM32_to_EU.csv', comment = '#') %>%
              select(iso3 = iso, GCAMEU_region) %>%
              mutate(iso3 = toupper(iso3)),
            by = 'iso3')



#------------------------
# OBSERVED Data
#------------------------
print('OBSERVED Data')
# stack observed data by URAU CODE if required
if (rerun == 'TRUE'
    || !file.exists('data/artifacts/03_obs_data_tg.gz.parquet')) {
  source('R/03_agg_to_urau_obs.R')

}

# load observed data by URAU CODE in a safe way



tg_obs_city <- open_dataset("data/artifacts/03_obs_data_tg.gz.parquet") %>%
  as.data.table()


#------------------------
# SIMULATED Data
#------------------------
print('SIMULATED Data')
# generate GCAM-Europe (+ STITCHES + BASD) simulated data by URAU CODE if required
if (rerun == 'TRUE'
    || !file.exists('data/artifacts/04_sim_data_tg.gz.parquet')) {
  source('R/04_agg_to_urau_sim.R')

}

# load simulated data
tg_sim_city <- open_dataset("data/artifacts/04_sim_data_tg.gz.parquet") %>%
  collect()
gc()


#------------------------
# LAND (URBAN-RURAL) Data
#------------------------
print('LAND (URBAN-RURAL) Data')
# generate urban-rural mask if required
if (rerun == 'TRUE'
    || !file.exists('data/artifacts/05_urau_land_cover_binary.tif')) {
  # CORINE 100m land cover layer
  clc_2018 <- terra::rast('data/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif')
  corine_map <- read.csv('data/mappings/corine_mapping.csv', comment = '#') %>%
    rename(ID = Value) %>%
    as.data.table()

  source('R/05a_urban-rural_mask_settl.R')
}

# load urban-rural mask
urau_land_cover_binary <- terra::rast('data/artifacts/05_urau_land_cover_binary.tif')

# urau_land_cover_binary_clean <- subst(urau_land_cover_binary, 2, NA)
urau_land_cover_binary_clean <- terra::rast('data/artifacts/05_urau_land_cover_binary_clean.tif')



# compute percentage by settlement type & socioeconomic characteristic if required
if (rerun == 'TRUE'
    || !file.exists('data/artifacts/05_urau_city_socioecon_percentage.gz.parquet')) {
  # Eurostat 1km socioeconomic raster data
  raw_pop_eur <- vect('data/Pop_SSP2/Eurostat/Eurostat_Census-GRID_2021_V2.2/ESTAT_Census_2021_V2.gpkg')
  raw_pop_uk <- rast('data/Pop_SSP2/UK/data/uk_residential_population_2021.tif')

  source('R/05b_urban-rural_mask_socioecon.R')
}

# load urban-rural mask
urau_settl_socioecon <- open_dataset('data/artifacts/05_urau_city_socioecon_percentage.gz.parquet') %>%
  collect() %>%
  as.data.table()


#------------------------
# OTHER constants
#------------------------
print('OTHER constants')
# list of available gcm models
models <- c('CanESM5','MPI-ESM1-2-LR','MIROC6','UKESM1-0-LL') #unique(tg_sim_city$model)

# list of available scenarios
scenarios <- c("BASE", "CLIM") # c("BASE", "CLIM", "ADAPT")

# length of period of reporting (in years)
perlen <- 5

# age groups (first and last define boundaries to exclude too young or too old)
agebreaks <- c(20, 45, 65, 75, 85, Inf)
agelabs <- gsub("-Inf", "+",
                paste(agebreaks[-length(agebreaks)], agebreaks[-1] - 1, sep = "-"))

# specification of the ERFs (follows Masselot et al 2023 Lancet Plan. Health)
varfun <- "bs"
vardegree <- 2
varper <- c(10, 75, 90)
vardf <- vardegree + length(varper)

# coefficients of the ERFs (follows Masselot et al 2023 Lancet Plan. Health)
coefs <- read.csv("data/from_Masselot_NatMed_2025/coefs.csv") |>
  mutate(agegroup = factor(agegroup, levels = agelabs))

# denominator for excess death rates
byrate <- 10^5

# temperature percentiles
predper <- c(seq(0, 1, 0.1), 2:98, seq(99, 100, 0.1))

# variables accounted (tas)
variables <- c('tg')
variables_map <- c("mean" = "tg")

# define historial and future time ranges
histrange <- c(1990, 2019) #equiv to range(obs_city$year)
projrange <- seq(2020, 2065, 5)
## for Masselot checks
# histrange <- c(2000, 2014)
# projrange <- seq(2015, 2055, 5)


# number of simulations for the Monte-Carlo CI
nsim <- 500



#------------------------
# Demographic Data
#------------------------
print('Demographic Data')
# -- settlement type projections
projsettl <- urau_settl_socioecon[item == 'T' & type != "excluded",
                                  .(URAU_CODE, settl_pop = type, settl_pop_perc = percentage_urb_rur)]
# rm missing data (UK data)
projsettl <- na.omit(projsettl)

# add total (non-settl type split) population
projsettl <- projsettl[, rbind(.SD, list(settl_pop = "total", settl_pop_perc = sum(settl_pop_perc))), by = URAU_CODE]



# -- load Wittgenstein projections
# read Age-specific survival ratios
projdeath <- fread("data/from_Masselot_NatMed_2025/wittgenstein_assr.csv") |>
  filter(ssp == 2) %>%
  as.data.table()

# attach the first year to each period to later merge with population
projdeath[, year5 := as.numeric(substr(period, 1, 4))]

# read Population
projpop <- fread("data/from_Masselot_NatMed_2025/wittgenstein_pop.csv") |>
  rename(year5 = "year") %>%
  filter(ssp == 2) %>%
  as.data.table()

# select sex and age
projdeath <- projdeath[age != "Newborn"]
projpop <- projpop[age != "All" & sex != "Both",]

# merge population and deaths
proj <- merge(projpop, projdeath)

# select only relevant years
proj <- proj[year5 %between% histrange | year5 %between% range(projrange),]

# rescale population and compute deaths
proj[, ":="(pop = 1000 * pop, death = 1000 * pop * (1 - assr))]

# aggregate sex
proj <- proj[, .(pop = sum(pop), death = sum(death)),
             by = .(CNTR_CODE, age, ssp, year5)]



# -- aggregate by age group

# create age groups
proj[, agegroup := cut(as.numeric(sapply(strsplit(age, "[-+]"), "[", 1)),
                       agebreaks, include.lowest = T, right = F, labels = agelabs)]

# sum population and death by age group
proj <- proj[!is.na(agegroup), .(wittpop = sum(pop), wittdeath = sum(death)),
             by = .(CNTR_CODE, agegroup, ssp, year5)]

# rescale number of deaths as annual average
proj[, ":="(wittdeath = wittdeath / 5)]



# -- calibrate using Urban Audit

# average pop and deaths of Wittgenstein over historical period
histo_witt <- proj[year5 %between% histrange,
                   .(histowpop = mean(wittpop), histowdeaths = mean(wittdeath)),
                   by = .(CNTR_CODE, agegroup, ssp)]

# extract pop and deaths from EUcityTRM
citycal <- rename(cityage, uraupop = agepop, uraudeath = death) |>
  subset(select = c(URAU_CODE, CNTR_CODE, agegroup, uraupop, uraudeath))

# merge Eurostat and projection data
citycal <- merge(histo_witt, citycal, all = T, allow.cartesian = T)

# multiplicative factor for each city from national
citycal[, ":="(popfac = uraupop / histowpop, dfac = uraudeath / histowdeaths)]

# add correction to population projection
projdata <- merge(proj, citycal, allow.cartesian = T, all = T)
projdata[, ":="(pop = wittpop * popfac, death = wittdeath * dfac)]

# discard the duplicates of historical period
projdata[, ssp := as.character(ssp)]
projdata[year5 %between% histrange, ssp := "hist"]
projdata <- unique(projdata)


# -- add settlement type percentage data
projdata <- merge(projdata, projsettl, by = 'URAU_CODE', allow.cartesian = T, all = T)
projdata <- projdata[rowSums(is.na(projdata)) == 0,]

# -- further cleaning

# # climate data
# tmeanproj <- open_dataset("data/artifacts/04_sim_data_tg.gz.parquet") |>
#   collect()

# select cities with full data
cities <- subset(cities, URAU_CODE %in% projdata$URAU_CODE) #  & URAU_CODE %in% tmeanproj$URAU_CODE
projdata <- subset(projdata, URAU_CODE %in% cities$URAU_CODE)

# define factors with nice names
cities <- mutate(cities,
                 across(URAU_CODE:cntr_name, ~ factor(.x, levels = unique(.x)))) %>%
  as.data.table()

# add region info
projdata <- merge(projdata, cities[, .(URAU_CODE, lat, lon, region, cntr_name)], by = "URAU_CODE")

# order demographic data
projdata[, ":="(URAU_CODE = factor(URAU_CODE, levels = levels(cities$URAU_CODE)),
                agegroup = factor(agegroup, levels = agelabs))]
setkey(projdata, ssp, URAU_CODE, agegroup)


#------------------------
# HIA data
#------------------------
print('HIA data')
# -- from Masselot et al. 2025

# ERA5Land data to perform QM calibration
raw_era5_temp <- open_dataset("data/from_Masselot_NatMed_2025/era5series.gz.parquet")

# HIA coefficients
raw_coef_hia <- open_dataset("data/from_Masselot_NatMed_2025/coef_simu.gz.parquet")


#------------------------
# Tree cover data
#------------------------
print('Tree cover data')
# load tree cover data
treecover <- rast(sprintf("%s/tree_cover_EUR_UK.tif",adir))

## load tree cover data
# treecover_eu <- rast(sprintf("%s/europe_tree_cover_2023_complete_CLEANED.tif",adir))
# treecover_uk <- vect('data/tree_cover_uk/national_tes.geojson')
#
## merge EU and UK data
# treecover_uk_3035 <- project(treecover_uk, crs(treecover_eu))
# uk_rasterized <- rasterize(treecover_uk_3035, treecover_eu, field = "treecanopy")
# uk_rasterized <- uk_rasterized * 100
# treecover <- merge(uk_rasterized, treecover_eu)
# writeRaster(treecover, file = 'data/artifacts/tree_cover_EUR_UK.tif')

#------------------------
# Aesthetics
#------------------------
print('Aesthetics')
pal_color_gcm_calib <- c("Observed" = "grey50",
                         "GCM Raw" = "lightgreen",
                         "GCM Calibrated" = "lightgreen")
pal_linetype_gcm_calib <- c("Observed" = "solid",
                            "GCM Raw" = "solid",
                            "GCM Calibrated" = "dashed")
pal_labels_gcm_calib <- c("Observed" = "Obs",
                         "GCM Raw" = "GCM Raw",
                         "GCM Calibrated" = "GCM Calib")

pal_color_cv <- c('General CV' = '#5F8F9F',
                  'Spatial CV' = '#B1D4ED')

pal_color_calib <- c('Original' = '#7F7F7F',
                     'Calibrated' = '#90EE90')

# pal_color_regions <- c('Northern' = '#548687', 'Southern' = '#b0413e', 'Eastern' = '#ffffc7','Western' = '#fcaa67')
pal_color_regions <- c('Northern' = '#9122B0', 'Southern' = '#BF2525', 'Eastern' = '#68A830','Western' = '#D5A92E')
pal_color_regions_scen <- c('Northern.BASE' = '#9122B0', 'Southern.BASE' = '#BF2525', 'Eastern.BASE' = '#68A830','Western.BASE' = '#D5A92E',
                       'Northern.CLIM' = '#50185F', 'Southern.CLIM' = '#6C1414', 'Eastern.CLIM' = '#2D500F','Western.CLIM' = '#7A5F16',
                       'Northern.BASE.bg' = '#E8DAEF', 'Southern.BASE.bg' = '#FADBD8', 'Eastern.BASE.bg' = '#D4EFDF','Western.BASE.bg' = '#FCF3CF',
                       'Northern.CLIM.bg' = '#E8DAEF', 'Southern.CLIM.bg' = '#FADBD8', 'Eastern.CLIM.bg' = '#D4EFDF','Western.CLIM.bg' = '#FCF3CF')
pal_color_scen <- c('BASE' = '#F29411', 'CLIM' = '#820263', 'ADAPT' = '#8ac926',
                            'BASE_light' = '#FFDBAD', 'CLIM_light' = '#FFD5F4', 'ADAPT_light' = '#D1FF88')
order_regions <- c('Northern','Southern','Eastern','Western')
pal_labels_regions <- c('Northern' = 'North',
                        'Southern' = 'South',
                        'Eastern' = 'East',
                        'Western' = 'West')
pal_labels_regions_scen <- c('Northern.BASE' = 'North - BASE',
                             'Southern.BASE' = 'South - BASE',
                             'Eastern.BASE' = 'East - BASE',
                             'Western.BASE' = 'West - BASE',
                             'Northern.CLIM' = 'North - MITIG',
                             'Southern.CLIM' = 'South - MITIG',
                             'Eastern.CLIM' = 'East - MITIG',
                             'Western.CLIM' = 'West - MITIG')
pal_labels_scen <- c('BASE' = 'BASE',
                     'CLIM' = 'MITIG',
                     'ADAPT' = 'ADAPT')

pal_color_deaths1 <- c('#a7c957', '#f0ead2', '#8c1c13')
pal_color_deaths2 <- c('#80b918', '#ffba08', '#e36414', '#941b0c')

pal_color_settl <- c('urban' = '#ffba08', 'rural' = '#80b918',
                     'uhi' = '#8e7dbe', 'total' = '#395C6B',
                     'urban_light' = '#F9D883', 'rural_light' = '#E2FFAD',
                     'urban_dark' = '#a17400', 'rural_dark' = '#324809',
                     'uhi_dark' = '#44366c', 'total_dark' = '#12303D')
pal_color_settl_dark <- c('urban_dark' = '#a17400', 'rural_dark' = '#324809',
                          'uhi_dark' = '#44366c', 'total_dark' = '#12303D')

pal_color_adapt <- c(perc0 = '#404040', perc30 = '#3BA808',
                     perc0_light = '#B1B1B1', perc0_light = '#96E771')
pal_labels_adapt <- c('perc0' = 'MITIG',#0%',
                      'perc30' = 'ADAPT',
                      'perc0_light' = 'MITIG',#0%',
                      'perc30_light' = 'ADAPT')#30%')
pal_labels_adapt_sens <- c('perc0' = '0%',
                           'perc25' = '25%',
                           'perc30' = '30%',
                           'perc35' = '35%')
pal_color_adapt_sens <- c('perc0' = '#9AABAF',
                          'perc25' = '#8ecae6',
                          'perc30' = '#00b4d8',
                          'perc35' = '#023047')

pal_linetype_settl <- c('urban' = 'solid',
                        'rural' = 'dotted')
pal_labels_settl <- c('urban' = 'City centre',
                      'rural' = 'Outskirts',
                      'total' = 'Entire city',
                      'urban_light' = 'City centre',
                      'rural_light' = 'Outskirts',
                      'urban_dark' = 'City centre',
                      'rural_dark' = 'Outskirts',
                      'total_dark' = 'Entire city')

est_vars <- c("est","low","high")
met_vars <- c("an","af","rate","cuman")

basic_theme <- theme_minimal() +
  theme(plot.background = element_rect(fill = 'white', color = 'white'),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.spacing.y = unit(-0.5, "cm"),
        legend.box.margin = margin(t = -15, unit = "pt"),
        legend.title = element_text(hjust = 0.5, face = "bold", size = 10, margin = margin(r = 5)),
        title = element_text(hjust = 0, face = "bold", size = 12),
        strip.text = element_text(hjust = 0.5, face = "bold", size = 12),
        axis.title = element_text(hjust = 0.5, face = "plain", size = 10),
        panel.border = element_rect(colour = 1, fill = NA),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"))
legend_justif_theme <- theme(
  legend.box.just = "left",
  legend.justification = "left"
)

si_theme <- basic_theme +
  theme(strip.text = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))


#------------------------
# CLEAN cache & tmp files
#------------------------
print('CLEAN cache & tmp files')
tmpFiles(remove = TRUE)
gc()

