################################################################################
#
# Heat-related health impacts across national mitigation and urban adaptation scenarios in European cities
#
# R Code Step 09c: HIA for global (Europe) results.
# Computes the Global impact measures by age group, `all`, Region, and Global.
#
# Clàudia Rodés-Bachs
#
################################################################################

cat(sprintf("%s Running 09c_hia\n
            ==============================\n
            ==============================\n
            ==============================\n",
            as.character(Sys.time())),
    file = sprintf("%s/00_trace.txt", tdir), append = TRUE) |> try()


# Load data
perlen = 5
projdata <- get(load('data/tmp/projdata.RData'))
models <- c('CanESM5','MPI-ESM1-2-LR','MIROC6','UKESM1-0-LL')
scenarios <- c("BASE", "CLIM")
range <- c('heat','cold','tot')

it <- projdata %>%
  select(agegroup, period = year5) %>%
  distinct()
it_all <- it %>%
  mutate(agegroup = 'all') %>%
  distinct()
it <- rbind(it, it_all)
# it <- it_all
it <- crossing(
  it,
  # gcm = models,
  scen = scenarios,
  range = range
) %>%
  filter(period >= 2015, period <= 2064)

iit <- (i*25+1):(min((i+1)*25,nrow(it)))


# Iterate
for (rr in iit) {
  cat(sprintf("%s Performing Global HIA: %s \n",
              as.character(Sys.time()), as.character(rr)),
      file = sprintf("%s/09_trace.txt", tdir), append = TRUE) |> try()
  print(rr)

  # load data
  rrow = it[rr,]

  aagegroup <- rrow$agegroup
  sscen <- rrow$scen
  # ggcm <- rrow$gcm
  rrange <- rrow$range
  pperiod <- rrow$period

  #########################################################################
  #### --- CHECK IF EXISTS
  #########################################################################
  idir <- sprintf("%s/09_hia_city_period/%s/%s/%s/%s/all_GCM/%s/res.parquet",
                  tdir, 'Regional_age', pperiod, aagegroup, sscen, rrange)

  if (file.exists(idir)) next

  #########################################################################
  #### --- FILTER
  #########################################################################
  resage_full <- open_dataset(sprintf("%s/09_hia_global_ires", tdir)) %>%
    filter(ageggroup == aagegroup,
           year >= pperiod,
           year <= pperiod + perlen - 1,
           scen == sscen,
           # gcm == ggcm,
           range == rrange) %>%
    collect() %>%
    rename(agegroup = ageggroup)
  resage_full_original <- data.table::copy(resage_full)


  #########################################################################
  #### --- GLOBAL
  #########################################################################
  print('global')
  # compute impact measures and rename
  resy <- impact_measures(resage_full, vars = c("full", "demo", "clim"),
                          by = c("year", "adapt", "settl_temp", "settl_pop", "range", "res", "scen", "agegroup"))
  setnames(resy, c("full", "demo", "clim"),
           sprintf("an_%s", c("full", "demo", "clim")))
  allvars <- lapply(c("full", "demo", "clim"), grep, names(resy),
                    value = T) |> unlist()

  # aggregate by period
  resy[, period := pperiod]
  periodres <- impact_summarise(resy, vars = allvars,
                                by = c("period", "range", "settl_temp", "settl_pop", "adapt", "scen", "agegroup"))
  # yearres <- impact_summarise(resy, vars = allvars,
  #                             by = c("year", "range", "settl_temp", "settl_pop", "adapt", "scen", "agegroup"))

  # save
  idir <- sprintf("%s/09_hia_city_period/%s/%s/%s/%s/all_GCM/%s",
                  tdir, 'Global_age', pperiod, aagegroup, sscen, rrange)
  dir.create(idir, recursive = T, showWarnings = F)
  write_parquet(periodres, sprintf("%s/res.parquet", idir))

  # idir <- sprintf("%s/09_hia_city_year/%s/%s/%s/%s/all_GCM/%s",
  #                 tdir, 'Global_age', pperiod, aagegroup, sscen, rrange)
  # dir.create(idir, recursive = T, showWarnings = F)
  # write_parquet(yearres, sprintf("%s/res.parquet", idir))


  rm(periodres, resy, resage_full)
  gc()


  #########################################################################
  #### --- REGIONAL
  #########################################################################
  print('regional')
  # add region info
  resage_regional <- resage_full_original %>%
    merge(unique(projdata[,.(URAU_CODE, region)]),
          by = "URAU_CODE")


  # compute impact measures and rename
  resy <- impact_measures(resage_regional, vars = c("full", "demo", "clim"),
                          by = c("year", "adapt", "settl_temp", "settl_pop", "range", "res", "scen", "region", "agegroup"))
  setnames(resy, c("full", "demo", "clim"),
           sprintf("an_%s", c("full", "demo", "clim")))
  allvars <- lapply(c("full", "demo", "clim"), grep, names(resy),
                    value = T) |> unlist()

  # aggregate by period
  resy[, period := pperiod]
  periodres <- impact_summarise(resy, vars = allvars,
                                by = c("period", "range", "settl_temp", "settl_pop", "adapt", "scen", "region", "agegroup"))
  # yearres <- impact_summarise(resy, vars = allvars,
  #                             by = c("year", "range", "settl_temp", "settl_pop", "adapt", "scen", "region", "agegroup"))

  # save
  idir <- sprintf("%s/09_hia_city_period/%s/%s/%s/%s/all_GCM/%s",
                  tdir, 'Regional_age', pperiod, aagegroup, sscen, rrange)
  dir.create(idir, recursive = T, showWarnings = F)
  write_parquet(periodres, sprintf("%s/res.parquet", idir))

  # idir <- sprintf("%s/09_hia_city_year/%s/%s/%s/%s/all_GCM/%s",
  #                 tdir, 'Regional_age', pperiod, aagegroup, sscen, rrange)
  # dir.create(idir, recursive = T, showWarnings = F)
  # write_parquet(yearres, sprintf("%s/res.parquet", idir))


  rm(periodres, resy, resage_regional, resage_full_original)
  gc()
}



#########################################################################
#### --- AGGREGATE
#########################################################################

# and the same for Global_age - Global & Regional_age - Regional

root_path <- sprintf("%s/09_hia_city_period/Regional_age", tdir)
all_files <- list.files(
  path = root_path,
  recursive = TRUE,
  full.names = TRUE,
  pattern = "\\.(parquet|feather|arrow)$"
)
all_files <- all_files[grep('all_GCM', all_files)]

data <- purrr::map_dfr(all_files, function(file_path) {
  df <- read_parquet(file_path)
  if ("range" %in% names(df)) {
    df <- df %>% mutate(range = as.character(range))
  }
  path_parts <- strsplit(file_path, "/")[[1]]
  df <- df %>% mutate(
    gcm      = path_parts[length(path_parts) - 2],
    scenario     = path_parts[length(path_parts) - 3],
    agegroup = path_parts[length(path_parts) - 4],
    period   = path_parts[length(path_parts) - 5]
  )
  return(df)
})

write_parquet(data, sprintf("%s/09_hia_city_period_%s_res.parquet",
                            adir, 'Regional'))

