################################################################################
#
# Heat-related health impacts across national mitigation and urban adaptation scenarios in European cities
#
# R Code Step 09b: HIA for global (Europe) results.
# Restructures the raw HIA data, currently saved by URAU CODE, scenario and age group,
# to be saved by MC simulation to latter compute the Global impact measures.
#
# Clàudia Rodés-Bachs
#
################################################################################

cat(sprintf("%s Running 09b_hia\n
            ==============================\n
            ==============================\n
            ==============================\n",
            as.character(Sys.time())),
    file = sprintf("%s/00_trace.txt", tdir), append = TRUE) |> try()

# for (i in seq(0,8)) {
  for (ires in c((i*25+1):(min((i+1)*25,500)))) {
    print(ires)
    idir <- sprintf("%s/09_hia_global_ires/09_hia_global_%s.gz.parquet", tdir, ires)
    time_limit_22 <- as.POSIXct("2026-07-10 08:00:00")
    if (file.exists(idir)) {
      info <- file.info(idir)
      date_modification <- info$mtime
      if (date_modification >= time_limit_22) {
        print(sprintf("%s Skipping %s\n",
                      as.character(Sys.time()), as.character(ires)))
        next
      }
    }

    data <- open_dataset(sprintf("%s/09_hia_global", tdir),
                         partitioning = c('URAU_CODE','scen','ageggroup')) %>%
      filter(res == paste0('sim',ires))

    idir <- sprintf("%s/09_hia_global_ires", tdir)
    dir.create(idir, recursive = T, showWarnings = F)
    write_parquet(data, sprintf("%s/09_hia_global_%s.gz.parquet", idir, ires))
  }
# }
