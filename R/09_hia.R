################################################################################
#
# Heat-related health impacts across national mitigation and urban adaptation scenarios in European cities
#
# R Code Step 9a: Perform Health Impact Assessment (HIA) at the URAU CODE level
# R Code Step 9b: Restructure the raw HIA data for future Global and Regional CI computation
# R Code Step 9c: Compute CI by Region and at the Global level
#
# Clàudia Rodés-Bachs
#
################################################################################

cat(sprintf("%s Running 09_hia\n
            ==============================\n
            ==============================\n
            ==============================\n",
            as.character(Sys.time())),
    file = sprintf("%s/00_trace.txt", tdir), append = TRUE) |> try()

# initialize trace
dir.create(tdir, recursive = T, showWarnings = FALSE)
writeLines(c(""), sprintf("%s/09_trace.txt", tdir))
cat(sprintf("================================\n%s\n", as.character(Sys.time())),
    file = sprintf("%s/09_trace.txt", tdir), append = FALSE) |> try()



source('R/09a_hia.R')

source('R/09b_hia_global.R')

source('R/09c_hia_global.R')


