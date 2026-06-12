################################################################################
#
# Heat-related health impacts across national mitigation and urban adaptation scenarios in European cities
#
# R Code Step 5: Create urban-rural mask by socioeconomic item
# R Code Step 5a: Create a urban-rural(-excluded) mask based on CORINE database
# R Code Step 5b: Compute ratios of:
#     i)    urban-rural (Total pop)
#     ii)   urban-rural (Male pop)
#     iii)  urban-rural (Female pop)
#     iv)   urban-rural (Y_1564 pop)
#     v)    urban-rural (Y_GE65 pop)
#     based on 5a output
#
# Clàudia Rodés-Bachs
#
################################################################################

cat(sprintf("%s Running 05_urban-rura_mask\n
            ==============================\n
            ==============================\n
            ==============================\n",
            as.character(Sys.time())),
    file = sprintf("%s/00_trace.txt", tdir), append = TRUE) |> try()


# initialize trace
dir.create(tdir, recursive = T, showWarnings = FALSE)
writeLines(c(""), sprintf("%s/05_trace.txt", tdir))
cat(sprintf("================================\n%s\n", as.character(Sys.time())),
    file = sprintf("%s/05_trace.txt", tdir), append = FALSE) |> try()

