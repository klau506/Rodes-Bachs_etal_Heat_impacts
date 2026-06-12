################################################################################
#
# Heat-related health impacts across national mitigation and urban adaptation scenarios in European cities
#
# R Code Step 7: Estimate future (simulated) UHI using a two-step approach:
# R Code Step 7a: Time-related calibration of GCM with CERRA observed data --> RURAL TEMPERATURES
# R Code Step 7b: Spatial-related calibration of step 7a output with CERRA observed data --> URBAN TEMPERATURES
#
# Clàudia Rodés-Bachs
#
################################################################################

cat(sprintf("%s Running 07_sim_uhi\n
            ==============================\n
            ==============================\n
            ==============================\n",
            as.character(Sys.time())),
    file = sprintf("%s/00_trace.txt", tdir), append = TRUE) |> try()

# initialize trace
dir.create(tdir, recursive = T, showWarnings = FALSE)
writeLines(c(""), sprintf("%s/07_trace.txt", tdir))
cat(sprintf("================================\n%s\n", as.character(Sys.time())),
    file = sprintf("%s/07_trace.txt", tdir), append = FALSE) |> try()



# Two step calibration method

# STEP 1: temporal calibration
# Meth:
#   1. identify the temporal bias between RAW GCM & OBS
#   2. apply the identified temporal bias to RAW GCM (in both historically and future periods)
# Result: temporally-bias-corrected GCM ~ RURAL temperatures
source('R/07a_sim_uhi_bias_temporal.R')

# STEP 2: spatial calibration
# Meth:
#   1. identify the bias between URBAN OBS & RURAL OBS
#   2. apply the identified spatial bias to the temporally-bias-corrected GCM
#      to obtain the simulated URBAN GCM temperatures
# Result: temporally-&-spatial-bias-corrected GCM ~ URBAN temperatures
source('R/07b_sim_uhi_bias_spatial.R')


# plot_07_temp_diff_urb_rur()
