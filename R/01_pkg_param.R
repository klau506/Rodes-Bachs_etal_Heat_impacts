################################################################################
#
# Heat-related health impacts across national mitigation and urban adaptation scenarios in European cities
#
# R Code Step 1: Load packages & analysis parameters
#
# Clàudia Rodés-Bachs
#
################################################################################


#------------------------
# LOAD Packages
#------------------------

# DIPC library path
# .libPaths(c("/scratch/bc3lc/R/x86_64-pc-linux-gnu-library/4.2", .libPaths()))


library(dplyr)            # Data.frame management
library(tidyr)            # For reshaping functions
library(data.table)       # For very large databases
library(tibble)           # For datasets management
library(dtplyr)           # To use dplyr verbs on data.tables
library(arrow)            # To deal with datasets that cannot be loaded all at once
library(stringr)          # Deal with strings
library(sf)               # For mapping
library(terra)            # To deal with rasters (ATTENTION: version >= 1.8.93)
library(tidyterra)        # To deal with SpatRasters
library(geodata)          # To download climate, elevation, and administrative boundary data
library(collapse)         # For fquantile (ATTENTION: version >= 2.1.6)
library(lubridate)        # To handle dates and times
library(exactextractr)    # To do computations over rasters
library(ggplot2)          # To plot
library(patchwork)        # For side-by-side plots
library(eurostat)         # For EUROSTAT data
library(giscoR)           # For maps
library(foreach)          # To parallelise
library(countrycode)      # For ISO2 - ISO3 conversion
library(doParallel)       # To parallelise
library(doSNOW)           # To parallelise
library(mgcv)             # To evaluate lm
library(dlnm)             # To create bases for RR computation
library(splines)          # To create bases for RR computation
library(xlsx)             # To deal with xlsx files
library(scico)            # To create fancy color palettes
library(cowplot)          # To aggregate plots
library(plotly)           # To plot geom_tile
library(scales)           # To modify plot axis
library(ggpattern)        # To add patterns in plots
library(ggpp)             # To stack and nudge altogether geom_col
library(gt)               # To create tables
library(knitr)            # To create tables
library(kableExtra)       # To save tables in .tex
library(forcats)          # To deal with factors
library(lme4)             # To do mixed fixed effects models
library(performance)      # To evaluate mixed fixed effects models
# library(plyr)           # To compute ecdf

rerun <- FALSE            # re-run all scripts or simply load their output

#------------------------
# FOLDERS SETUP
#------------------------

adir <- 'data/artifacts'      # artifacts directory
tdir <- 'data/tmp'            # temporary directory
fdir <- 'figures'             # figures directory
mdir <- 'figures/meth'        # figures to create methodology workflows directory
uc_exp <- 'ES002C'            # city to be used to create the meth workflows
ncores <- 4                   # nº of cores
deleteTempFiles <- FALSE      # TRUE to remove all temporary directories
diagnostics_citygrid_output <- "data/tmp/06_lm_citygrid_diagnostics.csv"  # lm model diagnostic file


#------------------------
# TERRA PKG CONFIGURATION
#------------------------
terra::gdalCache(23000)       # increase GDAL Cache
maxmem <- 1e8                 # max cells in memory
terraOptions(tempdir = tdir)  # assign temporary directory


#------------------------
# SPECIFIC FUNCTIONS
#------------------------
source("R/09_plots.R")            # plotting functions for intermediate steps
source("R/functions_isimip3.R")   # to calibrate simulated temperature with obs data
source("R/functions_impacts.R")   # to calibrate simulated temperature with obs data


#------------------------
# LM DIAGNOSTICS
#------------------------
set.seed(1)                   # fix random seed




cat(sprintf("%s Start running 00_main\n
            ==============================\n",
            as.character(Sys.time())),
    file = sprintf("%s/00_trace.txt", tdir), append = FALSE) |> try()


cat(sprintf("%s Running 01_pkg_param\n
            ==============================\n
            ==============================\n
            ==============================\n",
            as.character(Sys.time())),
    file = sprintf("%s/00_trace.txt", tdir), append = TRUE) |> try()
