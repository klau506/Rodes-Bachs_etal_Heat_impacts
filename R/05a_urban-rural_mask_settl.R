################################################################################
#
# Heat-related health impacts across national mitigation and urban adaptation scenarios in European cities
#
# R Code Step 5: Create urban-rural mask by socioeconomic item
# R Code Step 5a: Create a urban-rural(-excluded) mask based on CORINE database
#
# Clàudia Rodés-Bachs
#
################################################################################

cat(sprintf("%s Running 05a_urban-rura_mask_settl\n
            ==============================\n
            ==============================\n
            ==============================\n",
            as.character(Sys.time())),
    file = sprintf("%s/00_trace.txt", tdir), append = TRUE) |> try()

cat(sprintf("%s Running 05a_urban-rura_mask_settl\n
            ==============================\n
            ==============================\n",
            as.character(Sys.time())),
    file = sprintf("%s/05_trace.txt", tdir), append = TRUE) |> try()

#------------------------
# Process data
#------------------------

# get the Raster Attribute Table (RAT) from the raster
# by creating a table with the 'ID' (num) and the 'LABEL3' (char)
rat <- as.data.table(levels(clc_2018)[[1]])

# add the numeric column to corine_map
corine_map[, category_num := fcase(
  Category1 == "URBAN", 0,
  Category1 == "RURAL", 1,
  Category1 == "EXCLUDED", 2,
  default = 2
)]
reclass_dt <- merge(rat, corine_map, by = "LABEL3")

# transform it to a numeric matrix
reclass_matrix_numeric <- as.matrix(reclass_dt[, .(ID, category_num)])

# transform the raster to numeric values (lost of the categorical names)
# to ensure classify() function works
clc_raw <- as.numeric(clc_2018)


# transform the urau map to match that SAME standard CRS
urau_map_land_proj <- st_transform(urau_map, st_crs(clc_raw))

# reclassify the raster
clc_numeric <- classify(clc_raw, reclass_matrix_numeric)

# consider the URAU_CODEs polygons
urau_crop <- crop(clc_numeric, urau_map_land_proj, datatype = "INT1U")
urau_land_cover_categorical <- mask(urau_crop, urau_map_land_proj, updatevalue = 2)


#------------------------
# SAVE rasters
#------------------------
writeRaster(urau_land_cover_categorical,
            filename = sprintf("%s/05_urau_land_cover_categorical.tif",adir),
            overwrite = TRUE)

urau_land_cover_binary <- clc_numeric
writeRaster(urau_land_cover_binary,
            filename = sprintf("%s/05_urau_land_cover_binary.tif",adir),
            overwrite = TRUE)


#------------------------
# COMPUTE percentage of urb-rur by URAU CODE
#------------------------

cat(as.character(Sys.time()), "COMPUTE percentage of urb-rur by URAU CODE\n
    ========================================
    ========================================",
    file = sprintf("%s/05_trace.txt", tdir), append = TRUE) |> try()

counts_df <- data.frame()
for (uc in urau_codes) {
  print(uc)
  uc_geom <- urau_map %>%
    filter(URAU_CODE == uc) %>%
    st_as_sf()
  if (nrow(uc_geom) == 0) next

  # subset to URAU CODE
  uc_vect <- vect(uc_geom)
  uc_projected <- project(uc_vect, crs(urau_land_cover_binary_clean))
  uc_raster_masked_settl2 <- crop(urau_land_cover_binary_clean, uc_projected, mask = TRUE)

  # calculate percentage per layer
  counts_df2 <- as.data.table(freq(uc_raster_masked_settl2))
  counts_df2[, percentage := (count / sum(count)) * 100, by = layer]

  # attach
  counts_df2[, URAU_CODE := uc]
  counts_df <- rbind(counts_df, counts_df2)
}

write_parquet(counts_df, sprintf("%s/05_urau_city_settl_percentage.gz.parquet", adir))



#------------------------
# CLEAN
#------------------------

cat(as.character(Sys.time()), "05a_urban-rural_mask_settl.R run succesfully\n",
    file = sprintf("%s/05_trace.txt", tdir), append = TRUE) |> try()
cat(as.character(Sys.time()), "Cleaning temporal files and directories\n
    =======================================\n
    =======================================\n",
    file = sprintf("%s/05_trace.txt", tdir), append = TRUE) |> try()


# delete all created temporary files
tmpFiles(remove = TRUE)
gc()
