################################################################################
#
# Heat-related health impacts across national mitigation and urban adaptation scenarios in European cities
#
# R Code Step 10: Figures SI
#
# Clàudia Rodés-Bachs
#
################################################################################


if (!dir.exists('figures/fig_si/')) dir.create('figures/fig_si/')

# -------------------------------------------------------------------------------
# Figure extra1: regions
# -------------------------------------------------------------------------------

plotmap <- projdata %>%
  select(URAU_CODE, region, lat, lon) %>%
  unique() %>%
  mutate(region = factor(region, order_regions))

pl <- ggplot(plotmap) +
  # european map layout
  geom_sf(data = euromap, fill = grey(.9), col = "white", inherit.aes = F) +
  coord_sf(xlim = range(plotmap$lon), ylim = range(plotmap$lat),
           lims_method = "box", crs = st_crs(euromap), default_crs = st_crs(4326)) +

  # cities
  geom_point(data = plotmap, aes(x = lon, y = lat, fill = region),
             shape = 21, stroke = .01, alpha = 1, size = 3) +

  # palette
  scale_fill_manual(values = pal_color_regions,
                    name = 'Regions') +

  # labs
  labs(title = '', x = '', y = '') +

  # theme and layout
  basic_theme +
  theme(axis.text = element_blank())

pl

# save plot
ggsave(paste0("figures/fig_si/figSI_extra1_regions.pdf"), plot = pl, width = 13, height = 10)
ggsave(paste0("figures/fig_si/figSI_extra1_regions.png"), plot = pl, width = 13, height = 10)












# -------------------------------------------------------------------------------
# Figure extra meth: 07 - two step calibration - UHI
# -------------------------------------------------------------------------------

# ----
# -- panel A -- RMSE by GCM from 07: 1step calib for UHI estimation
# ----
# historical temperature summaries
tsumhist <- data_07_tsum %>%
  filter(calperiod == "hist", scen == 'BASE') %>%
  select(gcm = model, perc, tsim, full, demo, URAU_CODE = city) %>%
  collect()
tsumhist$perc <- as.numeric(tsumhist$perc)

# observed temperature quantiles
tsumobs <- read_parquet(sprintf("%s/data_from_Masselot_NatMed_2025_era5series.gz.parquet", adir)) %>%
  subset(year(date) %between% histrange) %>%
  reframe(perc = predper, obs = quantile(era5landtmean, predper / 100),
          .by = URAU_CODE)

# merge
tsumhist <- merge(tsumhist, tsumobs, by = c("URAU_CODE", "perc"))

# compute RMSEs
tcalib <- tsumhist[,
                   .(Original = sqrt(mean((obs - tsim)^2)),
                     Calibrated = sqrt(mean((obs - full)^2))),
                   by = .(URAU_CODE, gcm)]

# reshape
tcalib <- melt(tcalib, id.vars = c("URAU_CODE", "gcm"),
               measure.vars = c("Original", "Calibrated"),
               variable.name = "type", value.name = "rmse")

# plot
mapfig_meth_07_1 <- ggplot(tcalib) +

  geom_boxplot(aes(fill = type, y = rmse, x = gcm,
                   group = interaction(gcm, type)), outlier.size = .1,
               size = .1) +
  geom_hline(yintercept = 0, color = 'gray80', linetype = 'dashed') +

  # palette
  scale_fill_manual(values = pal_color_calib) +

  #labs
  labs(x = "", y = "RMSE (ºC)", fill = "Source") +

  # theme and layout
  basic_theme + legend_justif_theme

ggsave(paste0("figures/fig_si/figSI_meth_07_1.pdf"), plot = mapfig_meth_07_1, width = 13, height = 10)
ggsave(paste0("figures/fig_si/figSI_meth_07_1.png"), plot = mapfig_meth_07_1, width = 13, height = 10)

# ----
# -- panel B -- 2n QM step - Delta between City Centre & Outskirts temperature
# ----
data_07p2 <- dcast(dataa_08[scen == 'BASE' &
                              gcm == 'CanESM5' &
                              year == 2050 &
                              # Delta is static, so simply select 1 year-gcm-scen combination
                              adapt == 'perc0',],
                   URAU_CODE ~ settl,
                   value.var = "tas") %>%
  merge(projdata[, .(URAU_CODE, lon, lat, pop)],
        by = c("URAU_CODE"))
data_07p2[, delta := urban - rural]

plotmap <- data_07p2

# cut points for palette
cutpts <- unique(sort(c(0, unname(round(
  quantile(plotmap$delta, seq(0, 1, length.out = 10)) / 5, 2) * 5))))
plotmap[, colgrp := cut(delta, cutpts)]
signtab <- table(factor(sign(cutpts), c(-1, 0, 1)))

# palettes (fill and border)
npal <- (max(signtab)) * 2
pal <- c(scico(npal, palette = "bam", direction = -1)[
  max(signtab) - signtab[1] + seq_len(signtab[1])],
  scico(tail(signtab, 1), palette = "navia", direction = -1))
bpal <- rep(c("white", "black"), signtab[c("-1", "1")])
names(pal) <- levels(plotmap$colgrp)

mapfig_meth_07_2 <- fun_map(plotmap) +

  # labs
  labs(fill = "Delta (ºC)",
       size = "Population (in millions)",
       x = '', y = '')

# save plot
ggsave(paste0("figures/fig_si/figSI_meth_07_2_delta.pdf"), plot = mapfig_meth_07_2, width = 13, height = 10)
ggsave(paste0("figures/fig_si/figSI_meth_07_2_delta.png"), plot = mapfig_meth_07_2, width = 13, height = 10)



# # ----
# # -- panel extra -- temperature calibration 07: 1step calib for UHI estimation
# # ----
# data_gcm <- data_07_data[calperiod == 'hist', ]
# data_calib <- data_gcm[, .(date, temp = full, model, URAU_CODE)]
# data_raw <- data_gcm[, .(date, temp = tsim, model, URAU_CODE)]
# data_obs <- tg_obs_city[, .(date, temp = tg, URAU_CODE)]
#
# data_obs_exp <- rbindlist(lapply(models, function(m) {
#   copy(data_obs)[URAU_CODE %like% "*C$", ":="(model = m, type = "Observed")]
# }))
#
# data <- rbindlist(list(
#   # 1. Observed
#   data_obs_exp,
#   # 2. GCM Raw
#   data_raw[URAU_CODE %like% "*C$", type := "GCM Raw"],
#   # 3. Calibrated
#   data_calib[URAU_CODE %like% "*C$", type := "GCM Calibrated"]
# ), use.names = TRUE)
#
# # add cities info
# data2 <- data |>
#   merge(unique(projdata[, .(URAU_CODE, region, cntr_name)]),
#         by = 'URAU_CODE')
#
#
# # ecdf computation
# data_ecdf_uc <- data2[!is.na(temp),
#                       .(temp = sort(temp),
#                         prob = seq_len(.N)/.N),
#                       by = .(cntr_name, type, model)]
# data_ecdf_uc[, type := factor(type, levels = rev(sort(unique(type))))]
# data_ecdf_uc[, cntr_name := factor(cntr_name, levels = sort(unique(cntr_name)))]
#
# mapfig6.1 <- ggplot(data_ecdf_uc[cntr_name == 'Greece' & model == models[1]],
#                     aes(x = temp, y = prob, color = type, linetype = type)) +
#   geom_line(linewidth = 0.6) +
#
#   # facet
#   facet_grid(cntr_name ~ model) +
#
#   # palette
#   scale_color_manual(values = pal_color_gcm_calib,
#                      labels = pal_labels_gcm_calib) +
#   scale_linetype_manual(values = pal_linetype_gcm_calib,
#                         labels = pal_labels_gcm_calib) +
#
#   # labs
#   labs(x = "Temperature (ºC)", y = "Probability",
#        color = 'Source', linetype = 'Source') +
#
#
#   # theme and layout
#   basic_theme + legend_justif_theme +
#   theme(legend.position = "bottom",
#         strip.text = element_blank())
#
# # save
# ggsave("figures/subfig6/fig6.1_07aST1.pdf", plot = mapfig6.1, width = 13, height = 10)
# ggsave("figures/subfig6/fig6.1_07aST1.png", plot = mapfig6.1, width = 13, height = 10)
#
#
# # ----
# # -- panel extra pages -- temperature calibration 07: 1step calib for UHI estimation by CNTRY -- pages
# # ----
#
# # pagination setup
# cities_per_page <- 10
# all_cntries <- unique(data_ecdf_uc$cntr_name)
# num_pages <- ceiling(length(all_cntries) / cities_per_page)
#
# # Create a directory to store the sub-figures
# dir.create("figures/fig_si/figSI_6.1_pages", showWarnings = FALSE, recursive = TRUE)
#
# # 4. Loop through pages
# for (i in 1:cities_per_page) {
#
#   # Identify cities for the current page
#   start_idx <- ((i - 1) * cities_per_page) + 1
#   end_idx   <- min(i * cities_per_page, length(all_cntries))
#   current_cntries <- all_cntries[start_idx:end_idx]
#
#   data_tmp <- data_ecdf_uc[cntr_name %in% current_cntries]
#
#   # Create the plot for this specific chunk
#   p <- ggplot(data_tmp,
#               aes(x = temp, y = prob, color = type, linetype = type)) +
#     geom_line(linewidth = 0.3) +
#
#     # facet
#     facet_grid(cntr_name ~ model) +
#
#     # palette
#     scale_color_manual(values = pal_color_gcm_calib) +
#     scale_linetype_manual(values = pal_linetype_gcm_calib) +
#
#     # labs
#     labs(x = "Temperature (ºC)", y = "Probability", color = 'Type', linetype = 'Type') +
#
#
#     # theme and layout
#     basic_theme +
#     theme(legend.position = "bottom")
#
#   # save
#   ggsave(sprintf("figures/fig_si/figSI_6.1_pages/fig6.1_page_%03d.pdf", i),
#          plot = p, width = 8.27, height = 11.69, device = cairo_pdf)
#
#   message(sprintf("Saved page %d of %d", i, num_pages))
#   rm(data_tmp); gc()
# }


# ----
# ----
# -- FIG meth -- composition 07
# ----
clean_mapfig07_1 <- mapfig_meth_07_1 +
  labs(title = '') +
  theme(plot.margin = margin(t = 0, r = 1, b = 0, l = 1),
        legend.justification = "center") +
  theme(aspect.ratio = 1)
clean_mapfig07_2 <- mapfig_meth_07_2 +
  labs(title = '') +
  theme(plot.margin = margin(t = 0, r = 1, b = 0, l = 1),
        legend.justification = "center")

fig_si_meth07 <- plot_grid(
  clean_mapfig07_1,
  clean_mapfig07_2,
  ncol = 2,
  labels = c('a)', 'b)'),
  rel_widths = c(1, 1),
  label_size = 14
)

ggsave(paste0("figures/fig_si/figSI_meth_07.pdf"), plot = fig_si_meth07, width = 10, height = 6)
ggsave(paste0("figures/fig_si/figSI_meth_07.png"), plot = fig_si_meth07, width = 10, height = 6)




# -------------------------------------------------------------------------------
# Figure meth: 06 - LM COOLING CV
# -------------------------------------------------------------------------------
# ----
# -- FIG -- LM cooling model performance
# ----
# summarise data by URAU_CODE
data <- data_cool_06[, .(
  general_R2   = mean(summary_R2,   na.rm = TRUE),
  general_RMSE = mean(summary_RMSE, na.rm = TRUE),
  spatial_R2   = mean(spatialcv_R2,   na.rm = TRUE),
  spatial_RMSE = mean(spatialcv_RMSE, na.rm = TRUE)
), by = c('URAU_CODE' = 'urau_code')]

# reshape
data <- melt(data,
             id.vars = "URAU_CODE",
             measure.vars = patterns("_R2$", "_RMSE$"),
             variable.name = "cv_type",
             value.name = c("R2", "RMSE"))
data[, cv_type := fcase(
  cv_type == 1, "General CV",
  cv_type == 2, "Spatial CV"
)]

# add region info
data <- merge(data, unique(projdata[, .(URAU_CODE, region)]),
              by = c("URAU_CODE")) %>%
  mutate(region = factor(region, order_regions)) %>%
  as.data.table()


# R2 plot
mapfig_meth_06.1 <- ggplot(data, aes(x = cv_type, y = R2, fill = cv_type)) +
  geom_boxplot(alpha = 0.4, outlier.shape = 21) +
  geom_jitter(aes(color = region), width = 0.1, alpha = 0.5, size = 1.5) +

  # mean by region
  stat_summary(aes(color = region, group = region),
               fun = mean,
               geom = "crossbar",
               width = 0.6,
               linewidth = 0.5,
               linetype = "dashed") +

  # palette
  scale_fill_manual(values = pal_color_cv) +
  scale_color_manual(values = pal_color_regions,
                     name = 'Region') +

  # labs
  labs(x = NULL, y = "R²") +

  # theme and layout
  basic_theme + legend_justif_theme +
  guides(fill = 'none')

# RMSE plot
mapfig_meth_06.2 <- ggplot(data, aes(x = cv_type, y = RMSE, fill = cv_type)) +
  geom_boxplot(alpha = 0.4, outlier.shape = 21) +
  geom_jitter(aes(color = region), width = 0.1, alpha = 0.5, size = 1.5) +

  # mean by region
  stat_summary(aes(color = region, group = region),
               fun = mean,
               geom = "crossbar",
               width = 0.6,
               linewidth = 0.5,
               linetype = "dashed") +

  # palette
  scale_fill_manual(values = pal_color_cv) +
  scale_color_manual(values = pal_color_regions,
                     name = 'Region') +

  # labs
  labs(x = NULL, y = "RMSE (°C)") +

  # theme and layout
  basic_theme + legend_justif_theme +
  guides(fill = 'none')


legend <- get_legend(
  mapfig_meth_06.2 +
    theme(legend.position = "bottom")
)
mapfig_meth_06 <- plot_grid(
  mapfig_meth_06.1 +
    theme(legend.position = 'none',
          plot.margin = margin(t = 3, r = 2, b = 0, l = 0)),
  mapfig_meth_06.2 +
    theme(legend.position = 'none',
          plot.margin = margin(t = 3, r = 0, b = 0, l = 1)),
  ncol = 2,
  labels = c('a)', 'b)'),
  rel_widths = c(1, 1),
  label_size = 14
)
mapfig_meth_06 <- plot_grid(
  mapfig_meth_06,
  plot_grid(NULL, legend, NULL, ncol = 3, rel_widths = c(1, 2, 1)),
  ncol = 1,
  rel_heights = c(1, 0.1)
)
# save plot
ggsave(paste0("figures/fig_si/figSI_meth_06_lm_cv.pdf"), plot = mapfig_meth_06, width = 10, height = 5)
ggsave(paste0("figures/fig_si/figSI_meth_06_lm_cv.png"), plot = mapfig_meth_06, width = 10, height = 5)


## TO PRINT -- RMSE summary values
toprint <- data %>%
  group_by(cv_type) %>%
  summarise(RMSE_mean = quantile(RMSE, 0.50, na.rm = TRUE),
            RMSE_low = quantile(RMSE, 0.05, na.rm = TRUE),
            RMSE_high = quantile(RMSE, 0.95, na.rm = TRUE)) %>%
  ungroup()
print(toprint)

# ----
# -- TABLE -- LM cooling model performance by URAU CODE
# ----
uc_lm_summary <- data_cool_06[, .(
  general_R2   = mean(summary_R2,   na.rm = TRUE),
  general_RMSE = mean(summary_RMSE, na.rm = TRUE),
  spatial_R2   = mean(spatialcv_R2,   na.rm = TRUE),
  spatial_RMSE = mean(spatialcv_RMSE, na.rm = TRUE)
), by = c('URAU_CODE' = 'urau_code')]

uc_lm_summary[, .(
  URAU_CODE,
  general_R2   = round(general_R2,   3),
  general_RMSE = round(general_RMSE, 2),
  spatial_R2   = round(spatial_R2,   3),
  spatial_RMSE = round(spatial_RMSE, 2)
)] %>%
  gt() %>%
  cols_label(
    URAU_CODE     = "City",
    general_R2    = "R²", general_RMSE    = "RMSE",
    spatial_R2    = "R²", spatial_RMSE    = "RMSE"
  ) %>%
  tab_spanner(label = "General",     columns = c(general_R2,  general_RMSE)) %>%
  tab_spanner(label = "Spatial CV",  columns = c(spatial_R2,  spatial_RMSE))

# positions of jump between countries
ctries <- substr(uc_lm_summary$URAU_CODE, 1, 2)
pos_change <- which(ctries[-1] != ctries[-length(ctries)])

# latex table
cat(uc_lm_summary %>%
      kbl(format = "latex", longtable = TRUE,
          booktabs = TRUE, digits = 3, linesep = "") %>%
      kable_styling(latex_options = c("striped", "repeat_header")) %>%
      row_spec(pos_change, extra_latex_after = "\\addlinespace"),
    file = "figures/fig_si/figSI_6_TABLE_uc_lm_summary.tex")

write.csv(uc_lm_summary, file = 'figures/fig_si/06_lm_city_summary.csv', row.names = F)








# -------------------------------------------------------------------------------
# Figure meth: 06 - LM LST to AmbientTemp
# -------------------------------------------------------------------------------
# ----
# -- FIG -- LM cooling model performance
# ----
# Predicted vs Observed plot
mapfig_meth_06.3 <- ggplot(data_lst_06,
                           aes(x = tg_new, y = tg_avg, colour = region)) +
  geom_point(alpha = 0.5) +

  # reference line
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", linewidth = 0.5) +

  # palette
  scale_color_manual(values = pal_color_regions,
                     name = 'Region') +

  # labs
  labs(x = "Predicted [ºC]", y = "Observed [ºC]") +

  # theme and layout
  basic_theme + legend_justif_theme +
  guides(fill = 'none')

# save plot
ggsave(paste0("figures/fig_si/figSI_meth_06_lm_lst.pdf"), plot = mapfig_meth_06.3, width = 10, height = 5)
ggsave(paste0("figures/fig_si/figSI_meth_06_lm_lst.png"), plot = mapfig_meth_06.3, width = 10, height = 5)


# -------------------------------------------------------------------------------
# Figure extra meth: 09 - 1st step calibration - HIA
# -------------------------------------------------------------------------------

# ----
# -- FIG -- RMSE by GCM from 09: 1step calib for HIA
# ----
# historical temperature summaries
tsumhist <- data_09_tsum %>%
  filter(calperiod == "hist", scen == 'BASE') %>%
  select(!c(calperiod, scen)) %>%
  collect()
tsumhist$perc <- as.numeric(tsumhist$perc)

# observed temperature quantiles
tsumobs <- read_parquet(sprintf("%s/data_from_Masselot_NatMed_2025_era5series.gz.parquet", adir)) %>%
  subset(year(date) %between% histrange) %>%
  reframe(perc = predper, obs = quantile(era5landtmean, predper / 100),
          .by = URAU_CODE)

# merge
tsumhist <- merge(tsumhist, tsumobs, by = c("URAU_CODE", "perc"))

# compute RMSEs
tcalib <- tsumhist[,
                   .(Original = sqrt(mean((obs - tas)^2)),
                     Calibrated = sqrt(mean((obs - full)^2))),
                   by = .(URAU_CODE, gcm)]

# reshape
tcalib <- melt(tcalib, id.vars = c("URAU_CODE", "gcm"),
               measure.vars = c("Original", "Calibrated"),
               variable.name = "type", value.name = "rmse")

# plot
mapfig_meth_09 <- ggplot(tcalib) +

  geom_boxplot(aes(fill = type, y = rmse, x = gcm,
                   group = interaction(gcm, type)), outlier.size = .1,
               size = .1) +
  geom_hline(yintercept = 0, color = 'gray80', linetype = 'dashed') +

  # palette
  scale_fill_manual(values = pal_color_calib) +

  #labs
  labs(x = "", y = "RMSE (ºC)", fill = "Source") +

  # theme and layout
  si_theme + legend_justif_theme +
  theme(legend.justification = 'center')

ggsave(paste0("figures/fig_si/figSI_meth_09.pdf"), plot = mapfig_meth_09, width = 13, height = 5)
ggsave(paste0("figures/fig_si/figSI_meth_09.png"), plot = mapfig_meth_09, width = 13, height = 5)

