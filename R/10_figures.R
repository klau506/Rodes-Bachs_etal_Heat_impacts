################################################################################
#
# Heat-related health impacts across national mitigation and urban adaptation scenarios in European cities
#
# R Code Step 10: Figures
#
# Clàudia Rodés-Bachs
#
################################################################################

# -- preliminary loadings

# load packages
source("R/01_pkg_param.R")

# load data
source("R/02_prep_data.R")


# -- load data
data_09_period <- open_dataset(sprintf("%s/09_hia_city_period.gz.parquet", adir)) %>%
  collect() %>%
  as.data.table()

data_09_period_global <- open_dataset(sprintf("%s/09_hia_city_period_%s_res.parquet", adir, 'Global')) %>%
  filter(agegroup == 'all') %>%
  collect() %>%
  as.data.table()

data_09_period_regional <- open_dataset(sprintf("%s/09_hia_city_period_%s_res.parquet", adir, 'Regional')) %>%
  filter(agegroup == 'all') %>%
  collect() %>%
  as.data.table()

data_09_period_global_age <- open_dataset(sprintf("%s/09_hia_city_period_%s_res.parquet", adir, 'Global')) %>%
  filter(agegroup != 'all') %>%
  collect() %>%
  as.data.table()

data_09_tsum <- open_dataset(sprintf("%s/09_hia_city_tsum.gz.parquet", adir)) %>%
  collect() %>%
  as.data.table()

dataa_08 <- open_dataset(sprintf("%s/08_results_temperature.gz.parquet", adir)) %>%
  collect() %>%
  as.data.table()

data_07_data <- open_dataset(sprintf("%s/07_sim_calibStep1_tg.gz.parquet", adir)) %>%
  collect() %>%
  as.data.table()

data_07_tsum <- open_dataset(sprintf("%s/07_sim_calibStep1_tg_tsum.gz.parquet", adir)) %>%
  collect() %>%
  as.data.table()

data_cool_06 <- read.csv(sprintf("%s/06_lm_citygrid_diagnostics.csv", adir)) %>%
  as.data.table()

data_lst_06 <- read.csv(sprintf("%s/06_lst_to_ambient_diagnostics.csv", adir)) %>%
  as.data.table()

tg_obs_city <- read_parquet(sprintf("%s/03_obs_data_tg.gz.parquet", adir)) %>%
  as.data.table()

gc()

# -- figures constants
# desired period
des_per <- 2060
des_per_si <- 2030
des_period <- sort(c(des_per, des_per_si))

# -- create directories
if (!dir.exists('figures/')) dir.create('figures/')
if (!dir.exists('figures/fig_si/')) dir.create('figures/fig_si/')


# -- map value by city & population
fun_map <- function(data_to_plot) {
  pl <- ggplot(data_to_plot) +
    # european map layout
    geom_sf(data = euromap, fill = grey(.9), col = "white", inherit.aes = F) +
    coord_sf(xlim = range(data_to_plot$lon), ylim = range(data_to_plot$lat),
             lims_method = "box", crs = st_crs(euromap), default_crs = st_crs(4326)) +

    # cities
    geom_point(data = data_to_plot, aes(x = lon, y = lat, color = colgrp, fill = colgrp, size = pop),
               shape = 21, stroke = .01, alpha = 1) +

    # # facet
    # facet_wrap(. ~ period) +

    # palette
    scale_fill_manual(values = pal) +
    scale_color_manual(values = pal) +
    scale_size(range = c(1, 7), breaks = c(0.1, 0.5, 3, 7.5) * 10^6,
               labels = ~ scales::number(./10^6)) +

    # legend
    guides(size = guide_legend(override.aes = list(col = 1), order = 2),
           fill = guide_bins(override.aes = list(size = 5), direction = "horizontal", order = 1),
           color = 'none') +

    # theme and layout
    basic_theme +
    theme(axis.text = element_blank())

  return(pl)
}

# -------------------------------------------------------------------------------
# Figure 2: avoided health impacts in MITIG scenario
# -------------------------------------------------------------------------------

if (!dir.exists('figures/subfig2/')) dir.create('figures/subfig2/')

# ----
# -- panel A -- map deaths in 2060
# ----
# subset and add lat-lon data
data <- data_09_period[range == 'heat' & agegroup == 'all' & adapt == 'perc0' &
                         settl_pop == 'total' & metric == 'rate' & climate == 'full' &
                         scen == 'CLIM' & period %in% des_period,] %>%
  # update rate units
  group_by(URAU_CODE, scen, period, settl_temp, settl_pop) %>%
  summarise(est = mean(est) * byrate,
            low = mean(low) * byrate,
            high = mean(high) * byrate) %>%
  ungroup()

data <- merge(data, projdata[year5 %in% des_period & settl_pop == 'total',
                             .(pop = sum(pop)),
                             by = .(period = year5, URAU_CODE, lon, lat, region)],
              by = c("URAU_CODE","period")) %>%
  as.data.table()

plotmap <- data

# cut points for palette
cutpts <- unique(sort(c(0, unname(round(
  quantile(plotmap$est, seq(0, 1, length.out = 20)) / 5) * 5))))
plotmap[, colgrp := cut(est, cutpts)]
signtab <- table(factor(sign(cutpts), c(-1, 0, 1)))

# palettes (fill and border)
npal <- (max(signtab)) * 2
pal <- c(scico(npal, palette = "bam", direction = -1)[
  max(signtab) - signtab[1] + seq_len(signtab[1])],
  scico(tail(signtab, 1), palette = "navia", direction = -1))
bpal <- rep(c("white", "black"), signtab[c("-1", "1")])
names(pal) <- levels(plotmap$colgrp)

mapfig2.1 <- fun_map(plotmap %>%
                       filter(period == des_per)) +

  # labs
  labs(fill = sprintf("Excess death rate (x%s)",
                      formatC(byrate, format = "f", digits = 0, big.mark = ",")),
       size = "Population (in millions)",
       title = 'Premature death estimates under the MITIG scenario',
       x = '', y = '')

mapfig2.1

# save plot
ggsave(paste0("figures/subfig2/fig2.1_deaths_",des_per,".pdf"), plot = mapfig2.1, width = 13, height = 10)
ggsave(paste0("figures/subfig2/fig2.1_deaths_",des_per,".png"), plot = mapfig2.1, width = 13, height = 10)


## SI
mapsi2.1 <- fun_map(plotmap %>%
                       filter(period == des_per_si)) +

  # labs
  labs(fill = sprintf("Excess death rate (x%s)",
                      formatC(byrate, format = "f", digits = 0, big.mark = ",")),
       size = "Population (in millions)",
       title = 'Premature death estimates under the MITIG scenario',
       x = '', y = '')

mapsi2.1

# save plot
ggsave(paste0("figures/fig_si/figSI_2.1_deaths",des_per,".pdf"), plot = mapsi2.1, width = 13, height = 10)
ggsave(paste0("figures/fig_si/figSI_2.1_deaths",des_per,".png"), plot = mapsi2.1, width = 13, height = 10)


## TO PRINT - excess deaths summary
# -- rate (total)
toprint <- data_09_period_global %>%
  filter(period == des_per, range == 'heat', adapt == 'perc0', scenario == 'CLIM', settl_pop == 'total')
print(paste0('Global deaths rate: ',
             as.numeric(toprint[, rate_full_est]*byrate),
             ' [', as.numeric(toprint[, rate_full_low]*byrate),
             ', ', as.numeric(toprint[, rate_full_high]*byrate), ']'))
# [1] "Global deaths rate: 43.297168293214 [0.785048031010714, 163.757931418967]"

# -- an (total)
toprint <- data_09_period[range == 'heat' & agegroup == 'all' & adapt == 'perc0' &
                            settl_pop == 'rural'#urban/rural
                          & metric == 'an' & climate == 'full' &
                            scen == 'CLIM' & period == des_per,]
print(paste0('Global deaths total nº: ',
             as.numeric(round(sum(toprint$est))),
             ' [', as.numeric(round(sum(toprint$low))),
             ', ', as.numeric(round(sum(toprint$high))), ']'))
# [1] "Global deaths city centres nº: 70238 [20870, 154857]"
# [1] "Global deaths outskirts nº: 10024 [2727, 23472]"
# TOTAL: 80262 [23597, 178329]


# -- uhi % increase - perc0
toprint <- data_09_period_global %>%
  filter(period == des_per, range == 'heat', adapt == 'perc0', scenario == 'CLIM', settl_pop != 'total') %>%
  select(period, range, settl_temp, settl_pop, adapt, scenario, starts_with('rate_full')) %>%
  mutate(across(starts_with('rate'), ~ .x * byrate))
e = (toprint[settl_pop == 'urban', rate_full_est] - toprint[settl_pop == 'rural', rate_full_est])/toprint[settl_pop == 'rural', rate_full_est]
l = (toprint[settl_pop == 'urban', rate_full_low] - toprint[settl_pop == 'rural', rate_full_low])/toprint[settl_pop == 'rural', rate_full_low]
h = (toprint[settl_pop == 'urban', rate_full_high] - toprint[settl_pop == 'rural', rate_full_high])/toprint[settl_pop == 'rural', rate_full_high]
print(paste0('Global deaths rate increase in city centres vs outskirts (%): ',
             as.numeric(round(100*e,2)),
             ' [', as.numeric(round(100*l,2)),
             ', ', as.numeric(round(100*h,2)), ']'))
# [1] "Global deaths rate increase in city centres vs outskirts (%): 15.65 [64.01, 11.79]"

# -- uhi % reduction in city centres - perc0 vs perc30
toprint <- data_09_period_global %>%
  filter(period == des_per, range == 'heat', scenario == 'CLIM', settl_pop == 'urban') %>%
  select(period, range, settl_temp, settl_pop, adapt, scenario, starts_with('rate_full')) %>%
  mutate(across(starts_with('rate'), ~ .x * byrate))
e = (toprint[adapt == 'perc30', rate_full_est] - toprint[adapt == 'perc0', rate_full_est])/toprint[adapt == 'perc0', rate_full_est]
l = (toprint[adapt == 'perc30', rate_full_low] - toprint[adapt == 'perc0', rate_full_low])/toprint[adapt == 'perc0', rate_full_low]
h = (toprint[adapt == 'perc30', rate_full_high] - toprint[adapt == 'perc0', rate_full_high])/toprint[adapt == 'perc0', rate_full_high]
print(paste0('Global deaths rate increase in city centres with perc30 vs perc0 (%): ',
             as.numeric(round(100*e,2)),
             ' [', as.numeric(round(100*l,2)),
             ', ', as.numeric(round(100*h,2)), ']'))
# [1] "Global deaths rate increase in city centres with perc30 vs perc0 (%): -26.36 [-48.16, -25.02]"


# -- >85yy % mortality among total
data_toprint <- data_09_period_global_age %>%
  filter(period == des_per, range == 'heat', adapt == 'perc0', scenario == 'CLIM', settl_pop != 'total') %>%
  select(period, range, settl_temp, settl_pop, adapt, agegroup, scenario, starts_with('an_full')) %>%
  group_by(period, range, adapt, agegroup, scenario) %>%
  summarise(across(starts_with('an_full'), ~ sum(.x)), .groups = 'drop')

projdata_global_age <- projdata[settl_pop != 'total', .(
  pop    = sum(pop, na.rm = TRUE),
  death = sum(death, na.rm = TRUE)
), by = .(period=year5, agegroup)]

data_toprint <- data_toprint %>% merge(projdata_global_age, by = c('period', 'agegroup')) %>%
  # aggregate >85 and <85
  mutate(older75 = ifelse(agegroup %in% c('85+'), T, F)) %>%
  group_by(period, range, adapt, older75, scenario) %>%
  summarise(across(starts_with('an_full'), ~ sum(.x)),
            pop = sum(pop),
            death = sum(death),
            .groups = 'drop') %>%
  tidyr::pivot_longer(cols = c('an_full_est','an_full_low','an_full_high'),
                      names_to = 'res', values_to = 'full') %>%
  mutate(res = str_replace(res, 'an_full_',''))
setDT(data_toprint)

toprint <- data_toprint
e = toprint[older75 == T & res == 'est', full]/(toprint[older75 == T & res == 'est', full]+toprint[older75 == F & res == 'est', full])
l = toprint[older75 == T & res == 'low', full]/(toprint[older75 == T & res == 'low', full]+toprint[older75 == F & res == 'low', full])
h = toprint[older75 == T & res == 'high', full]/(toprint[older75 == T & res == 'high', full]+toprint[older75 == F & res == 'high', full])
print(paste0('>75yy representation of the total deaths (%): ',
             as.numeric(round(100*e,2)),
             ' [', as.numeric(round(100*l,2)),
             ', ', as.numeric(round(100*h,2)), ']'))
# [1] ">75yy representation of the total deaths (%): 85.39 [88.93, 85.12]"


# ----
# -- panel B -- map avoided deaths (BASE - CLIM) in 2060
# ----
# subset and add lat-lon data
data <- data_09_period[range == 'heat' & agegroup == 'all' & adapt == 'perc0' &
                         settl_pop == 'total' & metric == 'rate' & climate == 'full' &
                         period %in% des_period,] %>%
  # update rate units
  group_by(URAU_CODE, scen, period, settl_temp, settl_pop) %>%
  summarise(est = mean(est) * byrate) %>%
  ungroup() %>%
  as.data.table()

data <- dcast(data,
              URAU_CODE + period + settl_temp + settl_pop ~ scen,
              value.var = "est")
data$diff <- data$BASE - data$CLIM


data <- merge(data, projdata[year5 %in% des_period & settl_pop == 'total',
                             .(pop = sum(pop)),
                             by = .(period = year5, URAU_CODE, lon, lat, region)],
              by = c("URAU_CODE","period")) %>%
  as.data.table()

plotmap <- data %>%
  filter(period == des_per)

# cut points for palette
cutpts <- unique(sort(c(0, round(min(plotmap$diff)), unname(round(
  quantile(plotmap$diff, seq(0, 1, length.out = 20)) / 5) * 5))))
plotmap[, colgrp := cut(diff, cutpts)]
signtab <- table(factor(sign(cutpts), c(-1, 0, 1)))

# palettes (fill and border)
npal <- (max(signtab)) * 2
pal <- c(rev(scico(head(signtab, 1)*2, palette = "bamako", direction = -1)[
  1:head(signtab, 1)]),
  scico(tail(signtab, 1), palette = "acton", direction = -1))
bpal <- rep(c("white", "black"), signtab[c("-1", "1")])
names(pal) <- levels(plotmap$colgrp)

mapfig2.2 <- fun_map(plotmap) +

  # labs
  labs(fill = sprintf("Avoided death rate (x%s)",
                      formatC(byrate, format = "f", digits = 0, big.mark = ",")),
       size = "Population (in millions)",
       title = 'Avoided premature death estimates (MITIG - BASE)',
       x = '', y = '')

mapfig2.2

# save plot
ggsave(paste0("figures/subfig2/fig2.2_av_deaths_",des_per,".pdf"), plot = mapfig2.2, width = 13, height = 10)
ggsave(paste0("figures/subfig2/fig2.2_av_deaths_",des_per,".png"), plot = mapfig2.2, width = 13, height = 10)


## SI
plotmap <- data %>%
  filter(period == des_per_si)

# cut points for palette
cutpts <- unique(sort(c(0, round(min(plotmap$diff)), unname(round(
  quantile(plotmap$diff, seq(0, 1, length.out = 20)) / 5) * 5))))
plotmap[, colgrp := cut(diff, cutpts)]
signtab <- table(factor(sign(cutpts), c(-1, 0, 1)))

# palettes (fill and border)
npal <- (max(signtab)) * 2
pal <- c(rev(scico(head(signtab, 1)*2, palette = "bamako", direction = -1)[
  1:head(signtab, 1)]),
  scico(tail(signtab, 1), palette = "acton", direction = -1))
bpal <- rep(c("white", "black"), signtab[c("-1", "1")])
names(pal) <- levels(plotmap$colgrp)

mapsi2.2 <- fun_map(plotmap) +

  # labs
  labs(fill = sprintf("Avoided death rate (x%s)",
                      formatC(byrate, format = "f", digits = 0, big.mark = ",")),
       size = "Population (in millions)",
       title = 'Avoided premature death estimates (MITIG - BASE)',
       x = '', y = '')

mapsi2.2

# save plot
ggsave(paste0("figures/fig_si/figSI_2.2_av_deaths",des_per,".pdf"), plot = mapsi2.1, width = 13, height = 10)
ggsave(paste0("figures/fig_si/figSI_2.2_av_deaths",des_per,".png"), plot = mapsi2.1, width = 13, height = 10)


# TO PRINT
# -- MITIG - BASE difference
toprint <- data_09_period_global %>%
  filter(period == des_per, range == 'heat', adapt == 'perc0', settl_pop == 'total') %>%
  select(period, range, settl_temp, settl_pop, adapt, scenario, starts_with('rate_full')) %>%
  mutate(across(starts_with('rate'), ~ .x * byrate))
e = (toprint[scenario == 'BASE', rate_full_est] - toprint[scenario == 'CLIM', rate_full_est])/toprint[scenario == 'BASE', rate_full_est]
l = (toprint[scenario == 'BASE', rate_full_low] - toprint[scenario == 'CLIM', rate_full_low])/toprint[scenario == 'BASE', rate_full_low]
h = (toprint[scenario == 'BASE', rate_full_high] - toprint[scenario == 'CLIM', rate_full_high])/toprint[scenario == 'BASE', rate_full_high]
print(paste0('Global deaths rate difference (MITIG vs BASE) in whole city (%): ',
             as.numeric(round(100*e,2)),
             ' [', as.numeric(round(100*h,2)),
             ', ', as.numeric(round(100*l,2)), ']'))
# [1] "Global deaths rate difference (MITIG vs BASE) in whole city (%): 14.12 [12.57, 64.12]"

toprint <- data_09_period_global %>%
  filter(period == des_per, range == 'heat', adapt == 'perc0', settl_pop == 'total') %>%
  select(period, range, settl_temp, settl_pop, adapt, scenario, starts_with('rate_full')) %>%
  mutate(across(starts_with('rate'), ~ .x * byrate))
e = (toprint[scenario == 'BASE', rate_full_est] - toprint[scenario == 'CLIM', rate_full_est])
l = (toprint[scenario == 'BASE', rate_full_low] - toprint[scenario == 'CLIM', rate_full_low])
h = (toprint[scenario == 'BASE', rate_full_high] - toprint[scenario == 'CLIM', rate_full_high])
print(paste0('Global deaths rate difference (MITIG vs BASE) in whole city (units): ',
             as.numeric(round(e,2)),
             ' [', as.numeric(round(h,2)),
             ', ', as.numeric(round(l,2)), ']'))
# [1] "Global deaths rate difference (MITIG vs BASE) in whole city (units): 6.55 [21.86, 1.25]"

# ----
# -- panel C -- evolution of nº of deaths by region
# ----
# subset and add lat-lon data
data <- data_09_period[range == 'heat' & agegroup == 'all' & adapt == 'perc0' &
                         settl_pop == 'total' & metric == 'an' & climate == 'full' &
                         scen == 'CLIM',] %>%
  # add cities info
  merge(projdata[settl_pop == 'total',
                 .(pop = sum(pop)),
                 by = .(period = year5, URAU_CODE, lon, lat, region)],
        by = c("URAU_CODE","period")) %>%
  # compute rate
  pivot_longer(cols = c('est','low','high'), names_to = 'res', values_to = 'value') %>%
  group_by(region, res, scen, period, settl_temp, settl_pop) %>%
  summarise(value = (sum(value) / sum(pop)) * byrate) %>%
  ungroup() %>%
  # reshape
  pivot_wider(names_from = 'res', values_from = 'value') %>%
  mutate(region = factor(region, order_regions))

plotmap <- data

mapfig2.3 <- ggplot(plotmap,
                    aes(x = period, y = est, fill = region)) +

  # estimates by region
  geom_area(alpha = 0.8, position = "stack") +

  # palettes
  scale_fill_manual(values = pal_color_regions) +

  # labs
  labs(fill = 'Region',
       title = 'Evolution of premature death estimates under the MITIG scenario by region',
       x = '',
       y = sprintf("Excess death rate (x%s)",
                   formatC(byrate, format = "f", digits = 0, big.mark = ","))) +

  # theme and layout
  basic_theme +
  xlim(min(plotmap$period),max(plotmap$period))

mapfig2.3

# save plot
ggsave(paste0("figures/subfig2/fig2.3_num_deaths_evolution_CLIM_region.pdf"), plot = mapfig2.3, width = 13, height = 5)
ggsave(paste0("figures/subfig2/fig2.3_num_deaths_evolution_CLIM_region.png"), plot = mapfig2.3, width = 13, height = 5)


## TO PRINT -- temporal evolution
toprint <- plotmap %>%
  group_by(scen, period) %>%
  summarise(est = sum(est),
            high = sum(high),
            low = sum(low)) %>%
  ungroup() %>%
  as.data.table()
toprint_2020 <- toprint[period == '2020']
toprint_2060 <- toprint[period == '2060']
print(paste0(
  "Temporal evolution from 2020 to 2060 rate increase: ",
  round((toprint_2060$est - toprint_2020$est),2), " [",
  round((toprint_2060$low - toprint_2020$low),2), ", ",
  round((toprint_2060$high - toprint_2020$high),2), "]"))
# [1] "Temporal evolution from 2020 to 2060 rate increase: 94.07 [23.91, 203.91]"


## SI --
data <- data_09_period[range == 'heat' & agegroup == 'all' & adapt == 'perc0' &
                         settl_pop == 'total' & metric == 'an' & climate == 'full',] %>%
  # add cities info
  merge(projdata[settl_pop == 'total',
                 .(pop = sum(pop)),
                 by = .(period = year5, URAU_CODE, lon, lat, region)],
        by = c("URAU_CODE","period")) %>%
  # compute rate
  pivot_longer(cols = c('est','low','high'), names_to = 'res', values_to = 'value') %>%
  group_by(region, res, scen, period, settl_temp, settl_pop) %>%
  summarise(value = (sum(value) / sum(pop)) * byrate) %>%
  ungroup() %>%
  # reshape
  pivot_wider(names_from = 'res', values_from = 'value') %>%
  mutate(region = factor(region, order_regions))

plotmap <- data

legend_breaks <- c('Northern.BASE', 'Southern.BASE', 'Eastern.BASE', 'Western.BASE',
                   'Northern.CLIM', 'Southern.CLIM', 'Eastern.CLIM', 'Western.CLIM')
mapsi2.3 <- ggplot(plotmap,
                   aes(x = period, y = est, color = interaction(region,scen))) +

  # CI
  geom_ribbon(aes(x = period, ymin = low, ymax = high, fill = interaction(region,scen)),
              alpha = 0.2, show.legend = F) +

  # estimates by region
  geom_line(linewidth = 1) +

  # facet
  facet_wrap(. ~ region) +

  # palettes
  scale_color_manual(values = pal_color_regions_scen,
                     labels = pal_labels_regions_scen,
                     breaks = legend_breaks) +
  scale_fill_manual(values = pal_color_regions_scen,
                    labels = pal_labels_regions_scen,
                    breaks = legend_breaks) +

  # labs
  labs(color = 'Region & Scenario',
       # title = 'Evolution of premature death estimates by scenario by region',
       x = '',
       y = sprintf("Excess death rate (x%s)",
                   formatC(byrate, format = "f", digits = 0, big.mark = ","))) +

  # theme and layout
  basic_theme +
  xlim(min(plotmap$period),max(plotmap$period))

mapsi2.3

# save plot
ggsave(paste0("figures/fig_si/figSI_2.3_deaths_evolution.pdf"), plot = mapsi2.3, width = 13, height = 10)
ggsave(paste0("figures/fig_si/figSI_2.3_deaths_evolution.png"), plot = mapsi2.3, width = 13, height = 10)


## TO PRINT -- temporal evolution
toprint <- data_09_period_global[range == 'heat' & scenario == 'CLIM' &
                                   adapt == 'perc0' & settl_pop == 'total']

toprint_2020 <- toprint[period == '2020']
toprint_2060 <- toprint[period == '2060']
print(paste0(
  "Temporal evolution from 2020 to 2060 rate increase: ",
  round(100*(toprint_2060$rate_full_est - toprint_2020$rate_full_est)/toprint_2020$rate_full_est,2), "% [",
  round(100*(toprint_2060$rate_full_low - toprint_2020$rate_full_low)/toprint_2020$rate_full_low,2), "%, ",
  round(100*(toprint_2060$rate_full_high - toprint_2020$rate_full_high)/toprint_2020$rate_full_high,2), "%]"))
# [1] "Temporal evolution from 2020 to 2060 rate increase: 155.21% [311.85%, 155.75%]"

# ----
# -- FIG 2 -- composition
# ----
clean_mapfig2.1 <- mapfig2.1 +
  labs(title = '')
clean_mapfig2.2 <- mapfig2.2 +
  labs(title = '')
clean_mapfig2.3 <- plot_grid(
  NULL,
  mapfig2.3 +
    labs(title = ''),
  NULL,
  ncol = 3,
  rel_widths = c(0.025, 0.95, 0.025) # 0.75 width centered (0.125 padding on each side)
)

fig2_ab <- plot_grid(
  clean_mapfig2.1,
  clean_mapfig2.2,
  ncol = 2,
  labels = c('b)', 'c)'),
  rel_widths = c(1, 1),
  label_size = 14
)
fig2 <- plot_grid(
  clean_mapfig2.3,
  fig2_ab,
  ncol = 1,
  rel_heights = c(1, 1),
  labels = c('a)', ''),
  label_size = 14
)

ggsave(paste0("figures/fig2.pdf"), plot = fig2, width = 10, height = 10)
ggsave(paste0("figures/fig2.png"), plot = fig2, width = 10, height = 10)

## SI
clean_mapsi2.1 <- mapsi2.1 +
  labs(title = '')
clean_mapsi2.2 <- mapsi2.2 +
  labs(title = '')

fig2_si <- plot_grid(
  clean_mapsi2.1,
  clean_mapsi2.2,
  ncol = 1,
  labels = c('a)', 'b)'),
  rel_heights = c(1, 1),
  label_size = 14
)

ggsave(paste0("figures/fig_si/figSI_2.pdf"), plot = fig2_si, width = 10, height = 10)
ggsave(paste0("figures/fig_si/figSI_2.png"), plot = fig2_si, width = 10, height = 10)


# -------------------------------------------------------------------------------
# Figure 3: deaths due to UHI
# -------------------------------------------------------------------------------

if (!dir.exists('figures/subfig3/')) dir.create('figures/subfig3/')

# ----
# -- panel A -- temperature evolution by settl
# ----

# subset and add cities info
dataa <- dataa_08 %>%
  filter(adapt == 'perc0') %>%
  merge(unique(projdata[, .(URAU_CODE, region, cntr_name)]),
        by = c("URAU_CODE")) %>%
  mutate(calperiod = fct_recode(calperiod, "1990-2019" = "hist"))
# order regions & settlment types
data <- dataa %>%
  mutate(settl = factor(settl, levels = c('urban','rural')),
         region = factor(region, levels = order_regions))


plotmap <- data

mapfig3.1 <- ggplot(plotmap, aes(x = tas,
                                 color = region,
                                 fill = region,
                                 linetype = settl)) +

  geom_density(linewidth = 0.65, alpha = 0.4, key_glyph = "timeseries") +

  # palette
  scale_color_manual(values = pal_color_regions,
                     labels = pal_labels_regions) +
  scale_fill_manual(values = pal_color_regions,
                    labels = pal_labels_regions) +
  scale_linetype_manual(values = pal_linetype_settl,
                        labels = pal_labels_settl) +
  guides(color = guide_legend(override.aes = list(linewidth = 1, alpha = 1)),
         linetype = guide_legend(override.aes = list(linewidth = 1, fill = NA))) +

  # labs
  labs(color = 'Region', fill = 'Region', linetype = 'Exposure area',
       # title = 'Temperature evolution by exposure area',
       y = "Density", x = 'Temperature (ºC)') +

  # theme and layout
  basic_theme + legend_justif_theme +
  theme(legend.key.width = unit(1.2, "cm"))

mapfig3.1

# save
ggsave(paste0("figures/subfig3/fig3.1_temperature_evolution.pdf"), plot = mapfig3.1, width = 13, height = 8)
ggsave(paste0("figures/subfig3/fig3.1_temperature_evolution.png"), plot = mapfig3.1, width = 13, height = 8)


# si -- distrib plot
plotmap1 = plotmap[, `:=`(medi = quantile(tas, 0.5)),
                   by=c('region','calperiod','settl')]

mapsi1 <- ggplot(plotmap1) +

  # density plot
  geom_density(aes(x = tas, color = region, fill = region, linetype = settl),
               linewidth = 0.65, alpha = 0.4, key_glyph = "timeseries") +

  # median line
  geom_vline(aes(xintercept = medi, color = region, linetype = settl),
             key_glyph = "timeseries") +

  # facet
  facet_grid(calperiod ~ scen,
             labeller = labeller(scen = as_labeller(pal_labels_scen))) +

  # palette
  scale_color_manual(values = pal_color_regions,
                     labels = pal_labels_regions) +
  scale_fill_manual(values = pal_color_regions,
                    labels = pal_labels_regions) +
  scale_linetype_manual(values = pal_linetype_settl,
                        labels = pal_labels_settl) +
  guides(color = guide_legend(override.aes = list(linewidth = 1, alpha = 1)),
         linetype = guide_legend(override.aes = list(linewidth = 1, fill = NA))) +

  # labs
  labs(color = 'Region', fill = 'Region', linetype = 'Exposure area',
       # title = 'Temperature evolution by exposure area',
       y = "Density", x = 'Temperature (ºC)') +

  # theme and layout
  basic_theme + legend_justif_theme +
  theme(legend.key.width = unit(1.2, "cm"))

# mapsi1

# save
ggsave(paste0("figures/fig_si/figSI_3.1.1_temperature_evolution.pdf"), plot = mapsi1, width = 13, height = 14)
ggsave(paste0("figures/fig_si/figSI_3.1.1_temperature_evolution.png"), plot = mapsi1, width = 13, height = 14)


# si -- cumm plot
library(plyr)
plotmap2_mean <- plyr::ddply(plotmap, .(region,calperiod,settl,scen), summarize,
                             tas = unique(tas),
                             ecdf = ecdf(tas)(unique(tas)))
plotmap2_bygcm <- plyr::ddply(plotmap, .(region,calperiod,settl,scen,gcm), summarize,
                              tas = unique(tas),
                              ecdf = ecdf(tas)(unique(tas)))
detach("package:plyr", unload = TRUE)

legend_breaks <- c('Northern.BASE', 'Southern.BASE', 'Eastern.BASE', 'Western.BASE',
                   'Northern.CLIM', 'Southern.CLIM', 'Eastern.CLIM', 'Western.CLIM')
mapsi2 <- ggplot(plotmap2_mean) +

  # cumm plot by gcm
  geom_line(data = plotmap2_bygcm,
            aes(x = tas, y = ecdf, color = interaction(region,scen,'bg'), linetype = settl),
            linewidth = 0.65, key_glyph = "timeseries", show.legend = F) +

  # cumm plot mean
  geom_line(data = plotmap2_mean,
            aes(x = tas, y = ecdf, color = interaction(region,scen), linetype = settl),
            linewidth = 0.65, key_glyph = "timeseries") +

  # facet
  facet_grid(calperiod ~ region) +

  # palette
  scale_color_manual(values = pal_color_regions_scen,
                     labels = pal_labels_regions_scen,
                     breaks = legend_breaks) +
  scale_linetype_manual(values = pal_linetype_settl,
                        labels = pal_labels_settl) +
  guides(color = guide_legend(override.aes = list(linewidth = 1, alpha = 1), nrow = 2),
         linetype = guide_legend(override.aes = list(linewidth = 1, fill = NA))) +

  # labs
  labs(color = 'Region & Scenario', linetype = 'Exposure area',
       # title = 'Temperature evolution by exposure area',
       y = "Probability", x = 'Temperature (ºC)') +

  # theme and layout
  basic_theme + legend_justif_theme +
  theme(legend.key.width = unit(1.2, "cm"))

# mapsi2

# save
ggsave(paste0("figures/fig_si/figSI_3.1.2_temperature_evolution.pdf"), plot = mapsi2, width = 13, height = 14)
ggsave(paste0("figures/fig_si/figSI_3.1.2_temperature_evolution.png"), plot = mapsi2, width = 13, height = 14)



# ----
# -- panel B -- columns by country to depict deaths in 2060 by settl
# ----
# subset and add lat-lon data
data_an <- data_09_period[range == 'heat' & agegroup == 'all' & adapt == 'perc0' &
                            metric == 'an' & climate == 'full' &
                            scen == 'CLIM' & period %in% des_period &
                            !URAU_CODE %in% c('CY002C'),] %>%
  # reshape
  pivot_longer(cols = c('est','low','high'), names_to = 'res', values_to = 'value') %>%
  # mean estimate across MC
  group_by(URAU_CODE = URAU_CODE, scen, res, period, settl_temp, settl_pop) %>%
  summarise(value = mean(value)) %>%
  ungroup() %>%
  # add cities info
  merge(projdata[year5 %in% des_period,
                 .(pop = round(sum(pop*settl_pop_perc))),
                 by = .(period = year5, URAU_CODE, region, cntr_name, settl_pop)],
        by = c("URAU_CODE","period","settl_pop")) %>%
  as.data.table()
# count nº of cities by country
data_an[, cities_by_cntr := uniqueN(URAU_CODE), by = .(cntr_name)]
data_an[, cntr_name_full := paste0(cntr_name, " (", cities_by_cntr, ")")]

plotmap <- data_an %>%
  filter(period == des_per) %>%
  select(-settl_temp) %>%
  as.data.table()
# set settl_pop == 'total' as 'counterfactual' independent column
plotmap[, ":="(counterfactual_deaths = value[settl_pop == "total"],
               total_pop = pop[settl_pop == "total"]),
        by = .(URAU_CODE, period, scen, res, region, cntr_name, cntr_name_full)]
plotmap <- plotmap[settl_pop != "total"]
# estimate average deaths by country
plotmap <- plotmap %>%
  group_by(cntr_name, cntr_name_full, region, period, scen, res, settl_pop) %>%
  summarise(deaths = mean(value),
            counterfactual_deaths = mean(counterfactual_deaths),
            total_pop = sum(total_pop),
            pop = sum(pop)) %>%
  ungroup() %>%
  # order ctries alphabetically & settlement types & regions
  mutate(cntr_name_full = factor(cntr_name_full, levels = rev(sort(as.character(unique(cntr_name_full))))),
         settl_pop = factor(settl_pop, levels = c('urban','rural')),
         region = factor(region, levels = order_regions)) %>%
  as.data.table()

# reshape to plot
plotmap_ci <- dcast(plotmap,
                    cntr_name_full + region + period + scen + settl_pop ~ res,
                    value.var = "deaths")
plotmap_ci[, settl_pop_dark := paste0(settl_pop, "_dark")]

plotmap_ctrf <- dcast(plotmap[settl_pop == "urban"],
                      cntr_name_full + region + period + scen + settl_pop ~ res,
                      value.var = "counterfactual_deaths")


mapfig3.2 <- ggplot() +

  # estimate by settl type
  geom_col(data = plotmap_ci,
           aes(x = est, y = cntr_name_full, fill = settl_pop),
           width = 0.4,
           position = position_stacknudge(y = +0.2)) +

  # CI by settl type
  geom_errorbar(data = plotmap_ci,
                aes(y = cntr_name_full, xmin = low, xmax = high, color = settl_pop_dark),
                width = 0.2, alpha = 0.7,
                position = position_nudge(y = +0.2)) +

  # counterfactual
  geom_col_pattern(
    data = subset(plotmap_ctrf, settl_pop == "urban") %>%
      mutate(settl_pop = 'total'),
    aes(x = est, y = cntr_name_full, fill = settl_pop, pattern = ' '),
    position = position_nudge(y = -0.15),

    # pattern
    pattern_fill = "gray20",
    pattern_angle = 45,
    pattern_density = 0.1,
    pattern_spacing = 0.02,
    pattern_alpha = 0.4,

    # col
    linewidth = 0.5,
    width = 0.3,
    alpha = 0.5,
    # color = 'gray20'
  ) +

  # CI of counterfactual
  geom_errorbar(data = plotmap_ctrf %>%
                  mutate(settl_pop = 'total'),
                aes(y = cntr_name_full, xmin = low, xmax = high, color = settl_pop),
                position = position_nudge(y = -0.15),
                width = 0.1, color = "gray30") +

  # facet
  facet_grid(region ~ ., scales = "free_y", space = "free_y") +

  # palette
  scale_fill_manual(values = pal_color_settl,
                    labels = pal_labels_settl) +
  scale_pattern_manual(values = c(" " = "stripe")) +
  scale_color_manual(values = pal_color_settl_dark) +

  # labs
  labs(pattern = 'Counterfactual',
       fill = 'Exposure Area',
       title = paste0('Premature death estimates under the MITIG scenario in ',des_per,' due to UHI'),
       x = "Average attributable deaths number by country", y = '') +
  guides(color = "none",
         fill = guide_legend(override.aes = list(color = NA, pattern = 'none')),
         pattern = guide_legend(override.aes = list(color = NA))) +

  # theme and layout
  basic_theme + legend_justif_theme +
  theme(legend.key.width = unit(0.4, "cm"),
        legend.key.height = unit(0.4, "cm"),
        legend.box.margin = margin(t = 2, l = -40, unit = "pt"),
        legend.spacing.y = unit(0.1, "pt")
  )

mapfig3.2

# save
ggsave(paste0("figures/subfig3/fig3.2_deaths_",des_per,"_bars_UHI.pdf"), plot = mapfig3.2, width = 13, height = 8)
ggsave(paste0("figures/subfig3/fig3.2_deaths_",des_per,"_bars_UHI.png"), plot = mapfig3.2, width = 13, height = 8)


# ----
# -- panel C -- map deaths PER in 2060
# ----
# subset and add lat-lon data
data <- data_09_period[range == 'heat' & agegroup == 'all' & adapt == 'perc0' &
                         metric == 'an' & climate == 'full' &
                         scen == 'CLIM' & period %in% des_period,] %>%
  # update rate units
  group_by(URAU_CODE, scen, period, settl_temp, settl_pop) %>%
  summarise(est = mean(est)) %>%
  ungroup() %>%
  as.data.table()

plotmap <- data %>%
  # compute UHI effect (urban - rural)
  filter(settl_pop != 'total') %>%
  select(-settl_temp) %>%
  pivot_wider(names_from = 'settl_pop', values_from = 'est') %>%
  mutate(diff_per = 100*urban/(urban+rural)) %>%
  # add cities info
  merge(projdata[year5 %in% des_period & settl_pop == 'total',
                 .(pop = sum(pop)),
                 by = .(period = year5, URAU_CODE, lon, lat, region)],
        by = c("URAU_CODE","period")) %>%
  as.data.table()


# cut points for palette - percentage difference
plotmap_clean <- plotmap %>%
  filter(!is.infinite(diff_per),
         !is.na(diff_per))
cutpts <- unique(sort(c(0, unname(round(
  quantile(plotmap_clean$diff_per, seq(0, 1, length.out = 20)) / 5) * 5))))
plotmap_clean[, colgrp := cut(diff_per, cutpts)]
signtab <- table(factor(sign(cutpts), c(-1, 0, 1)))

# palettes (fill and border) - percentage difference
npal <- (max(signtab)) * 2
pal <- c(scico(npal, palette = "bam", direction = -1)[
  max(signtab) - signtab[1] + seq_len(signtab[1])],
  scico(tail(signtab, 1), palette = "navia", direction = -1))
bpal <- rep(c("white", "black"), signtab[c("-1", "1")])
names(pal) <- levels(plotmap_clean$colgrp)


# plot des_per - percentage difference
mapfig3.3 <- fun_map(plotmap_clean %>%
                       filter(period == des_per)) +
  # labs
  labs(fill = "City-centre share of\ntotal urban deaths (%)",
       size = "Population (in millions)",
       title = 'Premature death estimates under the MITIG scenario due to UHI',
       x = '', y = '')

# save plot
ggsave(paste0("figures/subfig3/fig3.3_deaths_uhi_per_",des_per,".pdf"), plot = mapfig3.3, width = 13, height = 10)
ggsave(paste0("figures/subfig3/fig3.3_deaths_uhi_per_",des_per,".png"), plot = mapfig3.3, width = 13, height = 10)

# plot des_per_si - percentage difference
mapsi <- fun_map(plotmap_clean %>%
                   filter(period == des_per_si)) +
  # labs
  labs(fill = sprintf("Excess death rate (x%s)",
                      formatC(byrate, format = "f", digits = 0, big.mark = ",")),
       size = "Population (in millions)",
       title = 'Premature death estimates under the MITIG scenario due to UHI',
       x = '', y = '')

# save plot
ggsave(paste0("figures/fig_si/figSI_3.3_deaths_uhi_per_",des_per_si,".pdf"), plot = mapsi, width = 13, height = 10)
ggsave(paste0("figures/fig_si/figSI_3.3_deaths_uhi_per_",des_per_si,".png"), plot = mapsi, width = 13, height = 10)


# TO PRINT - Global values
data_09_period_global %>% filter(range == 'heat',period == des_per,
                                 scenario == 'CLIM',adapt == 'perc0') -> toprint
# 1. UHI % increment vs counterfactual
ctf <- toprint[settl_pop == 'total', .(rate_full_est)] * byrate
uhi <- toprint[settl_temp == 'urban', .(rate_full_est)] * byrate
ctf_l <- toprint[settl_pop == 'total', .(rate_full_low)] * byrate
uhi_l <- toprint[settl_temp == 'urban', .(rate_full_low)] * byrate
ctf_h <- toprint[settl_pop == 'total', .(rate_full_high)] * byrate
uhi_h <- toprint[settl_temp == 'urban', .(rate_full_high)] * byrate
print(paste0("UHI % increment vs counterfactual: ",
             round(as.numeric(100*(uhi-ctf)/ctf),2), ' [',
             round(as.numeric(100*(uhi_l-ctf_l)/ctf_l),2), ',',
             round(as.numeric(100*(uhi_h-ctf_h)/ctf_h),2),']'))
# [1] "UHI % increment vs counterfactual: 14.78 [56.86,11.17]"

# 2. City centres vs Outskirts mortality %
out <- toprint[settl_pop == 'rural', .(an_full_est)]
cic <- toprint[settl_temp == 'urban', .(an_full_est)]
out_l <- toprint[settl_pop == 'rural', .(an_full_low)]
cic_l <- toprint[settl_temp == 'urban', .(an_full_low)]
out_h <- toprint[settl_pop == 'rural', .(an_full_high)]
cic_h <- toprint[settl_temp == 'urban', .(an_full_high)]
print(paste0("UHI % increment vs counterfactual: ",
             round(as.numeric(100*cic/(cic+out)),2), ' [',
             round(as.numeric(100*cic_l/(cic_l+out_l)),2), ',',
             round(as.numeric(100*cic_h/(cic_h+out_h)),2),']'))


# ----
# -- panel C -- map deaths ABS in 2060
# ----
# subset and add lat-lon data
data <- data_09_period[range == 'heat' & agegroup == 'all' & adapt == 'perc0' &
                         metric == 'rate' & climate == 'full' &
                         scen == 'CLIM' & period %in% des_period,] %>%
  # update rate units
  group_by(URAU_CODE, scen, period, settl_temp, settl_pop) %>%
  summarise(est = mean(est) * byrate) %>%
  ungroup() %>%
  as.data.table()

plotmap <- data %>%
  # compute UHI effect (urban - rural)
  filter(settl_pop != 'total') %>%
  select(-settl_temp) %>%
  pivot_wider(names_from = 'settl_pop', values_from = 'est') %>%
  mutate(diff_abs = urban - rural) %>%
  # add cities info
  merge(projdata[year5 %in% des_period & settl_pop == 'total',
                 .(pop = sum(pop)),
                 by = .(period = year5, URAU_CODE, lon, lat, region)],
        by = c("URAU_CODE","period")) %>%
  as.data.table()

# cut points for palette - absolute difference
cutpts <- unique(sort(c(0, unname(round(
  quantile(plotmap$diff_abs, seq(0, 1, length.out = 20)) / 5) * 5))))
plotmap[, colgrp := cut(diff_abs, cutpts)]
signtab <- table(factor(sign(cutpts), c(-1, 0, 1)))

# palettes (fill and border) - absolute difference
npal <- (max(signtab)) * 2
pal <- c(scico(npal, palette = "bam", direction = -1)[
  max(signtab) - signtab[1] + seq_len(signtab[1])],
  scico(tail(signtab, 1), palette = "navia", direction = -1))
bpal <- rep(c("white", "black"), signtab[c("-1", "1")])
names(pal) <- levels(plotmap$colgrp)

# plot des_per - absolute difference
mapsi <- fun_map(plotmap %>%
                   filter(period == des_per)) +
  # labs
  labs(fill = sprintf("Excess death rate (x%s)",
                      formatC(byrate, format = "f", digits = 0, big.mark = ",")),
       size = "Population (in millions)",
       title = 'Premature death estimates under the MITIG scenario due to UHI',
       x = '', y = '')

# save plot
ggsave(paste0("figures/subfig3/fig3.3_deaths_uhi_abs_",des_per,".pdf"), plot = mapsi, width = 13, height = 10)
ggsave(paste0("figures/subfig3/fig3.3_deaths_uhi_abs_",des_per,".png"), plot = mapsi, width = 13, height = 10)

# plot des_per_si - absolute difference
mapsi <- fun_map(plotmap %>%
                   filter(period == des_per_si)) +
  # labs
  labs(fill = sprintf("Excess death rate (x%s)",
                      formatC(byrate, format = "f", digits = 0, big.mark = ",")),
       size = "Population (in millions)",
       title = 'Premature death estimates under the MITIG scenario due to UHI',
       x = '', y = '')

# save plot
ggsave(paste0("figures/fig_si/figSI_3.3_deaths_uhi_abs_",des_per_si,".pdf"), plot = mapsi, width = 13, height = 10)
ggsave(paste0("figures/fig_si/figSI_3.3_deaths_uhi_abs_",des_per_si,".png"), plot = mapsi, width = 13, height = 10)


# ----
# -- FIG 3 -- composition
# ----
clean_mapfig3.1 <- mapfig3.1 +
  labs(title = '') +
  legend_justif_theme +
  theme(legend.margin = margin(l = -10, unit = "pt"),
        legend.spacing.y = unit(2, "pt"))
clean_mapfig3.2 <- mapfig3.2 +
  labs(title = '')
clean_mapfig3.3 <- mapfig3.3 +
  labs(title = '')

fig3 <- plot_grid(
  clean_mapfig3.1,
  clean_mapfig3.3,
  ncol = 1,
  labels = c('a)', 'b)'),
  rel_heights = c(1, 1.2),
  label_size = 14
)
fig3 <- plot_grid(
  fig3,
  clean_mapfig3.2,
  ncol = 2,
  labels = c('', 'c)'),
  rel_widths = c(1, 1),
  label_size = 14
)

ggsave(paste0("figures/fig3.pdf"), plot = fig3, width = 10, height = 10)
ggsave(paste0("figures/fig3.png"), plot = fig3, width = 10, height = 10)



# -------------------------------------------------------------------------------
# Figure 4: adaptation
# -------------------------------------------------------------------------------

if (!dir.exists('figures/subfig4/')) dir.create('figures/subfig4/')

# ----
# -- panel A -- map cooling in 2060
# ----
# annual average by country
data <- dataa_08[scen == 'CLIM' & settl == 'urban',] %>%
  # add cities info
  merge(unique(projdata[, .(URAU_CODE, lat, lon, region, cntr_name)]),
        by = c("URAU_CODE"))
# annual average by city & period
data <- data[, .(tas = mean(tas)),
             by = .(gcm, scen, calperiod, year5, adapt, settl, region, cntr_name, URAU_CODE, lat, lon)]
# compute difference of urban temperature by adapt
data <- dcast(data,
              URAU_CODE + lat + lon + gcm + scen + calperiod + year5 + settl + region + cntr_name ~ adapt,
              value.var = "tas")
data[, diff_tas := perc30 - perc0]


plotmap <- setDT(data)[rowSums(is.na(data)) == 0,]
plotmap <- plotmap %>%
  filter(year5 == des_per)

# cut points for palette
cutpts <- unique(sort(c(0, unname(round(
  quantile(plotmap$diff_tas, seq(0, 1, length.out = 10)) / 5, 2) * 5))))
plotmap[, colgrp := cut(diff_tas, cutpts)]
signtab <- table(factor(sign(cutpts), c(-1, 0, 1)))

# palettes (fill and border)
npal <- (max(signtab)) * 2
if (tail(signtab, 1) > 0) {
  pal <- c(scico(npal, palette = "devon", direction = 1)[
    max(signtab) - signtab[1] + seq_len(signtab[1]) - 1],
    scico(tail(signtab, 1), palette = "acton", direction = -1))
} else {
  pal <- scico(head(signtab, 1), palette = "imo", direction = 1)
}
bpal <- rep(c("white", "black"), signtab[c("-1", "1")])

data_to_plot <- plotmap
mapfig4.1 <- ggplot() +
  # european map layout
  geom_sf(data = euromap, fill = grey(.9), col = "white", inherit.aes = F) +
  coord_sf(xlim = range(data_to_plot$lon), ylim = range(data_to_plot$lat),
           lims_method = "box", crs = st_crs(euromap), default_crs = st_crs(4326)) +

  # cities
  geom_point(data = data_to_plot,
             aes(x = lon, y = lat, fill = diff_tas, color = diff_tas),
             shape = 21, stroke = .01, alpha = 1, size = 2) +

  # palette
  scale_fill_gradientn(colors = pal,
                       breaks = cutpts[2:(length(cutpts)-1)],
                       limits = c(min(cutpts[2]-0.15), max(cutpts[length(cutpts)-1]+0.15)),
                       oob = scales::squish,
                       labels = function(x) ifelse(x == 0, "0", format(x, nsmall = 2))) +
  scale_color_gradientn(colors = pal,
                        breaks = cutpts[2:(length(cutpts)-1)],
                        limits = c(min(cutpts[2]-0.15), max(cutpts[length(cutpts)-1]+0.15)),
                        oob = scales::squish,
                        labels = function(x) ifelse(x == 0, "0", format(x, nsmall = 2))) +

  # theme and layout
  basic_theme +
  theme(axis.text = element_blank(),
        legend.text = element_text(angle = 45, vjust = 1, hjust = 1)) +

  # labs
  labs(fill = "Mean cooling with\n30% tree coverage (ºC)",
       title = 'Temperature reduction by 30% tree coverage under the MITIG scenario',
       x = '', y = '') +
  guides(size = 'none', color = 'none',
         fill = guide_colorbar(barwidth = 12.5))

mapfig4.1

# save
ggsave(paste0("figures/subfig4/fig4.1_reduced_temp_",des_per,".pdf"), plot = mapfig4.1 +
         theme(legend.key.width = unit(1, "cm")), width = 13, height = 10)
ggsave(paste0("figures/subfig4/fig4.1_reduced_temp_",des_per,".png"), plot = mapfig4.1 +
         theme(legend.key.width = unit(1, "cm")), width = 13, height = 10)


## TO PRINT -- cooling potential city centre
toprint <- setDT(plotmap)[year5 == des_per & settl == 'urban', .(
  perc0_p50  = mean(perc0, na.rm = TRUE),
  perc0_p2.5  = quantile(perc0, probs = 0.025, na.rm = TRUE),
  perc0_p97.5 = quantile(perc0, probs = 0.975, na.rm = TRUE),

  perc30_p50  = mean(perc30, na.rm = TRUE),
  perc30_p2.5  = quantile(perc30, probs = 0.025, na.rm = TRUE),
  perc30_p97.5 = quantile(perc30, probs = 0.975, na.rm = TRUE)
)]
print(paste0('Cooling degrees: ',
             round(toprint$perc0_p50 - toprint$perc30_p50,2), ' [',
             round(toprint$perc0_p2.5 - toprint$perc30_p2.5,2),', ',
             round(toprint$perc0_p97.5 - toprint$perc30_p97.5,2), ']'))
# [1] "Cooling degrees: 0.45 [0.29, 0.78]"


## SI - map cooling potential
plotmap <- setDT(data)[rowSums(is.na(data)) == 0,]
plotmap[, diff_tas_30 := perc30 - perc0]
plotmap[, diff_tas_35 := perc35 - perc0]
plotmap[, diff_tas_25 := perc25 - perc0]

plotmap <- melt(plotmap,
                id.vars = c('URAU_CODE', 'lat', 'lon', 'gcm', 'scen', 'year5',
                            'calperiod', 'region'),
                measure.vars = c("diff_tas_30", "diff_tas_35", "diff_tas_25"),
                variable.name = "adapt_th",
                value.name = "value")
plotmap[, adapt_th := gsub("diff_tas_", "perc", adapt_th)]
plotmap <- plotmap %>%
  mutate(adapt_th = factor(adapt_th, levels = sort(unique(adapt_th))))

# cut points for palette
cutpts <- unique(sort(c(0, unname(round(
  quantile(plotmap$value, seq(0, 1, length.out = 10)) / 5, 2) * 5))))
plotmap[, colgrp := cut(value, cutpts)]
signtab <- table(factor(sign(cutpts), c(-1, 0, 1)))

# palettes (fill and border)
npal <- (max(signtab)) * 2
if (tail(signtab, 1) > 0) {
  pal <- c(scico(npal, palette = "devon", direction = 1)[
    max(signtab) - signtab[1] + seq_len(signtab[1]) - 1],
    scico(tail(signtab, 1), palette = "acton", direction = -1))
} else {
  pal <- scico(head(signtab, 1), palette = "imo", direction = 1)
}
bpal <- rep(c("white", "black"), signtab[c("-1", "1")])


data_to_plot <- plotmap %>%
  filter(year5 %in% des_per)
mapsi_4.1.1 <- ggplot() +
  # european map layout
  geom_sf(data = euromap, fill = grey(.9), col = "white", inherit.aes = F) +
  coord_sf(xlim = range(data_to_plot$lon), ylim = range(data_to_plot$lat),
           lims_method = "box", crs = st_crs(euromap), default_crs = st_crs(4326)) +

  # cities
  geom_point(data = data_to_plot,
             aes(x = lon, y = lat, fill = value, color = value),
             shape = 21, stroke = .01, alpha = 1, size = 2) +

  # facet
  facet_grid(. ~ adapt_th,
             switch = "y",
             labeller = labeller(adapt_th = as_labeller(pal_labels_adapt_sens))) +

  # palette
  scale_fill_gradientn(colors = pal,
                       breaks = cutpts[2:(length(cutpts)-1)],
                       limits = c(min(cutpts[2]-0.15), max(cutpts[length(cutpts)-1]+0.15)),
                       oob = scales::squish,
                       labels = function(x) ifelse(x == 0, "0", format(x, nsmall = 2))) +
  scale_color_gradientn(colors = pal,
                        breaks = cutpts[2:(length(cutpts)-1)],
                        limits = c(min(cutpts[2]-0.15), max(cutpts[length(cutpts)-1]+0.15)),
                        oob = scales::squish,
                        labels = function(x) ifelse(x == 0, "0", format(x, nsmall = 2))) +

  # theme and layout
  basic_theme +
  theme(axis.text = element_blank(),
        legend.text = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.key.width = unit(1, "cm")) +

  # labs
  labs(fill = "Mean cooling through tree coverage increase (ºC)\n",
       # title = 'Temperature reduction by 30% tree coverage under the MITIG scenario',
       x = '', y = '') +
  guides(size = 'none', color = 'none',
         fill = guide_colorbar(barwidth = 30))


# save
ggsave(paste0("figures/fig_si/figSI_4.1.1_reduced_temp_byTh.pdf"), plot = mapsi_4.1.1,
       width = 13, height = 10)
ggsave(paste0("figures/fig_si/figSI_4.1.1_reduced_temp_byTh.png"), plot = mapsi_4.1.1,
       width = 13, height = 10)



## SI -- geom line cooling potential
data_to_plot <- plotmap %>%
  filter(year5 %in% des_period)
data_to_plot[, lat_bin := round(lat)]
data_to_plot <- data_to_plot[, .(
  cool_est  = mean(value, na.rm = TRUE),
  cool_low  = quantile(value, 0.05, na.rm = TRUE),
  cool_high = quantile(value, 0.95, na.rm = TRUE)
), by = .(lat_bin, scen, year5, adapt_th)]


mapsi_4.1.2 <- ggplot(data = data_to_plot,
                      aes(x = lat_bin, y = cool_est, fill = adapt_th,
                          color = adapt_th)) +
  # mean line
  geom_line(linewidth = 0.75) +

  # CI
  geom_ribbon(aes(ymin = cool_low, ymax = cool_high), alpha = 0.2, color = NA) +

  # palette
  scale_color_manual(values = pal_color_adapt_sens,
                     labels = pal_labels_adapt_sens) +
  scale_fill_manual(values = pal_color_adapt_sens,
                    labels = pal_labels_adapt_sens) +

  # theme and layout
  basic_theme +
  theme(axis.text.y = element_blank()) +
  guides(fill = 'none') +

  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(x, "ºC")) +

  # labs
  labs(color = "Tree coverage threshold (%)",
       x = '', y = 'Temperature (ºC)')


# save
ggsave(paste0("figures/fig_si/figSI_4.1.2_reduced_temp_byTh.pdf"), plot = mapsi_4.1.2, width = 13, height = 10)
ggsave(paste0("figures/fig_si/figSI_4.1.2_reduced_temp_byTh.png"), plot = mapsi_4.1.2, width = 13, height = 10)



## SI -- compound figure
clean_mapfig4.1.1 <- mapsi_4.1.1 +
  labs(fill = "Mean cooling through\ntree coverage increase (ºC)",
       title = '') +
  legend_justif_theme +
  theme(legend.margin = margin(l = -7, unit = "pt"),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
  guides(fill = guide_colorbar(barwidth = 22.5))
clean_mapfig4.1.2 <- mapsi_4.1.2 +
  labs(color = "Tree coverage\nthreshold (ºC)",
       title = '') +
  legend_justif_theme +
  theme(legend.margin = margin(l = -45, unit = "pt"),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
  theme(aspect.ratio = 1)


fig_si_4 <- (clean_mapfig4.1.1 | clean_mapfig4.1.2) +
  plot_layout(widths = c(3, 1)) +
  plot_annotation(tag_levels = list(c("a)", "b)"))) &
  theme(plot.tag = element_text(size = 14, face = "bold"))


ggsave(paste0("figures/fig_si/figSI_4.1_reduced_temp_byTh.pdf"), plot = fig_si_4, width = 10, height = 4)
ggsave(paste0("figures/fig_si/figSI_4.1_reduced_temp_byTh.png"), plot = fig_si_4, width = 10, height = 4)




# ----
# -- panel B -- baseline tree coverage by latitude
# ----
data <- dataa_08[scen == 'CLIM' & settl == 'urban' & adapt == 'perc0' &
                   year == des_per,] %>%
  # add cities info
  merge(unique(projdata[, .(URAU_CODE, lat, lon, region, cntr_name)]),
        by = c("URAU_CODE"))
# mean by latitude
data[, lat_bin := round(lat)]
plotmap <- data[rowSums(is.na(data)) == 0,
                ':='(tree_cover_lat = mean(tree_cover, na.rm = TRUE),
                     tree_cover_low = quantile(tree_cover, 0.05, na.rm = TRUE),
                     tree_cover_high = quantile(tree_cover, 0.95, na.rm = TRUE)),
                by = .(lat_bin, scen)]

# plot
mapfig4.2 <- ggplot(plotmap,
                    aes(x = lat_bin, y = tree_cover_lat)) +

  geom_line(linewidth = 0.75) +

  geom_ribbon(aes(x = lat_bin, ymin = tree_cover_low, ymax = tree_cover_high),
              alpha = 0.2) +

  # 30% hline
  geom_hline(yintercept = 30, color = 'gray80', linetype = 'dashed') +

  # labs
  labs(title = 'Baseline tree coverage',
       x = 'Latitude', y = "Baseline tree coverage (%)") +

  # theme and layout
  basic_theme + legend_justif_theme +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))


mapfig4.2

# save plot
ggsave(paste0("figures/subfig4/fig4.2_baseline_treecoverage.pdf"), plot = mapfig4.2, width = 13, height = 10)
ggsave(paste0("figures/subfig4/fig4.2_baseline_treecoverage.png"), plot = mapfig4.2, width = 13, height = 10)


# TO PRINT -- baseline tree coverage by region
base_tree_se_min <- min(plotmap[region %in% c('Southern','Eastern'), tree_cover])
base_tree_se_av <- mean(plotmap[region %in% c('Southern','Eastern'), tree_cover])
base_tree_se_max <- max(plotmap[region %in% c('Southern','Eastern'), tree_cover])
base_tree_n_min <- min(plotmap[region %in% c('Northern','Western'), tree_cover])
base_tree_n_av <- mean(plotmap[region %in% c('Northern','Western'), tree_cover])
base_tree_n_max <- max(plotmap[region %in% c('Northern','Western'), tree_cover])

print(paste0('Baseline tree coverage S-E: ',
             round(base_tree_se_av,2),' [',
             round(base_tree_se_min,2),', ',
             round(base_tree_se_max,2),']'))
print(paste0('Baseline tree coverage N: ',
             round(base_tree_n_av,2),' [',
             round(base_tree_n_min,2),', ',
             round(base_tree_n_max,2),']'))
# [1] "Baseline tree coverage S-E: 5.82 [0.27, 20.74]]"
# [1] "Baseline tree coverage N: 12.24 [1.36, 45.3]"

# ----
# -- panel C -- map av deaths in 2060
# ----
# subset and add lat-lon data
data <- data_09_period[range == 'heat' & agegroup == 'all' &
                         settl_pop == 'urban' & metric == 'rate' & climate == 'full' &
                         scen == 'CLIM' & period %in% des_period,] %>%
  # update rate units
  group_by(URAU_CODE, scen, period, adapt) %>%
  summarise(est = mean(est) * byrate) %>%
  ungroup() %>%
  # compute AVOIDED deaths (perc0 - perc30)
  pivot_wider(names_from = 'adapt', values_from = 'est') %>%
  mutate(diff = perc0 - perc30) %>%
  ungroup()

data <- merge(data, projdata[year5 %in% des_period & settl_pop == 'total',
                             .(pop = sum(pop)),
                             by = .(period = year5, URAU_CODE, lon, lat, region)],
              by = c("URAU_CODE","period")) %>%
  as.data.table()

plotmap <- setDT(data)[rowSums(is.na(data)) == 0,]

# cut points for palette
cutpts <- unique(sort(c(0, unname(round(
  quantile(plotmap$diff, seq(0, 1, length.out = 20)) / 5) * 5))))
plotmap[, colgrp := cut(diff, cutpts)]
signtab <- table(factor(sign(cutpts), c(-1, 0, 1)))

# palettes (fill and border)
npal <- (max(signtab)) * 2
if (head(signtab, 1) > 0) {
  pal <- c(rev(scico(head(signtab, 1)*2, palette = "bamako", direction = -1)[
    1:head(signtab, 1)]),
    scico(tail(signtab, 1), palette = "acton", direction = -1))
} else {
  pal <- scico(tail(signtab, 1), palette = "acton", direction = -1)
}
bpal <- rep(c("white", "black"), signtab[c("-1", "1")])
names(pal) <- levels(plotmap$colgrp)

mapfig4.3 <- fun_map(plotmap %>%
                       filter(period == des_per)) +

  # labs
  labs(fill = sprintf("Avoided death rate (x%s)",
                      formatC(byrate, format = "f", digits = 0, big.mark = ",")),
       size = "Population (in millions)",
       title = 'Avoided premature death estimates under the MITIG scenario',
       x = '', y = '')

mapfig4.3

# save plot
ggsave(paste0("figures/subfig4/fig4.3_avdeaths_",des_per,".pdf"), plot = mapfig4.3, width = 13, height = 10)
ggsave(paste0("figures/subfig4/fig4.3_avdeaths_",des_per,".png"), plot = mapfig4.3, width = 13, height = 10)



# plot des_per_si
mapsi <- fun_map(plotmap %>%
                   filter(period == des_per_si)) +

  # labs
  labs(fill = sprintf("Avoided death rate (x%s)",
                      formatC(byrate, format = "f", digits = 0, big.mark = ",")),
       size = "Population (in millions)",
       title = 'Avoided premature death estimates under the MITIG scenario',
       x = '', y = '')

# save plot
ggsave(paste0("figures/fig_si/figSI_4.3_avdeaths_",des_per_si,".pdf"), plot = mapsi, width = 13, height = 10)
ggsave(paste0("figures/fig_si/figSI_4.3_avdeaths_",des_per_si,".png"), plot = mapsi, width = 13, height = 10)


# TO PRINT -- avoided UHI deaths
toprint <- data_09_period_global[range == 'heat' & settl_pop == 'urban' &
                                   scenario == 'CLIM' & period %in% des_per,] %>%
  # update rate units
  group_by(scenario, period, adapt) %>%
  summarise(rate_full_est = mean(rate_full_est) * byrate,
            rate_full_low = mean(rate_full_low) * byrate,
            rate_full_high = mean(rate_full_high) * byrate) %>%
  ungroup() %>%
  # compute AVOIDED deaths (perc0 - perc30)
  pivot_longer(cols = c(rate_full_est, rate_full_low, rate_full_high), names_to = 'est', values_to = 'value') %>%
  pivot_wider(names_from = 'adapt', values_from = 'value') %>%
  mutate(diff = perc0 - perc30) %>%
  ungroup() %>%
  as.data.frame()
print(paste0("avoided UHI urban death rate (perc0 - perc30): ",
             round(as.numeric(toprint$diff[toprint$est == 'rate_full_est']),2), ' [',
             round(as.numeric(toprint$diff[toprint$est == 'rate_full_low']),2), ',',
             round(as.numeric(toprint$diff[toprint$est == 'rate_full_high']),2), ']'))
# [1] "avoided UHI urban death rate (perc0 - perc30): 8.15 [0.35,27.84]"

# ----
# -- panel D -- count avoided death rate by latitude
# ----
# subset and add lat-lon data
data <- data_09_period[range == 'heat' & agegroup == 'all' &
                         settl_pop == 'urban' & metric == 'rate' & climate == 'full' &
                         scen == 'CLIM' & period %in% des_period,] %>%
  # update rate units
  group_by(URAU_CODE, scen, period, adapt) %>%
  summarise(est = mean(est) * byrate,
            low = mean(low) * byrate,
            high = mean(high) * byrate) %>%
  ungroup()

data <- merge(data, projdata[year5 %in% des_period & settl_pop == 'total',
                             .(period = year5, URAU_CODE, lon, lat, region)],
              by = c("URAU_CODE","period")) %>%
  as.data.table()

# mean by latitude
data[, lat_bin := round(lat)]
plotmap <- data[rowSums(is.na(data)) == 0,
                ':='(est_lat = mean(est, na.rm = TRUE),
                     low_lat = mean(low, na.rm = TRUE),
                     high_lat = mean(high, na.rm = TRUE)),
                by = .(lat_bin, period, scen, adapt)]

# plot
mapfig4.4 <- ggplot(plotmap %>%
                      filter(period == des_per,
                             adapt %in% c('perc0','perc30'))) +

  geom_line(aes(x = lat_bin, y = est_lat, color = adapt), linewidth = 0.75) +

  geom_ribbon(aes(x = lat_bin, ymin = low_lat, ymax = high_lat, fill = adapt),
              alpha = 0.2) +

  # palette
  scale_color_manual(values = pal_color_adapt,
                     labels = pal_labels_adapt) +
  scale_fill_manual(values = pal_color_adapt,
                    labels = pal_labels_adapt) +
  guides(fill = 'none') +

  # labs
  labs(color = 'Scenario',
       title = 'Deaths rate by latitude under the MITIG scenario',
       x = 'Latitude', y = sprintf("City centre excess\ndeath rate (x%s)",
                                   formatC(byrate, format = "f", digits = 0, big.mark = ","))) +

  # theme and layout
  basic_theme + legend_justif_theme +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))


mapfig4.4

# save plot
ggsave(paste0("figures/subfig4/fig4.4_deaths_adapt_",des_per,".pdf"), plot = mapfig4.4, width = 13, height = 10)
ggsave(paste0("figures/subfig4/fig4.4_deaths_adapt_",des_per,".png"), plot = mapfig4.4, width = 13, height = 10)

# ----
# -- FIG 4 -- composition
# ----
clean_mapfig4.1 <- mapfig4.1 +
  labs(title = '') +
  legend_justif_theme +
  theme(legend.margin = margin(l = 35, unit = "pt"),
        legend.spacing.y = unit(2, "pt"),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
clean_mapfig4.2 <- mapfig4.2 +
  # rotate plot
  coord_flip() +
  # clean
  labs(title = '') +
  theme(legend.margin = margin(l = -25, unit = "pt"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(t = 0, r = 155, b = 0, l = 0))
clean_mapfig4.3 <- mapfig4.3 +
  labs(title = '') +
  legend_justif_theme +
  theme(legend.margin = margin(l = 35, unit = "pt"),
        legend.spacing.y = unit(2, "pt"),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
clean_mapfig4.4 <- mapfig4.4 +
  # rotate plot
  coord_flip() +
  # clean
  labs(title = '') +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(t = 0, r = 155, b = 0, l = 0))


row1 <- (clean_mapfig4.1 | clean_mapfig4.2) +
  plot_layout(widths = c(3, 1))
row2 <- (clean_mapfig4.3 | clean_mapfig4.4) +
  plot_layout(widths = c(3, 1))

fig4 <- row1 / row2 +
  plot_annotation(tag_levels = list(c("a)", "b)", "c)", "d)"))) &
  theme(plot.tag = element_text(size = 14, face = "bold"))

ggsave(paste0("figures/fig4.pdf"), plot = fig4, width = 10, height = 13)
ggsave(paste0("figures/fig4.png"), plot = fig4, width = 10, height = 13)

# NOTE: for fig3-4 abstract, save fig3.pdf, save row1 of fig4 in pdf (half height), and type in the console
# pdfjam --nup 1x2 --delta "0 -2.5cm" fig3.pdf fig4-abstract.pdf --outfile fig34-abstract.pdf
# pdfjam --trim '2cm 2.9cm 2cm 0cm' fig34-abstract.pdf --outfile fig34-abstract.pdf

# -------------------------------------------------------------------------------
# Figure 5: deaths due to UHI
# -------------------------------------------------------------------------------

if (!dir.exists('figures/subfig5/')) dir.create('figures/subfig5/')

# ----
# -- panel A -- map deaths in 2060
# ----
# subset and add lat-lon data
data <- data_09_period[range == 'heat' & agegroup != 'all' & adapt == 'perc0' &
                         metric == 'rate' & climate == 'full' & settl_pop != 'total' &
                         period %in% des_period,] %>%
  # add cities info
  merge(unique(projdata[, .(URAU_CODE, region, cntr_name)]),
        by = c("URAU_CODE"))
# count nº of cities by country
data[, cities_by_cntr := uniqueN(URAU_CODE), by = .(cntr_name)]
data[, cntr_name_full := paste0(cntr_name, " (", cities_by_cntr, ")")]
# update rate units & compute average by URAU_CODE-agegroup
data <- data[, .(est = mean(est) * byrate),
             by = .(scen, period, agegroup, region, cntr_name_full, URAU_CODE, settl_pop)]
# annual average by country
data <- data[, .(est = mean(est)),
             by = .(scen, period, agegroup, region, cntr_name_full, settl_pop)]
# reshape and compute difference
data <- dcast(data,
              scen + period + agegroup + region + cntr_name_full ~ settl_pop,
              value.var = "est")
data <- data[, .(est = urban - rural),
             by = .(scen, period, agegroup, region, cntr_name_full)]

# order countries alphabetically, regions, settl, & agegroup labels
data <- data %>%
  mutate(cntr_name = factor(cntr_name_full, levels = rev(sort(as.character(unique(cntr_name_full))))),
         agegroup = factor(agegroup, levels = c("20-44","45-64","65-74","75-84","85+")),
         region = factor(region, levels = order_regions))


# cut points for palette - 2030
plotmap <- data %>%
  filter(period == des_per)
cutpts <- unique(sort(c(unname(round(min(plotmap$est))), 0, unname(round(
  quantile(plotmap$est, seq(0, 1, length.out = 20)) / 5) * 5))))
cutpts[1] <- cutpts[1]-1
cutpts[length(cutpts)] = cutpts[length(cutpts)] + 2
plotmap[, colgrp := cut(est, cutpts)]
signtab <- table(factor(sign(cutpts), c(-1, 0, 1)))

# palettes (fill and border) - 2030
npal <- (max(signtab)) * 2
pal <- c(rev(scico(npal, palette = "oslo", direction = -1)[
  max(signtab) - signtab[1] + seq_len(signtab[1]) - 1]),
  scico(tail(signtab, 1)+1, palette = "bilbao", direction = -1)[2:(tail(signtab, 1)+1)])
bpal <- rep(c("white", "black"), signtab[c("-1", "1")])
new_label <- as.character(paste0("<=-10"))
old_label <- paste0("(", sort(cutpts)[1], ",", sort(cutpts)[2], "]")
plotmap <- plotmap %>%
  mutate(colgrp = fct_recode(colgrp, !!new_label := old_label))
new_label <- as.character(paste0("(-10,-5]"))
old_label <- paste0("(", sort(cutpts)[2], ",", sort(cutpts)[3], "]")
plotmap <- plotmap %>%
  mutate(colgrp = fct_recode(colgrp, !!new_label := old_label))
new_label <- as.character(paste0(">55"))
old_label <- paste0("(", sort(cutpts)[length(cutpts)-1], ",", sort(cutpts)[length(cutpts)], "]")
plotmap <- plotmap %>%
  mutate(colgrp = fct_recode(colgrp, !!new_label := old_label))
names(pal) <- levels(plotmap$colgrp)

# plot des_per
mapfig5.1 <- ggplot(plotmap,
                    aes(x = agegroup, y = cntr_name, fill = colgrp)) +
  geom_tile() +
  scale_x_discrete(labels = function(x) paste0(x, "y")) +

  # facet
  facet_wrap(. ~ scen,
             labeller = labeller(scen = as_labeller(pal_labels_scen))) +

  # palette
  scale_fill_manual(values = pal) +

  # labs
  labs(fill = sprintf("UHI excess death\nrate (x%s)",
                      formatC(byrate, format = "f", digits = 0, big.mark = ",")),
       title = 'Premature deaths by age group',
       x = '', y = '') +

  # theme and layout
  basic_theme + legend_justif_theme +
  theme(legend.title = element_text(angle = 90, vjust = 0.5)) +
  guides(fill = guide_legend(nrow = 2))


# save plot
ggsave(paste0("figures/subfig5/fig5.1_deaths_uhi_agegroup.pdf"), plot = mapfig5.1, width = 13, height = 10)
ggsave(paste0("figures/subfig5/fig5.1_deaths_uhi_agegroup.png"), plot = mapfig5.1, width = 13, height = 10)


## SI - rate values (no diff) by scen - settl - agegroup
# subset and add lat-lon data
data <- data_09_period[range == 'heat' & agegroup != 'all' &
                         metric == 'rate' & climate == 'full' & settl_pop != 'total' &
                         period %in% des_period,] %>%
  # add cities info
  merge(unique(projdata[, .(URAU_CODE, region, cntr_name)]),
        by = c("URAU_CODE"))
# clean and rename
data <- data[!(adapt == "perc30" & scen == "BASE")]
data[adapt == "perc30" & scen == "CLIM", scen := "ADAPT"]
# count nº of cities by country
data[, cities_by_cntr := uniqueN(URAU_CODE), by = .(cntr_name)]
data[, cntr_name_full := paste0(cntr_name, " (", cities_by_cntr, ")")]
# update rate units & compute average by URAU_CODE-agegroup
data <- data[, .(est = mean(est) * byrate),
             by = .(scen, period, agegroup, region, cntr_name_full, URAU_CODE, settl_pop)]
# annual average by country
data <- data[, .(est = mean(est)),
             by = .(scen, period, agegroup, region, cntr_name_full, settl_pop)]

# order countries alphabetically, regions, settl, & agegroup labels
data <- data %>%
  mutate(settl_pop = factor(settl_pop, levels = c("urban","rural")),
         cntr_name = factor(cntr_name_full, levels = rev(sort(as.character(unique(cntr_name_full))))),
         agegroup = factor(agegroup, levels = c("20-44","45-64","65-74","75-84","85+")),
         region = factor(region, levels = order_regions),
         scen = factor(scen, levels = c('BASE','MITIG','CLIM','ADAPT')))


# cut points for palette - 2030
plotmap <- data %>%
  filter(period == des_per)
cutpts <- unique(sort(c(unname(round(min(plotmap$est))), 0, unname(round(
  quantile(plotmap$est, seq(0, 1, length.out = 20)) / 5) * 5))))
cutpts[length(cutpts)] <- cutpts[length(cutpts)]+2
plotmap[, colgrp := cut(est, cutpts)]
signtab <- table(factor(sign(cutpts), c(-1, 0, 1)))

# palettes (fill and border) - 2030
npal <- (max(signtab)) * 2
pal <- c(rev(scico(npal, palette = "oslo", direction = -1)[
  max(signtab) - signtab[1] + seq_len(signtab[1]) - 1]),
  scico(tail(signtab, 1)+1, palette = "bilbao", direction = -1)[2:(tail(signtab, 1)+1)])
bpal <- rep(c("white", "black"), signtab[c("-1", "1")])
new_label <- as.character(paste0(">=",cutpts[length(cutpts)-1]))
old_label <- as.character(plotmap[est == max(est),]$colgrp)
plotmap <- plotmap %>%
  mutate(colgrp = fct_recode(colgrp, !!new_label := old_label))
names(pal) <- levels(plotmap$colgrp)

# plot des_per
mapsi <- ggplot(plotmap,
                aes(x = agegroup, y = cntr_name, fill = colgrp)) +
  geom_tile() +
  scale_x_discrete(labels = function(x) paste0(x, "y")) +

  # facet
  facet_grid(settl_pop ~ scen,
             labeller = labeller(scen = as_labeller(pal_labels_scen),
                                 settl_pop = as_labeller(pal_labels_settl[1:2]))) +

  # palette
  scale_fill_manual(values = pal) +

  # labs
  labs(fill = sprintf("UHI excess death\nrate (x%s)",
                      formatC(byrate, format = "f", digits = 0, big.mark = ",")),
       # title = 'Premature deaths by age group',
       x = '', y = '') +

  # theme and layout
  basic_theme + legend_justif_theme +
  guides(fill = guide_legend(nrow = 1))


# save plot
ggsave(paste0("figures/fig_si/figSI_5.1_deaths_uhi_agegroup_settl_",des_per,".pdf"), plot = mapsi, width = 13, height = 10)
ggsave(paste0("figures/fig_si/figSI_5.1_deaths_uhi_agegroup_settl_",des_per,".png"), plot = mapsi, width = 13, height = 10)


# ----
# -- panel B -- map av deaths due to adapt in 2060
# ----
# subset and add lat-lon data
data <- data_09_period[range == 'heat' & agegroup != 'all' & scen == 'CLIM' &
                         metric == 'rate' & climate == 'full' & settl_pop == 'urban' &
                         period %in% des_period,] %>%
  # add cities info
  merge(unique(projdata[, .(URAU_CODE, region, cntr_name)]),
        by = c("URAU_CODE"))
# update rate units & compute average by URAU_CODE-agegroup
data <- data[, .(est = mean(est) * byrate),
             by = .(scen, period, agegroup, region, cntr_name, URAU_CODE, settl_pop, adapt)]
# annual average by country
data <- data[, .(est = mean(est)),
             by = .(scen, period, agegroup, region, cntr_name, settl_pop, adapt)]
# reshape and compute difference
data <- dcast(data,
              scen + period + agegroup + region + cntr_name ~ adapt,
              value.var = "est")
data <- data[, .(est = perc0 - perc30),
             by = .(scen, period, agegroup, region, cntr_name)]

# order countries alphabetically, regions, settl, & agegroup labels
data <- data %>%
  mutate(cntr_name = factor(cntr_name, levels = rev(sort(as.character(unique(cntr_name))))),
         agegroup = factor(agegroup, levels = c("20-44","45-64","65-74","75-84","85+")),
         region = factor(region, levels = order_regions))


# cut points for palette - 2030
plotmap <- data %>%
  filter(period == des_per)
cutpts <- unique(sort(c(unname(round(min(plotmap$est))), 0, unname(round(
  quantile(plotmap$est, seq(0, 1, length.out = 20)) / 5) * 5)+1)))
cutpts[length(cutpts)] <- cutpts[length(cutpts)]+1
plotmap[, colgrp := cut(est, cutpts)]
signtab <- table(factor(sign(cutpts), c(-1, 0, 1)))

# palettes (fill and border) - 2030
npal <- (max(signtab)) * 2
if (head(signtab, 1) > 0) {
  pal <- c(rev(scico(head(signtab, 1)*2, palette = "bamako", direction = -1)[
    1:head(signtab, 1)]),
    scico(tail(signtab, 1), palette = "acton", direction = -1))
} else {
  pal <- scico(tail(signtab, 1), palette = "acton", direction = -1)
}
bpal <- rep(c("white", "black"), signtab[c("-1", "1")])
new_label <- as.character(paste0(">",cutpts[length(cutpts)-1]))
old_label <- as.character(plotmap[est == max(est),]$colgrp)
plotmap <- plotmap %>%
  mutate(colgrp = fct_recode(colgrp, !!new_label := old_label))
names(pal) <- levels(plotmap$colgrp)


# plot des_per
mapfig5.2 <- ggplot(plotmap %>%
                      mutate(scen = 'ADAPT'),
                    aes(x = agegroup, y = cntr_name, fill = colgrp)) +
  geom_tile() +
  scale_x_discrete(labels = function(x) paste0(x, "y")) +

  # palette
  scale_fill_manual(values = pal) +

  # facet
  facet_wrap(. ~ scen) +

  # labs
  labs(fill = sprintf("Avoided death\nrate (x%s)",
                      formatC(byrate, format = "f", digits = 0, big.mark = ",")),
       title = 'Avoided premature deaths by age group',
       x = '', y = '') +

  # theme and layout
  basic_theme + legend_justif_theme +
  theme(legend.title = element_text(angle = 90, vjust = 0.5)) +
  guides(fill = guide_legend(nrow = 2))


# save plot
ggsave(paste0("figures/subfig5/fig5.2_av_deaths_uhi_adapt_agegroup.pdf"), plot = mapfig5.2, width = 13, height = 10)
ggsave(paste0("figures/subfig5/fig5.2_av_deaths_uhi_adapt_agegroup.png"), plot = mapfig5.2, width = 13, height = 10)


# SI -- cut points for palette - des_per_si
plotmap <- data %>%
  filter(period == des_per_si)
cutpts <- unique(sort(c(0, unname(round(
  quantile(plotmap$est, seq(0, 1, length.out = 20)) / 5) * 5)+2)))
cutpts[length(cutpts)] <- cutpts[length(cutpts)]+1
plotmap[, colgrp := cut(est, cutpts)]
levels(plotmap$colgrp)[length(levels(plotmap$colgrp))] <- ">257"
signtab <- table(factor(sign(cutpts), c(-1, 0, 1)))

# palettes (fill and border) - 2060
npal <- (max(signtab)) * 2
if (head(signtab, 1) > 0) {
  pal <- c(rev(scico(head(signtab, 1)*2, palette = "bamako", direction = -1)[
    1:head(signtab, 1)]),
    scico(tail(signtab, 1), palette = "acton", direction = -1))
} else {
  pal <- scico(tail(signtab, 1), palette = "acton", direction = -1)
}
bpal <- rep(c("white", "black"), signtab[c("-1", "1")])
names(pal) <- levels(plotmap$colgrp)

# plot des_per_si
mapsi <- ggplot(plotmap,
                aes(x = agegroup, y = cntr_name, fill = colgrp)) +
  geom_tile() +
  scale_x_discrete(labels = function(x) paste0(x, "y")) +

  # facet
  facet_wrap(. ~ scen) +

  # palette
  scale_fill_manual(values = pal) +

  # labs
  labs(fill = sprintf("Avoided death rate (x%s)",
                      formatC(byrate, format = "f", digits = 0, big.mark = ",")),
       title = 'Avoided premature deaths by age group',
       x = '', y = '') +

  # theme and layout
  basic_theme + legend_justif_theme +
  guides(fill = guide_legend(nrow = 1))

mapsi

# save plot
ggsave(paste0("figures/fig_si/figSI_5.2_deaths_uhi_agegroup_",des_per_si,".pdf"), plot = mapsi, width = 13, height = 10)
ggsave(paste0("figures/fig_si/figSI_5.2_deaths_uhi_agegroup_",des_per_si,".png"), plot = mapsi, width = 13, height = 10)



# ----
# -- panel C -- map deaths by agegroup, scenario, settl_pop
# ----
# subset and add lat-lon data
data <- data_09_period_global_age[range == 'heat' & agegroup != 'all' &
                                    settl_pop != 'total' &
                                    period %in% des_period,]
# update rate units & compute global average by agegroup
data <- data[, .(est = mean(rate_full_est) * byrate,
                 low = mean(rate_full_low) * byrate,
                 high = mean(rate_full_high) * byrate),
             by = .(scen = scenario, period, agegroup, settl_pop, adapt)]
# reshape and compute difference
data <- melt(data,
             id.vars = c("scen", "period", "agegroup", "adapt", "settl_pop"),
             measure.vars = c("est", "low", "high"),
             variable.name = "res",
             value.name = "value")

# rearrange labels
data <- data %>%
  # order labels
  mutate(agegroup = factor(agegroup, levels = c("20-44","45-64","65-74","75-84","85+")),
         settl_pop = factor(settl_pop, levels = c('urban','rural')),
         scen = factor(scen, levels = c('BASE','MITIG','CLIM','ADAPT')))


# plot - 2030
plotmap <- dcast(data[period == des_per &
                        adapt %in% c("perc30","perc0")],
                 scen + period + agegroup + adapt +
                   settl_pop ~ res,
                 value.var = "value")
plotmap <- plotmap[!(adapt == "perc30" & scen == "BASE")]
plotmap <- plotmap[adapt == "perc30", scen := "ADAPT"]
# compute CI ONLY by scen
plotmap <- plotmap %>%
  group_by(scen, period, agegroup) %>%
  mutate(low = min(low),
         high = max(high)) %>%
  ungroup() %>%
  # add "light" labels
  mutate(scen_light = paste0(scen, "_light"))

mapfig5.3 <- ggplot(plotmap,
                    aes(x = agegroup, y = est,
                        group = interaction(scen,settl_pop))) +
  # CI
  geom_ribbon(data = plotmap,
              aes(x = agegroup, ymin = low, ymax = high, fill = scen_light),
              alpha = 0.3) +

  # estimates
  geom_point(aes(color = scen)) +
  geom_line(aes(color = scen, linetype = settl_pop), linewidth = 0.6) +

  # palette
  scale_color_manual(values = pal_color_scen,
                     labels = pal_labels_scen) +
  scale_linetype_manual(values = pal_linetype_settl,
                        labels = pal_labels_settl) +
  scale_fill_manual(values = pal_color_scen,
                    labels = pal_labels_scen) +

  # labs
  labs(color = 'Scenario', linetype = 'Exposure Area',
       y = sprintf("Avoided excess\ndeath rate (x%s)",
                   formatC(byrate, format = "f", digits = 0, big.mark = ",")),
       x = '') +
  scale_x_discrete(labels = function(x) paste0(x, "y")) +

  # theme and layout
  basic_theme + legend_justif_theme +
  guides(fill = 'none')

mapfig5.3

# save plot
ggsave(paste0("figures/subfig5/fig5.3_deaths_uhi_adapt_settl_agegroup.pdf"), plot = mapfig5.3, width = 13, height = 10)
ggsave(paste0("figures/subfig5/fig5.3_deaths_uhi_adapt_settl_agegroup.png"), plot = mapfig5.3, width = 13, height = 10)


## TO PRINT -- global deaths by age group & CI
toprint <- setDT(plotmap)[period == des_per & agegroup == '85+' & settl_pop == 'urban']
print(paste0('BASE: ', round(toprint[scen == 'BASE']$est,2), ' [',
             round(toprint[scen == 'BASE']$low,2),', ',
             round(toprint[scen == 'BASE']$high,2), ']'))
print(paste0('MITIG: ', round(toprint[scen == 'CLIM']$est,2), ' [',
             round(toprint[scen == 'CLIM']$low,2),', ',
             round(toprint[scen == 'CLIM']$high,2), ']'))
print(paste0('ADAPT: ', round(toprint[adapt == 'perc30']$est,2), ' [',
             round(toprint[adapt == 'perc30']$low,2),', ',
             round(toprint[adapt == 'perc30']$high,2), ']'))
# [1] "BASE: 261.59 [13.79, 843.46]"
# [1] "MITIG: 226.41 [4.51, 752.64]"
# [1] "ADAPT: 185.89 [5.13, 620.11]"

## TO PRINT -- global deaths by age group, settl & CI
data <- data_09_period_global_age[range == 'heat' & agegroup != 'all' &
                                    period %in% des_period & adapt %in% c("perc30","perc0"),]
# update rate units & compute global average by agegroup
data <- data[, .(est = mean(rate_full_est) * byrate,
                 low = mean(rate_full_low) * byrate,
                 high = mean(rate_full_high) * byrate),
             by = .(scen = scenario, period, agegroup, settl_pop, adapt)]
data <- melt(data,
             id.vars = c("scen", "period", "agegroup", "adapt", "settl_pop"),
             measure.vars = c("est", "low", "high"),
             variable.name = "res",
             value.name = "value")
plotmap <- dcast(data[period == des_per],
                 scen + period + agegroup + adapt +
                   settl_pop ~ res,
                 value.var = "value")
plotmap <- plotmap[!(adapt == "perc30" & scen == "BASE")]
plotmap <- plotmap[adapt == "perc30", scen := "ADAPT"]
toprint <- setDT(plotmap)[period == des_per & agegroup == '85+']
print(paste0('MITIG: ', round(toprint[scen == 'CLIM' & settl_pop == 'total']$est,2), ' [',
             round(toprint[scen == 'CLIM' & settl_pop == 'total']$low,2),', ',
             round(toprint[scen == 'CLIM' & settl_pop == 'total']$high,2), ']'))
print(paste0('MITIG: ', round(toprint[scen == 'CLIM' & settl_pop == 'urban']$est,2), ' [',
             round(toprint[scen == 'CLIM' & settl_pop == 'urban']$low,2),', ',
             round(toprint[scen == 'CLIM' & settl_pop == 'urban']$high,2), ']'))
print(paste0('ADAPT: ', round(100*((toprint[scen == 'ADAPT' & settl_pop == 'urban']$est-toprint[scen == 'CLIM' & settl_pop == 'urban']$est)/toprint[scen == 'CLIM' & settl_pop == 'urban']$est),2), ' [',
             round(100*((toprint[scen == 'ADAPT' & settl_pop == 'urban']$low-toprint[scen == 'CLIM' & settl_pop == 'urban']$low)/toprint[scen == 'CLIM' & settl_pop == 'urban']$low),2),', ',
             round(100*((toprint[scen == 'ADAPT' & settl_pop == 'urban']$high-toprint[scen == 'CLIM' & settl_pop == 'urban']$high)/toprint[scen == 'CLIM' & settl_pop == 'urban']$high),2), ']'))
# [1] "MITIG: 199.74 [4.83, 678.4]"
# [1] "MITIG: 226.41 [7.76, 752.64]"
# [1] "ADAPT: -17.9 [-33.82, -17.61]"

# ----
# -- FIG 5 -- composition
# ----
clean_mapfig5.1 <- mapfig5.1 +
  labs(title = '') +
  theme(legend.margin = margin(l = 60, unit = "pt"))
clean_mapfig5.2 <- mapfig5.2 +
  theme(axis.text.y = element_blank(),
        legend.margin = margin(l = -55, unit = "pt")) +
  labs(title = '')
clean_mapfig5.3 <- mapfig5.3
# theme(axis.title.y = element_blank())

fig5 <- (clean_mapfig5.1 | clean_mapfig5.2) +
  plot_layout(widths = c(1, 0.5)) +
  plot_annotation(tag_levels = list(c("a)", "b)"))) &
  theme(plot.tag = element_text(size = 14))

fig5 <- fig5 / clean_mapfig5.3 +
  plot_layout(heights = c(1, 0.4)) +
  plot_annotation(tag_levels = list(c("a)", "b)", "c)"))) &
  theme(plot.tag = element_text(size = 14, face = "bold"))


ggsave(paste0("figures/fig5.pdf"), plot = fig5, width = 10, height = 10)
ggsave(paste0("figures/fig5.png"), plot = fig5, width = 10, height = 10)



