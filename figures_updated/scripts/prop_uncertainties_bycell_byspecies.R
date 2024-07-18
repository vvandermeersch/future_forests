
cols <- c("ssp", "gcm", "sdm",
          "sdm.climate",
          "residuals")

anovatot <- rast(anova_ss[c("x", "y", "tot")], type = "xyz")

anova_r <- rast(lapply(cols, function(i)
  rast(anova_ss[c("x", "y", i)], type = "xyz")))

crs(anova_r) <- "EPSG:4326"
anova_rp <- anova_r %>% project("EPSG:3035") 

maps_prop_uncertainties <- ggplot() +
  facet_wrap(~lyr, 
             labeller = labeller(lyr = c(`typemax1.num` = "First major source", `typemax2.num` = "Second major source"))) +
  geom_spatraster(data = subset(anova_rp, 1:3)) +
  scale_fill_gradientn(colours = c("#D98B65", "#ddb166", "#7fa688", "#6B95B2", "#8E85C4"),
                       na.value = NA, limits = c(0,1)) +
  geom_spatvector(data = ctr, color = "grey40", fill = NA, linewidth = 0.1) +
  theme_void() +
  scale_y_continuous(expand = c(0, 0), limits = c(1298120, 5561560)) +
  scale_x_continuous(expand = c(0, 0), limits = c(2441580, 7018580)) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 6.5),
    legend.position = "bottom",
    plot.margin = margin(t=0,b=5.5,l=0,r=0),
    strip.text = element_text(size = 8, margin = margin(t=0,b=5,l=0,r=0)),
    panel.border = element_rect(colour = "grey30", fill=NA, linewidth=0.4),
    panel.grid.major = element_line(color = "grey90"))


sim_r <- simulations %>% 
  group_by(id_cell, x, y) %>%
  summarise(mean = mean(mean)) %>%
  ungroup() %>% 
  dplyr::select(x,y, mean) %>% 
  rast()
crs(sim_r) <- "EPSG:4326"

boot_mean <- function(x, i) mean(x[i])

world_map <- ne_countries(scale="medium",returnclass = 'sf')
countries <- c("France", "Sweden", "Romania", "Finland", "Germany", "Spain", "Poland")
anova_countries <- sapply(countries, function(country){
  cat(country)
  country_map <- world_map %>% 
    filter(sovereignt %in% country) %>%
    sf::st_crop(sf::st_bbox(c(xmin = -12, xmax = 45, ymax = 71, ymin = 32), crs = sf::st_crs(4326))) %>%
    vect()
  
  country_anova <- crop(anova_r, country_map, mask = TRUE)
  
  country_fit <- crop(sim_r, country_map, mask = TRUE)
  mean_fit <- t(global(country_fit, mean, na.rm = TRUE))
  sd_fit <- t(global(country_fit, sd, na.rm = TRUE))
  median_fit <- t(global(country_fit, median, na.rm = TRUE))
  
  # p_hat <- values(country_fit,na.rm=TRUE)
  # alpha <- 0.95
  # b <- boot::boot(p_hat, boot_mean, R = 5000)
  # ci <- boot::boot.ci(b, conf = alpha, type = "basic")  
  # boot.ci.min <- ci[["basic"]][4]
  # boot.ci.max <- ci[["basic"]][5]
  boot.ci.min <- NA
  boot.ci.max <- NA
  
  c(country, t(global(country_anova, mean, na.rm = TRUE)), mean_fit, sd_fit, median_fit, boot.ci.min, boot.ci.max)
}) %>% t() %>% data.frame() 
names(anova_countries) <- c("country", cols, "mean", "sd", "median", "boot.ci.min", "boot.ci.max")

data_plot <- anova_countries %>%
  # tidyr::pivot_longer(cols = cols, names_to = "source", values_to = "prop") %>%
  mutate(mean = as.numeric(mean), 
         sd = as.numeric(sd),
         boot.ci.min = as.numeric(boot.ci.min), boot.ci.max = as.numeric(boot.ci.max),
         country = factor(country, levels = rev(c("Finland", "Sweden", "Poland","Germany", "Romania", "Spain", "France")))) %>%
  mutate_at(cols, as.numeric) %>%
  mutate(tot = ssp + gcm + sdm + sdm.climate + residuals)

# barplot_prop_uncertainties <- ggplot(data = data_plot,
#        aes(x = factor(country, levels = rev(c("Finland", "Sweden", "Poland","Germany", "Romania", "Spain", "France"))), 
#            y = prop*mean, fill = factor(source, levels = c("climate", "sdm.climate", "sdm", "species", "species.all", "residuals")))) +
#   geom_bar(stat="identity") +
#   # geom_point(aes(y = mean)) + 
#   # geom_errorbar(
#   #   data = unique(data_plot[c("country", "boot.ci.min", "boot.ci.max")]),
#   #   aes(x = factor(country, levels = rev(c("Finland", "Sweden", "Poland","Germany", "Romania", "Spain", "France"))),
#   #       ymin=boot.ci.min, ymax=boot.ci.max), width = 0.2, inherit.aes = FALSE, colour = "grey20") +
#   scale_fill_manual(
#     name = "",
#     values = c("#D98B65", "#ddb166", "#7fa688", "#6B95B2", "#9991C9", "#a25752"),
#     breaks = c("climate", "sdm.climate", "sdm", "species", "species.all", "residuals"),
#     labels = c("Climate", "Climate - SDM", "SDM", "Species", "Species - climate/SDM", "Residuals"),
#     na.value = NA) +
#   geom_hline(aes(yintercept = 0), colour = "grey50", linewidth=0.3) + 
#   theme_bw() +
#   theme(
#     legend.position = 'none',
#     panel.border = element_rect(colour = "grey30", fill=NA, linewidth=0.4),
#     panel.grid.major.y = element_line(color = "grey90"),
#     panel.grid.major.x = element_blank(), axis.title.x = element_blank(),
#     axis.title = element_text(size = 8.5),
#     axis.text.y = element_text(size = 7),
#     axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0, size = 8)) +
#   scale_y_continuous(position = "right", expand = c(0,0), limits = c(-0.15,0.3),
#                      breaks = seq(-0.15, 0.3, 0.15)) +
#   labs(y = "Average projected change in suitability")


cols <- c("residuals" = "#937668", "sdm" = "#7fa688", "sdm.climate" = "#ddb166",
          "gcm" = "#D98B65", ssp = "#a25752")

barplot_prop_uncertainties <- ggplot(data = data_plot) +
  
  geom_hline(aes(yintercept = 0), colour = "grey50", linewidth=0.3) +
  geom_rect(aes(ymin = mean - 1.645*sd * (ssp + gcm + sdm.climate + sdm + residuals)/tot, 
                ymax = mean + 1.645*sd * (ssp + gcm + sdm.climate + sdm + residuals)/tot,
                xmin = as.numeric(country)-0.3, xmax = as.numeric(country)+0.3, fill = "residuals"),
            color = NA) +
  geom_rect(aes(ymin = mean - 1.645*sd * (ssp + gcm + sdm.climate + sdm)/tot, 
                ymax = mean + 1.645*sd * (ssp + gcm + sdm.climate + sdm)/tot,
                xmin = as.numeric(country)-0.3, xmax = as.numeric(country)+0.3, fill = "sdm"),
            color = NA) +
  geom_rect(aes(ymin = mean - 1.645*sd * (ssp + gcm + sdm.climate)/tot, 
                ymax = mean + 1.645*sd * (ssp + gcm + sdm.climate)/tot,
                xmin = as.numeric(country)-0.3, xmax = as.numeric(country)+0.3, fill = "sdm.climate"),
            color = NA) +
  geom_rect(aes(ymin = mean - 1.645*sd * (ssp + gcm)/tot, 
                ymax = mean + 1.645*sd *  (ssp + gcm)/tot,
                xmin = as.numeric(country)-0.3, xmax = as.numeric(country)+0.3, fill = "gcm"),
            color = NA) +
  geom_rect(aes(ymin = mean - 1.645*sd * (ssp)/tot, 
                ymax = mean + 1.645*sd * (ssp)/tot,
                xmin = as.numeric(country)-0.3, xmax = as.numeric(country)+0.3, fill = "ssp"),
            color = NA) +
  
  
  geom_rect(aes(ymin = mean - 1.645*sd, ymax = mean + 1.645*sd,
                xmin = as.numeric(country)-0.3, xmax = as.numeric(country)+0.3),
            fill = NA, color = "grey30", linewidth = 0.2) +
  
  geom_errorbarh(aes(y = mean, xmin = as.numeric(country)-0.24, xmax = as.numeric(country)+0.24), 
                 height = 0, color = "white", linewidth = 0.8) +
  geom_point(aes(x = country, y = mean), color = "white", size = 1) +
  geom_point(aes(x = country, y = mean), color = "grey30", size = 0.5) +
  geom_errorbarh(aes(y = mean, xmin = as.numeric(country)-0.20, xmax = as.numeric(country)+0.20), 
                 height = 0, color = "grey30", linewidth = 0.4) +
  
  
  scale_fill_manual(name = "Source of uncertainty:", values=cols) +
  
  theme_bw() +
  theme(
    legend.position = 'none', 
    panel.border = element_rect(colour = "grey30", fill=NA, linewidth=0.4),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(), axis.title.x = element_blank(),
    axis.title = element_text(size = 8.5),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0, size = 8)) +
  scale_y_continuous(position = "right", expand = c(0,0), limits = c(-0.35,0.45),
                     breaks = round(seq(-0.3, 0.4, 0.1),1)) +
  labs(y = "Average projected change in suitability")







