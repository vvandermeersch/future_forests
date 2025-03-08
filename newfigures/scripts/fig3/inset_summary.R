
data_all <- data_barplot %>%
  summarise(sdm = mean(sdm/tot*100), 
            sdm.climate = mean((gcm.sdm+ssp.sdm)/tot*100), residuals = mean((residuals/tot*100)),
            gcm = mean((gcm)/tot*100), 
            gcm.ssp = mean((gcm.ssp)/tot*100),
            ssp = mean((ssp)/tot*100)
  )

data_all <- 
  pivot_longer(data_all, cols = c("sdm", "sdm.climate", "gcm", "gcm.ssp", "ssp", "residuals"), names_to = "var", values_to = "frac") %>%
  mutate(ymax = cumsum(frac),
         ymin = c(0, head(ymax, n=-1)))

data_all$pos <- (data_all$ymax + data_all$ymin) / 2

cols <- c("residuals" = "#937668", "sdm" = "#72957a", "sdm.climate" = "#ddb166",
          "gcm" = "#D98B65", "gcm.ssp" = "#BD8985", "ssp" = "#ab6763") 

bump <- 2

data_all[data_all$var == "sdm", "ymax"] <- data_all[data_all$var == "sdm", "ymax"] + bump
data_all[data_all$var == "sdm", "ymin"] <- data_all[data_all$var == "sdm", "ymin"] + bump

data_all[data_all$var == "sdm.climate", "ymax"] <- data_all[data_all$var == "sdm.climate", "ymax"] + bump*2
data_all[data_all$var == "sdm.climate", "ymin"] <- data_all[data_all$var == "sdm.climate", "ymin"] + bump*2

data_all[data_all$var == "gcm", "ymax"] <- data_all[data_all$var == "gcm", "ymax"] + bump*3
data_all[data_all$var == "gcm", "ymin"] <- data_all[data_all$var == "gcm", "ymin"] + bump*3

data_all[data_all$var == "gcm.ssp", "ymax"] <- data_all[data_all$var == "gcm.ssp", "ymax"] + bump*3
data_all[data_all$var == "gcm.ssp", "ymin"] <- data_all[data_all$var == "gcm.ssp", "ymin"] + bump*3

data_all[data_all$var == "ssp", "ymax"] <- data_all[data_all$var == "ssp", "ymax"] + bump*3
data_all[data_all$var == "ssp", "ymin"] <- data_all[data_all$var == "ssp", "ymin"] + bump*3

data_all[data_all$var == "residuals", "ymax"] <- data_all[data_all$var == "residuals", "ymax"] + bump*4
data_all[data_all$var == "residuals", "ymin"] <- data_all[data_all$var == "residuals", "ymin"] + bump*4


plot_summary <- ggplot(data_all, aes(ymax=3, ymin=2.5, xmax=ymax, xmin=ymin, fill=var)) +
  geom_rect(color = "grey30", linewidth = 0.1) +
  # coord_polar(theta="y") +
  ylim(c(1.1, 4.4)) +
  coord_cartesian(clip = 'off', xlim = c(0,110)) +
  theme_void() +
  theme(legend.position = "none") + 
  scale_fill_manual(name = NULL, 
                    breaks = c("ssp", "gcm.ssp", "gcm", "sdm.climate", "sdm", "residuals"),
                    labels = c("SSP", "SSP - GCM", "GCM", "Climate - SDM", "SDM", "Residuals"),
                    values=cols) +
  theme(strip.text = element_blank(),
        panel.spacing = unit(-1, "lines"),
        plot.margin = margin(r = 0)) +
  geom_segment(
    data = data_all[data_all$var == "sdm",],
    aes(x =  ymin, xend = ymax, y = 2.1, yend = 2.1), 
    color = 'grey30', linewidth = .2) +
  geom_segment(
    data = data_all[data_all$var == "sdm",],
    aes(x =  ymin, xend = ymin, y = 2.1, yend = 2.3), 
    color = 'grey30', linewidth = .2) +
  geom_segment(
    data = data_all[data_all$var == "sdm",],
    aes(x =  ymax, xend = ymax, y = 2.1, yend = 2.3), 
    color = 'grey30', linewidth = .2) +
  geom_segment(
    data = data_all[data_all$var %in% c("ssp", "gcm.ssp", "gcm"),] %>%
      summarise(ymin = min(ymin), ymax = max(ymax)),
    aes(x =  ymin, xend = ymax, y = 2.1, yend = 2.1), 
    color = 'grey30', linewidth = .2, inherit.aes = FALSE) +
  geom_segment(
    data = data_all[data_all$var %in% c("ssp", "gcm.ssp", "gcm"),] %>%
      summarise(ymin = min(ymin), ymax = max(ymax)),
    aes(x =  ymin, xend = ymin, y = 2.1, yend = 2.3), 
    color = 'grey30', linewidth = .2, inherit.aes = FALSE) +
  geom_segment(
    data = data_all[data_all$var %in% c("ssp", "gcm.ssp", "gcm"),] %>%
      summarise(ymin = min(ymin), ymax = max(ymax)),
    aes(x =  ymax, xend = ymax, y = 2.1, yend = 2.3), 
    color = 'grey30', linewidth = .2, inherit.aes = FALSE)+ 
  geom_text(
    data = data_all[data_all$var %in% c("ssp", "gcm.ssp", "gcm"),] %>%
      summarise(ymin = min(ymin), ymax = max(ymax)),
    aes(y = 1.7, x = (ymin+ymax)/2, label = 'Emissions'),
    angle = 0, color = "grey30", size = 1.8, inherit.aes = FALSE) +
  geom_text(
    data = data_all[data_all$var %in% c("ssp", "gcm.ssp", "gcm"),] %>%
      summarise(ymin = min(ymin), ymax = max(ymax)),
    aes(y = 1.3, x = (ymin+ymax)/2, label = '+ climate models'),
    angle = 0, color = "grey30", size = 1.8, inherit.aes = FALSE) +
  geom_text(
    data = data_all[data_all$var == "sdm",],
    aes(y = 1.7, x = (ymin+ymax)/2, label = 'Ecological models'),
    angle = 0, color = "grey30", size = 1.8, inherit.aes = FALSE) +
  theme(
    plot.margin = margin(t = 2, b = 5, r = 0, l = 0),
    panel.border = element_rect(colour = "grey30", fill=NA, linewidth=0.2)) +
  # shadowtext::geom_shadowtext(data = data_all[data_all$var %in% c("ssp", "gcm.ssp", "gcm"),] %>%
  #                               summarise(ymin = min(ymin), ymax = max(ymax), frac = sum(frac)), 
  #                             aes(x = (ymin+ymax)/2, y = 3, label = paste0(round(frac,0), "%")), 
  #                             color = "white", size = 2, bg.colour='#ab6763', bg.r = 0.13*3, inherit.aes = FALSE) +
  # shadowtext::geom_shadowtext(data = data_all[data_all$var %in% c("ssp", "gcm.ssp", "gcm"),] %>%
  #                               summarise(ymin = min(ymin), ymax = max(ymax), frac = sum(frac)), 
  #                             aes(x = (ymin+ymax)/2, y = 3, label = paste0(round(frac,0), "%")), 
  #                             color = "white", size = 2, bg.colour='#BD8985', bg.r = 0.13*2, inherit.aes = FALSE) +
  shadowtext::geom_shadowtext(data = data_all[data_all$var %in% c("ssp", "gcm.ssp", "gcm"),] %>%
                                summarise(ymin = min(ymin), ymax = max(ymax), frac = sum(frac)), 
                              aes(x = (ymin+ymax)/2, y = 3.1, label = paste0(round(frac,0), "%")), 
                              color = "white", size = 2, bg.colour='grey30', bg.r = 0.11, inherit.aes = FALSE) +
  shadowtext::geom_shadowtext(data = data_all[data_all$var == "sdm",], 
                              aes(x = (ymin+ymax)/2, y = 3.1, label = paste0(round(frac,0), "%")), 
                              color = "white", size = 2, bg.colour='grey30', bg.r = 0.11, inherit.aes = FALSE) +
  annotate('text', y = 4.05, x = 55, label = 'All species, all biomes',
           angle = 0, color = "grey30", size = 1.9, hjust = 0.5) +
  annotate('text', y = 4.2, x = -2, label = 'c',
           angle = 0, color = "grey30", size = 9/.pt, hjust = 0)





