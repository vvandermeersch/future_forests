
cols <- c("residuals" = "grey40", "sdm" = "#b4b4b4", "sdm.climate" = "#d4d4d4",
          "gcm" = "#909090", ssp = "#636363")

cols <- c("residuals" = "white", "sdm" = "white", "sdm.climate" = "white",
          "gcm" = "white", ssp = "white")

cols <- c("residuals" = "grey75", "sdm" = "grey85", "sdm.climate" = "grey95",
          "gcm" = "grey92", "gcm.ssp" = "grey87", "ssp" = "grey90")

insetlegend <- ggplot(data = 
         data_barplot %>% filter(ecoregion == "Boreal") %>%
         mutate(meany=replace(meany, species == "abies_alba", 0.37),
                meany=replace(meany, species == "quercus_pubescens", 0.32),
                meany=replace(meany, species == "quercus_robur", 0.39),
                int=replace(int, species == "fagus_sylvatica", 0.12),
                int=replace(int, species == "fagus_sylvatica", 0.07),
                int=replace(int, species == "quercus_ilex", 0.08),
                int=replace(int, species == "quercus_pubescens", 0.06),
                int=replace(int, species == "quercus_petraea", 0.09),
                int=replace(int, species == "quercus_robur", 0.09),
                int=replace(int, species == "abies_alba", 0.10),
                species = factor(species, labels = c("Silver fir", "Beech", 
                                                     "Evergreen oak ","Sessile oak", 
                                                     "Pubesc. oak", "Pedunc. oak")))
       
       
       ) +
  geom_rect(
    data = data.frame(ymin = rep(-0.65,7), ymax = rep(0.65,7),
                      xmin = seq(0.5, 6.5, 1), xmax = seq(1.5, 7.5, 1),
                      alpha = c("0","1","0","1","0","1","0")),
    aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, alpha = alpha),
    fill = "grey90") +
  scale_alpha_manual(values = c(0,0.5)) +
  # facet_wrap(~ species) +
  # geom_hline(aes(yintercept = 0), colour = "white", linewidth=0.3) +
  # geom_hline(aes(yintercept = 0), colour = "grey50", linewidth=0.3, linetype = "dashed") +
  geom_rect(aes(ymin = meany - 1.645*int * (ssp + gcm.ssp + gcm + sdm.climate + sdm + residuals)/tot, 
                ymax = meany + 1.645*int * (ssp + gcm.ssp + gcm + sdm.climate + sdm + residuals)/tot,
                xmin = as.numeric(ecoregion)-mid+(species.pos-wid), xmax = as.numeric(ecoregion)-mid+species.pos, fill = "residuals", group = species),
            color = NA, alpha = 1) +
  geom_rect(aes(ymin = meany - 1.645*int * (ssp + gcm.ssp + gcm + sdm.climate + sdm)/tot, 
                ymax = meany + 1.645*int * (ssp + gcm.ssp + gcm + sdm.climate + sdm)/tot,
                xmin = as.numeric(ecoregion)-mid+(species.pos-wid), xmax = as.numeric(ecoregion)-mid+species.pos, fill = "sdm"),
            color = NA, alpha = 1) +
  geom_rect(aes(ymin = meany - 1.645*int * (ssp + gcm.ssp + gcm + sdm.climate)/tot, 
                ymax = meany + 1.645*int * (ssp + gcm.ssp + gcm + sdm.climate)/tot,
                xmin = as.numeric(ecoregion)-mid+(species.pos-wid), xmax = as.numeric(ecoregion)-mid+species.pos, fill = "sdm.climate"),
            color = NA, alpha = 1) +
  geom_rect(aes(ymin = meany - 1.645*int * (ssp + gcm.ssp + gcm)/tot, 
                ymax = meany + 1.645*int *  (ssp + gcm.ssp + gcm)/tot,
                xmin = as.numeric(ecoregion)-mid+(species.pos-wid), xmax = as.numeric(ecoregion)-mid+species.pos, fill = "gcm"),
            color = NA, alpha = 1) +
  geom_rect(aes(ymin = meany - 1.645*int * (ssp + gcm.ssp)/tot, 
                ymax = meany + 1.645*int * (ssp+ gcm.ssp)/tot,
                xmin = as.numeric(ecoregion)-mid+(species.pos-wid), xmax = as.numeric(ecoregion)-mid+species.pos, fill = "gcm.ssp"),
            color = NA, alpha = 1) +
  geom_rect(aes(ymin = meany - 1.645*int * (ssp)/tot, 
                ymax = meany + 1.645*int * (ssp)/tot,
                xmin = as.numeric(ecoregion)-mid+(species.pos-wid), xmax = as.numeric(ecoregion)-mid+species.pos, fill = "ssp"),
            color = NA, alpha = 1) +
  
  
  geom_rect(aes(ymin = meany - 1.645*int, ymax = meany + 1.645*int,
                xmin = as.numeric(ecoregion)-mid+(species.pos-wid), xmax = as.numeric(ecoregion)-mid+species.pos),
            fill = NA, color = "grey30", linewidth = 0.2) +
  
  # geom_errorbarh(aes(y = mean, 
  #                    xmin = as.numeric(ecoregion)-mid+(species.pos-wid) +0.01, 
  #                    xmax = as.numeric(ecoregion)-mid+species.pos-0.01), 
  #                height = 0, color = "white", linewidth = 0.8) +
  # geom_point(aes(x = as.numeric(ecoregion)-mid+species.pos-(wid/2), y = mean), color = "white", size = 0.2) +
  # geom_point(aes(x = as.numeric(ecoregion)-mid+species.pos-(wid/2), y = mean), color = "grey30", size = 0.1) +
  # geom_errorbarh(aes(y = mean, 
  #                    xmin = as.numeric(ecoregion)-mid+(species.pos-wid) +0.015, 
  #                    xmax = as.numeric(ecoregion)-mid+species.pos-0.015), 
  #                height = 0, color = "grey30", linewidth = 0.4) +
  
  geom_text(aes(y = meany, x = as.numeric(ecoregion)-mid+species.pos-(wid/2), label = species),
             angle = -90, alpha = 0.8, color = "grey10", size = 2.2) +
  
  # shadowtext::geom_shadowtext(aes(y = mean, x = as.numeric(ecoregion)-mid+species.pos-(wid/2), label = species),
  #                             angle = -90, alpha = 0.8, fontface = "italic", bg.r = 0.1, bg.colour = "grey10") +
  
  scale_fill_manual(name = "", 
                    breaks = c("ssp", "gcm.ssp", "gcm", "sdm.climate", "sdm", "residuals"),
                    labels = c("SSP", "SSP-GCM", "GCM", "Climate-SDM", "SDM", "Residuals"),
                    values=cols) +
  
  theme_bw() +
  theme(
    plot.margin = margin(t = 0, b = -3, r = -3, l = 0),
    legend.position = 'none',
    legend.key.height  = unit(6, "pt"), legend.key.width  = unit(6, "pt"),
    panel.border = element_rect(colour = "grey30", fill=NA, linewidth=0.4),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(), axis.title.x = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(), axis.ticks = element_blank()) +
  scale_y_continuous(position = "right", expand = c(0,0), limits = c(0.16,0.56)) +
  labs(y = "Average projected change in suitability") +
  scale_x_continuous(
    limits = c(4.55,5.45), expand = c(0,0),
    breaks=seq(1,5,1), 
    labels = levels(data_barplot$ecoregion))
