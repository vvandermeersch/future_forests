
data_barplot$species.pos <- sapply(data_barplot$species, function(s) which(species_list == s)/10 + (which(species_list == s))*0.03)
data_barplot$sdm.climate = data_barplot$gcm.sdm + data_barplot$ssp.sdm

cols <- c("residuals" = "#937668", "sdm" = "#7fa688", "sdm.climate" = "#ddb166",
          "gcm" = "#D98B65", "gcm.ssp" = "#BD8985", "ssp" = "#a25752") 

mid <- 0.39
wid <- 0.13

barplot_prop_uncertainty_withinspecies <- ggplot(data = data_barplot) +
  geom_rect(
    data = data.frame(ymin = rep(-0.6,7), ymax = rep(0.85,7),
                      xmin = seq(0.5, 6.5, 1), xmax = seq(1.5, 7.5, 1),
                      alpha = c("0","1","0","1","0","1","0")),
    aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, alpha = alpha),
    fill = "grey90") +
  scale_alpha_manual(values = c(0,0.5), guide="none") +
  # facet_wrap(~ species) +
  geom_hline(aes(yintercept = 0), colour = "white", linewidth=0.4) +
  geom_hline(aes(yintercept = 0), colour = "grey50", linewidth=0.3, linetype = "dashed") +
  geom_rect(aes(ymin = meany - 1.645*int * (ssp + gcm + gcm.ssp + sdm.climate + sdm + residuals)/tot, 
                ymax = meany + 1.645*int * (ssp + gcm + gcm.ssp + sdm.climate + sdm + residuals)/tot,
                xmin = as.numeric(ecoregion)-mid+(species.pos-wid), xmax = as.numeric(ecoregion)-mid+species.pos, fill = "residuals", group = species),
            color = NA) +
  geom_rect(aes(ymin = meany - 1.645*int * (ssp + gcm + gcm.ssp + sdm.climate + sdm)/tot, 
                ymax = meany + 1.645*int * (ssp + gcm + gcm.ssp + sdm.climate + sdm)/tot,
                xmin = as.numeric(ecoregion)-mid+(species.pos-wid), xmax = as.numeric(ecoregion)-mid+species.pos, fill = "sdm"),
            color = NA) +
  geom_rect(aes(ymin = meany - 1.645*int * (ssp + gcm + gcm.ssp + sdm.climate)/tot, 
                ymax = meany + 1.645*int * (ssp + gcm + gcm.ssp + sdm.climate)/tot,
                xmin = as.numeric(ecoregion)-mid+(species.pos-wid), xmax = as.numeric(ecoregion)-mid+species.pos, fill = "sdm.climate"),
            color = NA) +
  geom_rect(aes(ymin = meany - 1.645*int * (ssp + gcm.ssp + gcm)/tot, 
                ymax = meany + 1.645*int *  (ssp + gcm.ssp +  gcm)/tot,
                xmin = as.numeric(ecoregion)-mid+(species.pos-wid), xmax = as.numeric(ecoregion)-mid+species.pos, fill = "gcm"),
            color = NA) +
  geom_rect(aes(ymin = meany - 1.645*int * (ssp + gcm.ssp)/tot, 
                ymax = meany + 1.645*int *  (ssp + gcm.ssp)/tot,
                xmin = as.numeric(ecoregion)-mid+(species.pos-wid), xmax = as.numeric(ecoregion)-mid+species.pos, fill = "gcm.ssp"),
            color = NA) +
  geom_rect(aes(ymin = meany - 1.645*int * (ssp)/tot, 
                ymax = meany + 1.645*int * (ssp)/tot,
                xmin = as.numeric(ecoregion)-mid+(species.pos-wid), xmax = as.numeric(ecoregion)-mid+species.pos, fill = "ssp"),
            color = NA) +
  
  
  geom_rect(aes(ymin = meany - 1.645*int, ymax = meany + 1.645*int,
                xmin = as.numeric(ecoregion)-mid+(species.pos-wid), xmax = as.numeric(ecoregion)-mid+species.pos),
            fill = NA, color = "grey30", linewidth = 0.2) +
  
  geom_errorbarh(aes(y = meany, 
                     xmin = as.numeric(ecoregion)-mid+(species.pos-wid) +0.01, 
                     xmax = as.numeric(ecoregion)-mid+species.pos-0.01), 
                 height = 0, color = "white", linewidth = 0.8) +
  # geom_point(aes(x = as.numeric(ecoregion)-mid+species.pos-(wid/2), y = mean), color = "white", size = 0.2) +
  # geom_point(aes(x = as.numeric(ecoregion)-mid+species.pos-(wid/2), y = mean), color = "grey30", size = 0.1) +
  geom_errorbarh(aes(y = meany, 
                     xmin = as.numeric(ecoregion)-mid+(species.pos-wid) +0.015, 
                     xmax = as.numeric(ecoregion)-mid+species.pos-0.015), 
                 height = 0, color = "grey30", linewidth = 0.4) +
  
  scale_fill_manual(name = "", 
                    breaks = c("ssp", "gcm.ssp", "gcm", "sdm.climate", "sdm", "residuals"),
                    labels = c("SSP", "SSP - GCM", "GCM", "Climate - SDM", "SDM", "Residuals"),
                    values=cols) +
  
  theme_bw() +
  theme(
    plot.margin = margin(t = 0, b = 0, r = 5.5, l = 5.5),
    legend.position = 'bottom', legend.text = element_text(size = 6.5, margin = margin(l = 1,  r = 2.5)),
    legend.key.height  = unit(5, "pt"), legend.key.width  = unit(5, "pt"),
    legend.box.margin = margin(t = -10),
    panel.border = element_rect(colour = "grey30", fill=NA, linewidth=0.4),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(), axis.title.x = element_blank(),
    axis.title = element_text(size = 8.5),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(angle = 0, size = 7, vjust = 0)) +
  scale_y_continuous(position = "left", expand = c(0,0), limits = c(-0.6,0.85),
                     breaks = round(seq(-0.5, 0.75, 0.25),2),
                     labels = c("-0.5", "-0.25", "0", "0.25", "0.5", "0.75")) +
  labs(y = "Average projected change in suitability") +
  scale_x_continuous(
    position = "top",
    limits = c(0.5,5.5), expand = c(0,0),
    breaks=seq(1,5,1), 
    labels = levels(data_barplot$ecoregion)) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

