
data_barplot$species.pos <- sapply(data_barplot$species, function(s) which(species_order == s)/10 + (which(species_order == s))*0.0025)
data_barplot$sdm.climate = data_barplot$gcm.sdm + data_barplot$ssp.sdm

cols <- c("residuals" = "#937668", "sdm" = "#72957a", "sdm.climate" = "#ddb166",
          "gcm" = "#D98B65", "gcm.ssp" = "#BD8985", "ssp" = "#ab6763") 

mid <- 0.47
wid <- 0.08

barplot_prop_uncertainty_withinspecies <- ggplot(data = data_barplot) +
  geom_rect(
    data = data.frame(ymin = rep(-1.15,5), ymax = rep(0.85,5),
                      xmin = seq(0.5, 4.5, 1), xmax = seq(1.5, 5.5, 1),
                      alpha = c("0","1","0","1","0")),
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
  
  scale_fill_manual(name = NULL, 
                    breaks = c("ssp", "gcm.ssp", "gcm", "sdm.climate", "sdm", "residuals"),
                    labels = c("SSP", "SSP - GCM", "GCM", "Climate - SDM", "SDM", "Residuals"),
                    values=cols) +
  
  geom_text(
    data = data_barplot[data_barplot$ecoregion == "Alpine",],
    aes(y = meany + 1.645*int + 0.03, x = as.numeric(ecoregion)-mid+species.pos-(wid/2), label =  c(2,5,6,7,8,9,3,4,1)),
    angle = 0, color = "grey30", size = 1.7) +
  
  theme_bw() +
  theme(
    plot.margin = margin(t = 0, b = 0, r = 5.5, l = 5.5),
    legend.position = c(.4,.93), legend.text = element_text(size = 6.5, margin = margin(l = 1,  r = 1.5)),
    legend.key.height  = unit(5, "pt"), legend.key.width  = unit(5, "pt"),
    legend.box.background = element_rect(colour = "grey30", fill=NA, linewidth=0.4),
    legend.title = element_text(), 
    legend.margin = margin(t = 1, b = 1, r = 0.5, l = 0.5),
    # legend.box.margin = margin(t = -30),
    panel.border = element_rect(colour = "grey30", fill=NA, linewidth=0.3),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(), axis.title.x = element_blank(),
    axis.title = element_text(size = 8.5),
    axis.title.y = element_text(hjust = 0.72, colour = "grey30"),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(angle = 0, size = 7, vjust = 0)) +
  
  scale_y_continuous(position = "left", expand = c(0,0),
                     breaks = round(seq(-0.75, 0.75, 0.25),2),
                     labels = c("-0.75", "-0.5", "-0.25", "0", "0.25", "0.5", "0.75")) +
  labs(y = "Average projected change in suitability") +
  scale_x_continuous(
    position = "bottom",
    expand = c(0,0),
    breaks=seq(1,5,1),
    labels = c("Mediterr.", "Atlantic", "Continent.", "Alpine", "Boreal")
    #labels = levels(data_barplot$ecoregion)
    ) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  coord_cartesian(clip = 'off', xlim = c(0.5,5.5), ylim = c(-1.15,0.85)) +
  ggnewscale::new_scale_fill() +
  geom_rect(
    data = data.frame(ymin = rep(-1.26,5), ymax = rep(-1.205,5),
                      xmin = seq(0.57, 4.57, 1), xmax = seq(0.65, 4.65, 1),
                      # offset = c(0.02,0.05,0,0.05,0.05),
                      offset = c(0,0.025,-0.055,0.065,0.065),
                      ecoregion = c("Mediterr.", "Atlantic", "Continent.", "Alpine", "Boreal")),
    aes(ymin = ymin, ymax = ymax, xmin = xmin + offset, xmax = xmax + offset, fill = ecoregion)) +
  scale_fill_manual(
    name = NULL,
    values = c("#ed947e", "#99c17b", "#dbbb8e", "#c0ddf0", "#4faba0", "grey90"),
    breaks = c("Mediterr.", "Atlantic", "Continent.", "Alpine", "Boreal"),
    na.value = NA, guide = 'none') +
  annotate('text', y = 0.8, x = .55, label = 'a',
           angle = 0, color = "grey30", size = 10/.pt, hjust = 0)

