cols <- c("residuals" = "#937668", "sdm" = "#7fa688", "sdm.climate" = "#ddb166",
          "climate" = "#D98B65", "species" = "#6B95B2", "species.all" = "#8E85C4")

barplot_prop_uncertainty <- saved_data_all %>% 
  filter(year == 2090) %>%
  mutate(climate = gcm + ssp + gcm.ssp, sdm.climate = gcm.sdm + ssp.sdm, species.all = species.sdm + species.gcm + species.ssp,
         ecoregion = factor(ecoregions, levels = c("Mediterranean", "Atlantic", "Continental", "Alpine", "Boreal"),
                            labels = c("Mediter.", "Atlantic", "Contin.", "Alpine", "Boreal"))) %>% 
  
  ggplot() +
  
  geom_hline(aes(yintercept = 0), colour = "grey50", linewidth=0.3) +
  geom_rect(aes(ymin = meany - 1.645*int * (climate + sdm.climate + sdm + species + species.all + residuals)/tot, 
                ymax = meany + 1.645*int * (climate + sdm.climate + sdm + species + species.all + residuals)/tot,
                xmin = as.numeric(ecoregion)-0.3, xmax = as.numeric(ecoregion)+0.3, fill = "residuals"),
            color = NA) +
  geom_rect(aes(ymin = meany - 1.645*int * (climate + sdm.climate + sdm + species + species.all)/tot, 
                ymax = meany + 1.645*int * (climate + sdm.climate + sdm + species + species.all)/tot,
                xmin = as.numeric(ecoregion)-0.3, xmax = as.numeric(ecoregion)+0.3, fill = "species.all"),
            color = NA) +
  geom_rect(aes(ymin = meany - 1.645*int * (climate + sdm.climate + sdm + species)/tot, 
                ymax = meany + 1.645*int * (climate + sdm.climate + sdm + species)/tot,
                xmin = as.numeric(ecoregion)-0.3, xmax = as.numeric(ecoregion)+0.3, fill = "species"),
            color = NA) +
  geom_rect(aes(ymin = meany - 1.645*int * (climate + sdm.climate + sdm)/tot, 
                ymax = meany + 1.645*int * (climate + sdm.climate + sdm)/tot,
                xmin = as.numeric(ecoregion)-0.3, xmax = as.numeric(ecoregion)+0.3, fill = "sdm"),
            color = NA) +
  geom_rect(aes(ymin = meany - 1.645*int * (climate + sdm.climate)/tot, 
                ymax = meany + 1.645*int * (climate + sdm.climate)/tot,
                xmin = as.numeric(ecoregion)-0.3, xmax = as.numeric(ecoregion)+0.3, fill = "sdm.climate"),
            color = NA) +
  geom_rect(aes(ymin = meany - 1.645*int * (climate)/tot, 
                ymax = meany + 1.645*int * (climate)/tot,
                xmin = as.numeric(ecoregion)-0.3, xmax = as.numeric(ecoregion)+0.3, fill = "climate"),
            color = NA) +
  
  
  geom_rect(aes(ymin = meany - 1.645*int, ymax = meany + 1.645*int,
                xmin = as.numeric(ecoregion)-0.3, xmax = as.numeric(ecoregion)+0.3),
            fill = NA, color = "grey30", linewidth = 0.2) +
  
  geom_errorbarh(aes(y = meany, xmin = as.numeric(ecoregion)-0.24, xmax = as.numeric(ecoregion)+0.24), 
                 height = 0, color = "white", linewidth = 0.8) +
  geom_point(aes(x = ecoregion, y = meany), color = "white", size = 1) +
  geom_point(aes(x = ecoregion, y = meany), color = "grey30", size = 0.5) +
  geom_errorbarh(aes(y = meany, xmin = as.numeric(ecoregion)-0.20, xmax = as.numeric(ecoregion)+0.20), 
                 height = 0, color = "grey30", linewidth = 0.4) +
  
  
  scale_fill_manual(name = "", values=cols) +
  
  theme_bw() +
  theme(
    legend.position = 'none',
    panel.border = element_rect(colour = "grey30", fill=NA, linewidth=0.4),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(), axis.title.x = element_blank(),
    axis.title = element_text(size = 8.5),
    axis.text.y = element_text(size = 7),
    plot.margin = margin(t=5.5,b=5.5,l=5.5,r=0),
    axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0, size = 8)) +
  scale_y_continuous(position = "right", expand = c(0,0), limits = c(-0.5,0.75),
                     breaks = round(seq(-0.5, 0.75, 0.25),2)) +
  labs(y = "Average projected change in suitability")
