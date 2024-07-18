
anova_ss$typemax1 <- as.factor(anova_ss$typemax1)
anova_ss$typemax2 <- as.factor(anova_ss$typemax2)

levels(anova_ss$typemax1) <- list("1" = "ssp", "2" = "gcm.spp", "3" = "gcm", "4" = "sdm.climate", "5" = "sdm", "6" = "residuals")
levels(anova_ss$typemax2) <- list("1" = "ssp", "2" = "gcm.spp", "3" = "gcm", "4" = "sdm.climate", "5" = "sdm", "6" = "residuals")

anova_ss$typemax1.num <- as.character(anova_ss$typemax1)
anova_ss$typemax2.num <- as.character(anova_ss$typemax2)

anova_r <- c(rast(anova_ss[c("x", "y", "typemax1.num")], type = "xyz"),
             rast(anova_ss[c("x", "y", "typemax2.num")], type = "xyz")) %>%
  mutate(typemax1.num = factor(typemax1.num, levels = c("1", "2", "3", "4", "5", "6")), 
         typemax2.num = factor(typemax2.num, levels = c("1", "2", "3", "4", "5", "6")))

crs(anova_r) <- "EPSG:4326"
anova_r <- anova_r %>% project("EPSG:3035") 

ctr <- ifel(is.na(subset(anova_r,1)), 0, 1) %>% as.polygons()

maps_uncertainties <- ggplot() +
  facet_wrap(~lyr, 
             labeller = labeller(lyr = c(`typemax1.num` = "First major source", `typemax2.num` = "Second major source")),
             ncol = 1, strip.position = 'left') +
  geom_spatraster(data = anova_r, show.legend=TRUE) +
  scale_fill_manual(
    name = "",
    values = c("#a25752", "#BD8985", "#D98B65", "#ddb166", "#7fa688", "#937668"),
    breaks = c("1", "2", "3", "4", "5", "6"),
    labels = c("SSP", "SSP-GCM", "GCM", "Climate-SDM", "SDM", "Residuals"),
    na.value = NA, drop = FALSE) +
  geom_spatvector(data = ctr, color = "grey40", fill = NA, linewidth = 0.1) +
  theme_void() +
  scale_y_continuous(expand = c(0, 0), limits = c(1298120, 5561560)) +
  scale_x_continuous(expand = c(0, 0), limits = c(2441580, 7018580)) +
  theme(
    legend.key.height  = unit(6, "pt"),
    legend.key.width  = unit(5, "pt"),
    legend.title = element_text(size = 6.5),
    legend.text = element_text(size = 6.5, margin = margin(l = 1,  r = 0)),
    legend.position = "bottom",
    legend.box.margin = margin(t = 10, b = 0, r = 0, l = 0),
    plot.margin = margin(t=0,b=5.5,l=0,r=0),
    strip.text = element_text(size = 8, margin = margin(t=0,b=0,l=0,r=5), angle = 90),
    panel.border = element_rect(colour = "grey30", fill=NA, linewidth=0.4),
    panel.grid.major = element_line(color = "grey90")) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))


