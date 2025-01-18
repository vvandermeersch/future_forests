#EU-Trees4F_dataset

dir <- "D:/species/EU-Trees4F_dataset/ens_clim/bin"
species <- "Abies_alba"

file <- paste0(species, "_ens-clim_rcp45_fut2095_bin_pot.tif")
futur <- rast(file.path(dir, file))

file <- paste0(species, "_ens-clim_cur2005_bin_pot.tif")
hist <- rast(file.path(dir, file))

plot(futur-hist)

cstdist <- ifel(hist == 1 & futur == 1, 1, 0)
maskd <- ifel(cstdist == 1, 1, NA)

chgdist <- futur-hist
chgdist <- ifel(chgdist == 0, NA, chgdist)
nochg <- ifel(cstdist == 1, 0, NA)
chgdist <- sum(chgdist,nochg, na.rm = TRUE)
names(chgdist) <- "value"
chgdist <- chgdist %>%
  mutate(value = factor(value))

ctr <- hist
ctr <- ifel(is.na(ctr), 0, 1) %>% 
  as.polygons() %>% 
  simplifyGeom(tolerance=5000, preserveTopology=TRUE, makeValid=FALSE) # less heavy plot


ggplot() +
  geom_spatraster(data = hist,
                  maxcell = 2e+05) +
  scale_fill_gradient(low = "grey98", high = "grey98", na.value = NA, guide="none") +
  ggnewscale::new_scale_fill() +
  geom_spatraster(data = chgdist,
                  maxcell = 2e+05) +
  scale_fill_manual(
    name = "Projected suitability",
    breaks = c(-1, -0.5, 0, 0.5, 1),
    values = c(`-1`="#a63716", NA, `0`= "#c7cdd1", NA, `1`="#15607a"),  
    labels = c("No longer suitable", "Decreasing", "No change", "Increasing", "Becoming suitable"),
    na.value = NA, guide = 'none') +
  geom_spatvector(data = ctr, color = "grey40", fill = NA, linewidth = 0.1) +
  theme_void() +
  #scale_x_continuous(limits = c(2431478, 6800000)) +
  #scale_y_continuous(limits = c(1288120, 5571562)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(1288428, 5551462)) +
  theme(
    legend.key.height  = unit(8, "pt"),
    legend.key.width = unit(8, "pt"),
    legend.title = element_text(size = 6.7, face = "bold"),
    legend.text = element_text(size = 6.5),
    legend.position = "right", 
    legend.box.margin = margin(t = 0, b = 0, r = 0, l = 0),
    panel.border = element_rect(colour = "grey30", fill=NA, linewidth=0.4),
    panel.grid.major = element_line(color = "grey90"))
