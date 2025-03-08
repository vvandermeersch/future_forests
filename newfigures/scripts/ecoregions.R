
# Load ecoregions shapefile
ecoregions <- vect( file.path(wd, "figures_cleaned/data", "eea_bioregions", "BiogeoRegions2016.shp")) %>% project("EPSG:3035")
ecoregions <- simplifyGeom(ecoregions, 7830.79, makeValid=TRUE)

# Load simulation grid (by loading one of the simulated raster, for example Fagus for some model)
scenario <- "ssp245"; type <- 'expert'; species <- "fagus_sylvatica"
sim_distribution <- readRDS(file = file.path(wd, "figures_cleaned", "data", "maps", paste0(species, "_", type, "_distribution_", scenario, ".rds")))
grid <- subset(sim_distribution,2) %>% project("EPSG:3035") 
names(grid) <- "val"



list_ecoreg <- c("Alpine","Anatolian","Arctic","Atlantic","BlackSea","Boreal",
                 "Continental", "Mediterranean","Pannonian","Steppic")

ecorast <- lapply(1:length(list_ecoreg), function(i){
  e <- list_ecoreg[i]
  ecor <- grid %>% 
    crop(ecoregions[ecoregions$code == e], mask = T) %>%
    extend(grid)  %>%
    mutate(val = ifelse(is.na(val), NA, i))
  return(ecor)
}) %>% sprc() %>% merge() %>% 
  mutate(val = factor(val, labels = list_ecoreg))

grid <- subset(sim_distribution,2)
names(grid) <- "val"


ecorast_wgs84 <- lapply(1:length(list_ecoreg), function(i){
  e <- list_ecoreg[i]
  ecor <- grid %>% 
    crop(ecoregions[ecoregions$code == e]%>% project("EPSG:4326") , mask = T) %>%
    extend(grid)  %>%
    mutate(val = ifelse(is.na(val), NA, i))
  return(ecor)
}) %>% sprc() %>% merge() %>% 
  mutate(val = factor(val, labels = list_ecoreg))

ecorast2 <- ecorast %>% 
  mutate(val = ifelse(val %in% c("Alpine", "Atlantic", "Boreal", "Continental", "Mediterranean"), val, 
                      ifelse(is.na(val), NA, 'Other')))

ctr2 <- ifel(is.na(ecorast2), 0, 1) %>% as.polygons()

saveRDS(ecorast2, file.path(wd, "figures_cleaned", "data", "sims", "ecoregions_rast.rds"))

ecoregions_map <- ggplot() +
  geom_spatraster(data = ecorast2, alpha = 0.8) +
  scale_fill_manual(
    name = NULL,
    values = c("#ed947e", "#99c17b", "#d5af7b", "#c0ddf0", "#4faba0", "grey90"),
    breaks = c("1", "4", "6", "7", "8", "Other"),
    labels = c("Alpine", "Atlantic", "Boreal", "Continental", "Mediterranean", "Other"),
    na.value = NA) +
  geom_spatvector(data = ctr2, color = "grey40", fill = NA, linewidth = 0.1) +
  theme_void() +
  scale_y_continuous(expand = c(0, 0), limits = c(1287903, 5459220)) +
  scale_x_continuous(expand = c(0, 0), limits = c(2440871, 6306970)) +
  theme(
    legend.key.height  = unit(6, "pt"),
    legend.key.width  = unit(6, "pt"),
    legend.title = element_text(size = 6.5),
    legend.text = element_text(size = 6.5),
    legend.position = "bottom",
    plot.margin = margin(t=0,b=5.5,l=0,r=0),
    plot.title = element_text(size = 7, vjust = -8, hjust = 0.05),
    panel.border = element_rect(colour = "grey30", fill=NA, linewidth=0.4),
    panel.grid.major = element_line(color = "grey90"))

ecoregions_map <- ggplot() +
  geom_spatraster(data = ecorast2, alpha = 1) +
  scale_fill_manual(
    name = NULL,
    values = c("#ed947e", "#99c17b", "#dbbb8e", "#c0ddf0", "#4faba0", "grey90"),
    breaks = c('8', '4', '7', '1', '6', 'Other'), 
    labels = c("Mediterr.", "Atlantic", "Contin.", "Alpine", "Boreal", "Other"),
    na.value = NA) +
  geom_spatvector(data = ctr2, color = "grey40", fill = NA, linewidth = 0.1) +
  theme_void() +
  scale_y_continuous(expand = c(0, 0), limits = c(1287903, 5459220)) +
  scale_x_continuous(expand = c(0, 0), limits = c(2440871, 6306970)) +
  theme(
    legend.key.height  = unit(5, "pt"),
    legend.key.width  = unit(5, "pt"),
    legend.key.spacing.x = unit(1, "pt"),
    legend.title = element_text(size = 6.5),
    legend.text = element_text(size = 6.5, margin = margin(l = 1,  r = 1.5)),
    legend.position = "bottom", legend.margin = margin(t=0,b=0,l=0,r=0),
    plot.margin = margin(t=0,b=0,l=0,r=0),
    plot.title = element_text(size = 7, vjust = -8, hjust = 0.05),
    panel.border = element_rect(colour = "grey30", fill=NA, linewidth=0.4),
    panel.grid.major = element_line(color = "grey90")) +
  annotate('text', y = 5200000, x = 2540871, label = 'd',
           angle = 0, color = "grey30", size = 9/.pt, hjust = 0)
    