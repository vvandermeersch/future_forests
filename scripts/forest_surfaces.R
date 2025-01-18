
forest_dir <- "D:/land_use/global"

if(reload_data){
  plan(multisession, workers = 2)
  forest_surf <- future_lapply(list.files(forest_dir), function(tile){
    cat(tile)
    ecoregions <- rast(file.path(wd, "figures_cleaned", "data", "sims", "ecoregions_rast.rds")) %>% project("EPSG:4326") %>% 
      crop(ext(-10.5, 31.7, 34.6, 71.2))
    forest_cover <- rast(file.path(forest_dir, tile)) %>%
      aggregate(1000, sum)/1000^2
    crs(forest_cover) <- "EPSG:4326"
    cell_size <- cellSize(forest_cover, unit = "ha")
    forest_surf <- (forest_cover*cell_size) %>% crop(ext(-10.5, 31.7, 34.6, 71.2))
    er <- ecoregions %>% project(forest_surf) %>% crop(ext(forest_surf)) 
    forest_surf_df <- data.frame(zonal(forest_surf, er, sum, na.rm = TRUE), tile = tile)
    names(forest_surf_df) <- c("ecoregion", "surface", "tile")
    list(wrap(forest_surf), forest_surf_df)
  })
  plan(sequential);gc()
  saveRDS(forest_surf, file.path(wd,"figures_cleaned","data","europe_forest_surf.rds"))
}else{
  forest_surf <- readRDS(file.path(wd,"figures_cleaned","data","europe_forest_surf.rds"))
}

forest_surf_df <- lapply(1:length(forest_surf), function(i) data.frame(forest_surf[[i]][2]))
forest_surf_df <- as.data.frame(do.call(rbind, forest_surf_df))
tot_surf <- sum(forest_surf_df$surface)
forest_surf_df <- forest_surf_df %>%
  group_by(ecoregion) %>%
  summarise(
    surface_tot = sum(surface)/1e6,
    proportion = sum(surface)/tot_surf*100)

forest_surf_r <- lapply(1:length(forest_surf), function(i) unwrap(forest_surf[[i]][[1]])) %>% sprc()

europe_forest <- mosaic(forest_surf_r, fun = "first") %>% 
  crop(ext(-10.5, 31.7, 34.6, 71.2))

ecoregions <- rast(file.path(wd, "figures_cleaned", "data", "sims", "ecoregions_rast.rds")) %>% project("EPSG:4326") %>% 
  crop(ext(-10.5, 31.7, 34.6, 71.2)) %>%
  project(europe_forest)

europe_forest <- mask(europe_forest, ecoregions)
europe_forest <- mask(focal(europe_forest, w=3, fun=mean, na.policy="only", na.rm=T), ecoregions) # plotting issue

europe_forest_map <- ggplot() +
  geom_spatraster(data = europe_forest %>% project("EPSG:3035"),
                  maxcell = 2e+05) +
  scale_fill_gradient2(low = "#f0ead2", mid = "#7ABA78", high = "#0A6847", na.value = NA,
                       midpoint = 3000, name = "Forest area (hectares)") +
  theme_void() +
  scale_y_continuous(expand = c(0, 0), limits = c(1298120, 5459220)) +
  scale_x_continuous(expand = c(0, 0), limits = c(2441580, 6306970)) +
  theme(
    legend.key.height  = unit(5, "pt"),
    legend.key.width  = unit(100, "pt"),
    legend.title = element_text(size = 6.7, face = "bold"),
    legend.text = element_text(size = 6.5),
    legend.position = "bottom", 
    legend.box.margin = margin(t = 0, b = 0, r = 0, l = 0),
    panel.border = element_rect(colour = "grey30", fill=NA, linewidth=0.4),
    panel.grid.major = element_line(color = "grey90"))





