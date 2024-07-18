
# historical distribution
plan(multisession, workers = ncores)
.hist_dist <- future_lapply(calibrations, function(c){
  
  suit <- rast(file.path(wd, "data", "processed", species, "suit", paste0(c, ".tif")))
  
  dist <- lapply(gcms, function(m){
    
    ths <- readRDS(file.path(wd, "data", "fit", m, species, paste0(c, ".rds")))$best_threshold
    
    indices <- which(names(suit) == paste0(m, "_hist") & time(suit, format ="years") %in% c(1970:2000))
    
    fitness <- mean(subset(suit,indices))
    dist <- ifel(fitness < ths, 0, 1)
    
    return(dist)
  }) %>% rast() %>% sum()
  
  return(wrap(dist))
}) 
hist_dist <- lapply(.hist_dist, unwrap) %>% rast() %>% sum()

plan(sequential)
names(hist_dist) <- "historical"

dist <- c(hist_dist, sim_distribution) %>% crop(ext(-12.05, 40.05, 34.65, 71.15)) %>% project("EPSG:3035")
dist <- ifel(dist >= ths_ag_dist, 1, 0) 

cstdist <- ifel(dist$historical == 1 & dist$`2090` == 1, 1, 0)
maskd <- ifel(cstdist == 1, 1, NA)

ctr <- dist$historical
ctr <- ifel(is.na(ctr), 0, 1) %>% as.polygons() 

chgdist <- dist$`2090`-dist$historical
chgdist <- ifel(chgdist == 0, NA, chgdist)
nochg <- ifel(cstdist == 1, 0, NA)
chgdist <- sum(chgdist,nochg, na.rm = TRUE) %>%
  mutate(`2090` = factor(`2090`))

sim_fitness_rp <- sim_fitness %>% crop(ext(-12.05, 40.05, 34.65, 71.15)) %>% project("EPSG:3035")
sim_agreement_rp <- sim_agreement %>% crop(ext(-12.05, 40.05, 34.65, 71.15)) %>% project("EPSG:3035")
decsuit <- ifel(sim_fitness_rp$`2090` < -0.1 & sim_agreement_rp$`2090` ==1, -1, NA) %>% 
  mask(maskd) %>% 
  mutate(`2090` = factor(`2090`)) %>% 
  as.polygons()
names(decsuit) <- "dec"

dist_map_less <- ggplot() +
  geom_spatraster(data = cstdist) +
  scale_fill_gradient(low = "grey98", high = NA, na.value = NA, guide="none") +
  ggnewscale::new_scale_fill() +
  geom_spatraster(data = chgdist) +
  scale_fill_manual(
    name = "",
    breaks = c(-1, -0.5, 0, 0.5, 1),
    values = c(`-1`="#a63716", NA, `0`= "#c7cdd1", NA, `1`="#15607a"),  
    labels = c("No more suitable", "Less suitable", "Still suitable", "More suitable", "Newly suitable"),
    na.value = NA) +
  geom_sf_pattern(data = decsuit, 
                  aes(pattern = dec, pattern_fill = dec),
                  color = NA, fill = NA,
                  pattern_colour = NA,
                  pattern_angle = 45,
                  pattern_density = 0.2,
                  pattern_spacing = 0.01,
                  pattern_key_scale_factor = 3) +
  scale_pattern_manual(
    name = "",
    breaks = c(NA, -1, NA, 1, NA),
    values = c(NA, `-1` = "stripe", NA, `1` = "stripe", NA),
    labels = c("No more suitable", "Less suitable", "Still suitable", "More suitable", "Newly suitable")) +
  scale_pattern_fill_manual(
    name = "",
    breaks = c(NA, -1, NA, 1, NA),
    values = c(NA, `-1` = "#a63716", NA, `1` = NA, NA),
    labels = c("No more suitable", "Less suitable", "Still suitable", "More suitable", "Newly suitable")) +
  geom_spatvector(data = ctr, color = "grey40", fill = NA, linewidth = 0.1) +
  theme_void() +
  guides(
    pattern = guide_legend(override.aes = list(fill = NA))) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(
    panel.border = element_rect(colour = "grey80", fill=NA, linewidth=0.7),
    panel.grid.major = element_line(color = "grey90"))

decsuit <- ifel(sim_fitness_rp$`2090` < -0.1 & sim_agreement_rp$`2090` ==1, -1, 
                ifel(sim_fitness_rp$`2090` > 0.1 & sim_agreement_rp$`2090` ==1, 1, NA)) %>% 
  mask(maskd) %>% 
  mutate(`2090` = factor(`2090`)) %>% 
  as.polygons()
names(decsuit) <- "dec"

dist_map_lessmore <- ggplot() +
  geom_spatraster(data = cstdist) +
  scale_fill_gradient(low = "grey98", high = NA, na.value = NA, guide="none") +
  ggnewscale::new_scale_fill() +
  geom_spatraster(data = chgdist) +
  scale_fill_manual(
    name = "",
    breaks = c(-1, -0.5, 0, 0.5, 1),
    values = c(`-1`="#a63716", NA, `0`= "#c7cdd1", NA, `1`="#15607a"),  
    labels = c("No more suitable", "Less suitable", "Still suitable", "More suitable", "Newly suitable"),
    na.value = NA) +
  geom_sf_pattern(data = decsuit, 
                  aes(pattern = dec, pattern_fill = dec),
                  color = NA, fill = NA,
                  pattern_colour = NA,
                  pattern_angle = 45,
                  pattern_density = 0.2,
                  pattern_spacing = 0.01,
                  pattern_key_scale_factor = 3) +
  scale_pattern_manual(
    name = "",
    breaks = c(NA, -1, NA, 1, NA),
    values = c(NA, `-1` = "stripe", NA, `1` = "stripe", NA),
    labels = c("No more suitable", "Less suitable", "Still suitable", "More suitable", "Newly suitable")) +
  scale_pattern_fill_manual(
    name = "",
    breaks = c(NA, -1, NA, 1, NA),
    values = c(NA, `-1` = "#a63716", NA, `1` = "#15607a", NA),
    labels = c("No more suitable", "Less suitable", "Still suitable", "More suitable", "Newly suitable")) +
  geom_spatvector(data = ctr, color = "grey40", fill = NA, linewidth = 0.1) +
  theme_void() +
  guides(
    pattern = guide_legend(override.aes = list(fill = NA))) +
  #scale_x_continuous(limits = c(2431478, 6800000)) +
  #scale_y_continuous(limits = c(1288120, 5571562)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(
    panel.border = element_rect(colour = "grey80", fill=NA, linewidth=0.7),
    panel.grid.major = element_line(color = "grey90"))

saveRDS(dist_map_lessmore, file = file.path(wd,"figures_cleaned","data","maps", paste0(species, "_", type, "_2090_dist_", scenario, ".rds")))
