
# Model agreement on sign of fitness change
sign <- lapply(c("correlative","expert","fitted"), function(m){
  
  
  sim_r <- simulations %>% 
    dplyr::filter(method == m) %>%
    group_by(x,y) %>%
    summarise(suit = mean(mean)) %>%
    rast(crs = "EPSG:4326") %>% 
    project("EPSG:3035")
  
  sign <- ifel(sim_r < 0, -1, 1)
  
  return(sign)
  
}) %>% rast()
neg <- ifel(sign < 0, 1, 0) %>% sum()
neg <- ifel(neg < 3, 0, 1)
pos <- ifel(sign > 0, 1, 0) %>% sum()
pos <- ifel(pos < 3, 0, 1)
agreement <- pos + neg

agreement <- resample(agreement, ecorast2)

sim_agreement <- ifel(agreement == 0, 0, NA) %>% 
  crop(ecorast2) %>%
  mask(ecorast2) %>%
  as.polygons()  %>% 
  simplifyGeom(tolerance=10000, preserveTopology=TRUE, makeValid=FALSE) # less heavy plot
  
values(sim_agreement) <- "Low"

suit_r <- simulations %>%
  group_by(x,y) %>%
  summarise(suit = mean(mean)) %>%
  rast(crs = "EPSG:4326") %>% 
  project("EPSG:3035") %>%
  resample(ecorast2) %>%
  mask(ecorast)

map_suitability <- ggplot() +
  geom_spatraster(data = suit_r, maxcell = 1e+05) +
  geom_sf_pattern(data = sim_agreement, 
                  aes(pattern = value),
                  color = NA, fill = NA,
                  pattern_colour = NA,
                  pattern_fill = "grey50",
                  pattern_angle = 45,
                  pattern_density = 0.2,
                  pattern_spacing = 0.015,
                  pattern_key_scale_factor = 3) +
  scale_pattern_manual(values = c(Low = "stripe"), labels = c("Low model agreement")) +
  geom_spatvector(data = ctr2 %>%  simplifyGeom(tolerance=5000, preserveTopology=TRUE, makeValid=FALSE), 
                  color = "grey40", fill = NA, linewidth = 0.1) +
  theme_void() +
  scale_y_continuous(expand = c(0, 0), limits = c(1287903, 5459220)) +
  scale_x_continuous(expand = c(0, 0), limits = c(2440871, 6306970)) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 6.5),
    legend.position = "bottom", legend.box="vertical",
    legend.box.margin = margin(t = 0, b = 0, r = 0, l = 0),
    plot.margin = margin(t=5.5,b=0,l=0,r=0),
    strip.text = element_text(size = 8, margin = margin(t=0,b=5,l=0,r=0)),
    panel.border = element_rect(colour = "grey30", fill=NA, linewidth=0.4),
    panel.grid.major = element_line(color = "grey90")) +
  scale_fill_gradient2(
    low = "#a63716",
    mid = "#eef0f1",
    high = "#15607a",
    na.value = "transparent", 
    limits = c(-0.45, 0.45),
    breaks = seq(-0.4, 0.4, 0.2), 
    labels = c("-0.4", "-0.2", "0", "0.2", "0.4")
  ) +
  guides(
    fill = guide_colorbar(order = 2,
                          frame.colour = "grey30", ticks.colour = NA,
                          frame.linewidth = 0.2,
                          theme = theme(legend.key.height  = unit(5, "pt"),
                                        legend.key.width  = unit(120, "pt"),
                                        legend.text = element_text(size = 6.5, margin = margin(t = 3.5)))),
    pattern = guide_legend(override.aes = list(fill = "white", color = "grey30"), order = 1,
                           theme = theme(legend.key.height  = unit(6, "pt"),
                                         legend.text = element_text(size = 7))))

ctratlmed <- ifel(ecorast2 %in% c('8', '4', '7', 'Other') & suit_r <= 0 & agreement == 1, 1, 0) %>% as.polygons() %>% disagg()
ctratlmed$exp <- expanse(ctratlmed)
ctratlmed <- ctratlmed[ctratlmed$exp > 5e9 & ctratlmed$exp < 1e13] %>% terra::aggregate() %>% fillHoles()
ctratlmed_pt <- vect(data.frame(x = 3.1e6, y = 2e6), geom = c('x','y'), crs="EPSG:3035")
ctratlmed_line <- vect(rbind(c(3.1e6, 2e6), c(2440871-.5e6, 2e6)),  type="lines",  crs="EPSG:3035")

ctrcont <- ifel(ecorast2 %in% c('7', '1', '4', '6', 'Other') & agreement == 0, 1, 0) %>% as.polygons() %>% disagg()
ctrcont$exp <- expanse(ctrcont)
ctrcont <- ctrcont[ctrcont$exp > 5e9 & ctrcont$exp < 1e13] %>% terra::aggregate() %>% fillHoles()
ctrcont_pt <- vect(data.frame(x = 5.3e6, y = 3.2e6), geom = c('x','y'), crs="EPSG:3035")
ctrcont_line <- vect(rbind(c(5.3e6, 3.2e6), c(6306970+.5e6, 3.2e6)),  type="lines",  crs="EPSG:3035")

ctrbor <- ifel(ecorast2 %in% c('1', '6', '4') & suit_r > 0 & agreement == 1, 0, 1) %>% as.polygons() %>% disagg()
ctrbor$exp <- expanse(ctrbor)
ctrbor <- ctrbor[ctrbor$exp > 5e9 & ctrbor$exp < 1e13] %>% terra::aggregate() %>% fillHoles()
ctrbor_pt <- vect(data.frame(x = 4.55e6, y = 4.5e6), geom = c('x','y'), crs="EPSG:3035")
ctrbor_line <- vect(rbind(c(4.55e6, 4.5e6), c(2440871-.5e6, 4.5e6)),  type="lines",  crs="EPSG:3035")


vect(rbind(c(1, 0), c(3, 0)),  type="lines",  crs="+proj=longlat")

map_suitability_3zones <-ggplot() +
  geom_spatraster(data = suit_r, maxcell = 1e+05) +
  geom_sf_pattern(data = sim_agreement, 
                  aes(pattern = value),
                  color = NA, fill = NA,
                  pattern_colour = NA,
                  pattern_fill = "grey50",
                  pattern_angle = 45,
                  pattern_density = 0.2,
                  pattern_spacing = 0.015,
                  pattern_key_scale_factor = 3) +
  scale_pattern_manual(values = c(Low = "stripe"), labels = c("Low model agreement")) +
  geom_spatvector(data = ctr2 %>%  simplifyGeom(tolerance=5000, preserveTopology=TRUE, makeValid=FALSE), 
                  color = "grey40", fill = NA, linewidth = 0.1) +
  theme_void() +
  
  # scale_y_continuous(expand = c(0, 0), limits = c(1287903, 5459220)) +
  # scale_x_continuous(expand = c(0, 0), limits = c(2440871, 6306970)) +
  
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 6.5),
    legend.position = "bottom", legend.box="vertical",
    legend.box.margin = margin(t = 0, b = 0, r = 0, l = 0),
    plot.margin = margin(t=5.5,b=5.5,l=10,r=10),
    strip.text = element_text(size = 8, margin = margin(t=0,b=5,l=0,r=0)),
    panel.border = element_rect(colour = "grey30", fill=NA, linewidth=0.4),
    panel.grid.major = element_line(color = "grey90")) +
  scale_fill_gradient2(
    low = "#a63716",
    mid = "#eef0f1",
    high = "#15607a",
    na.value = "transparent", 
    limits = c(-0.45, 0.45),
    breaks = seq(-0.4, 0.4, 0.2), 
    labels = c("-0.4", "-0.2", "0", "0.2", "0.4")
  ) +
  guides(
    fill = guide_colorbar(order = 2,
                          frame.colour = "grey30", ticks.colour = NA,
                          frame.linewidth = 0.2,
                          theme = theme(legend.key.height  = unit(5, "pt"),
                                        legend.key.width  = unit(120, "pt"),
                                        legend.text = element_text(size = 6.5, margin = margin(t = 3.5)))),
    pattern = guide_legend(override.aes = list(fill = "white", color = "grey30"), order = 1,
                           theme = theme(legend.key.height  = unit(6, "pt"),
                                         legend.text = element_text(size = 7)))) +
  geom_spatvector(data = ctratlmed, 
                  color = "white", fill = NA, linewidth = 1.5) +
  geom_spatvector(data = ctratlmed, 
                  color = "#a63716", fill = NA, linewidth = 0.4) +
  
  geom_spatvector(data = ctrcont, 
                  color = "white", fill = NA, linewidth = 1.5) +
  geom_spatvector(data = ctrcont, 
                  color = "#607a15", fill = NA, linewidth = 0.4) +
  geom_spatvector(data = ctrbor, 
                  color = "white", fill = NA, linewidth = 1.5) +
  geom_spatvector(data = ctrbor, 
                  color = "#15607a", fill = NA, linewidth = 0.4) +
  
  geom_spatvector(data = ctratlmed_pt, 
                  color = "#a63716", fill = NA, size = 2.2) +
  geom_spatvector(data = ctratlmed_line, 
                  color = "#a63716", fill = NA, size = .5) +
  geom_spatvector(data = ctratlmed_pt, 
                color = "white", fill = 'white', size = 1.5) +
  geom_spatvector(data = ctratlmed_pt, 
                  color = "#a63716", fill = NA, size = 0.4) +
  
  geom_spatvector(data = ctrcont_pt, 
                  color = "#607a15", fill = NA, size = 2.2) +
  geom_spatvector(data = ctrcont_line, 
                 color = "#607a15", fill = NA, size = .5) +
  geom_spatvector(data = ctrcont_pt, 
                  color = "white", fill = NA, size = 1.5) +
  geom_spatvector(data = ctrcont_pt, 
                  color = "#607a15", fill = NA, size = 0.4) +
  
  geom_spatvector(data = ctrbor_pt, 
                  color = "#15607a", fill = NA, size = 2.2) +
  geom_spatvector(data = ctrbor_line, 
                  color = "#15607a", fill = NA, size = .5) +
  geom_spatvector(data = ctrbor_pt, 
                  color = "white", fill = NA, size = 1.5) +
  geom_spatvector(data = ctrbor_pt, 
                  color = "#15607a", fill = NA, size = 0.4) + 
  coord_sf(clip = 'off', xlim = c(2440871, 6306970),
           ylim = c(1287903, 5459220), expand = FALSE) 

map_suitability_3zones
