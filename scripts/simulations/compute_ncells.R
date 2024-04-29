library(future.apply)


country <- "France"
calibrations <- (paste0("subset",rep(1:10, each = 10),"_rep", 1:10))

# compute number of cells potentially occupied
plan(multisession, workers = 5)
ncells <- as.data.frame(do.call(rbind, future_lapply(calibrations, function(c){
  
  # get country extent
  world_map <- ne_countries(scale="medium",returnclass = 'sf')
  country_map <- world_map %>% 
    filter(sovereignt %in% country) %>%
    sf::st_crop(sf::st_bbox(c(xmin = -12, xmax = 45, ymax = 71, ymin = 32), crs = sf::st_crs(4326))) %>%
    vect()
  
  suit <- rast(file.path(dir, "data", "processed", species, "bin", paste0(c, ".tif"))) %>% 
    crop(country_map, mask = T)
  
  ncells <- as.data.frame(do.call(rbind, lapply(gcms, function(m){
    indices <- which(names(suit) == paste0(m, "_ref"))
    ref <- sapply(subset(suit,indices), function(i) ncell(i[i == 1]))
    ncells <- as.data.frame(do.call(rbind, lapply(scenarios, function(s){
      indices <- which(names(suit) == paste0(m, "_", s))
      ncells <- sapply(subset(suit,indices), function(i) ncell(i[i == 1]))
      return(data.frame(ncells, ssp = s, date = time(subset(suit,indices))))
    })))
    ncells$gcm <- m
    ncells$ref <- ref
    return(ncells)
  })))
  
  ncells$cal <- c
  
  return(ncells)
})))
plan(sequential);gc()

library(tidyterra)
library(patchwork)

country_map <- ggplot() + 
  geom_spatvector(data = country_map) +
  theme_void()

ggplot(data = ncells, aes(x = date, y = (ncells-ref)/ref*100, group = paste0(cal, gcm),
                          col = gcm)) +
  facet_wrap(~ ssp) + 
  # geom_point() +
  geom_line(alpha = 0.5) +
  stat_summary(
    aes(x = date, y = (ncells-ref)/ref*100), inherit.aes = FALSE,
    geom="line", fun = "median", color="black", linewidth=1) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(), legend.position = 'none') +
  inset_element(country, l = -0.1, r = 0.4,  t = 0.7, b = 0.1, clip = FALSE)

ncells$col <- ifelse(ncells$date <= as.Date('2015-01-01'), "historical", ncells$ssp)

cols <- c("historical" = "black", "ssp245" = "#F79420", "ssp585" = "#951B1E")
fils <- c("historical" = "black", "ssp245" = "#F79420", "ssp585" = "#951B1E")

ggplot(data = ncells, aes(x = date, y = (ncells-ref)/ref*100,
       color = col, fill = col)) +
  scale_colour_manual(values = cols) +
  scale_fill_manual(values = fils) +
  geom_line(aes(group = paste0(gcm, ssp, cal)), linewidth = 0.1, alpha = 0.15) +
  stat_summary(geom="ribbon", alpha = 0.2, linewidth = 0.4, linetype = "dashed",
               fun.min = function(x) quantile(x, 0.05), 
               fun.max = function(x) quantile(x, 0.95)) +
  stat_summary(geom="line", fun = "median", linewidth=1.1, color = "white") +
  stat_summary(geom="line", fun = "median", linewidth=0.7) +
  geom_vline(xintercept = as.Date('2015-07-02')) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(), legend.position = 'none') +
  labs(x = "Year", y = "Relative to 1970-2000 (%)") +
  ggtitle(paste0("F. sylvatica suitable area, ", country," (15-year running averages)")) +
  coord_cartesian(expand = FALSE, ylim = c(-100, 10)) +
  inset_element(country_map, l = -0.1, r = 0.35,  t = 0.7, b = 0.1, clip = FALSE) 
