country <- "Germany"

country_map <- ne_countries(scale="medium",returnclass = 'sf') %>%
  filter(sovereignt %in% country) %>%
  sf::st_crop(sf::st_bbox(c(xmin = -12, xmax = 45, ymax = 71, ymin = 32), crs = sf::st_crs(4326))) %>%
  vect()

fit <- mean(crop(csdm_r2090,fpbm_r2090), fpbm_r2090) %>%
  crop(country_map, mask=TRUE)
fit %>% global(mean, na.rm = T)
fit %>% global(sd, na.rm = T)

fit <- mean(epbm_r2090) %>%
  crop(country_map, mask=TRUE)
fit %>% global(mean, na.rm = T)
fit %>% global(sd, na.rm = T)
