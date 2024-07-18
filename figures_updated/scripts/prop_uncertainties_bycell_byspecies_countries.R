
data_barplot <- foreach(sp = species_list, .combine=rbind) %do% {
  
  cols <- c("ssp", "gcm", "sdm",
            "sdm.climate",
            "residuals")
  
  anova_sp <- anova_species %>%
    filter(species == sp) %>% 
    mutate(sdm.climate = gcm.sdm + ssp.sdm)
  
  anova_sp$typemax1 <- apply(anova_sp[,c("gcm", "ssp", "sdm", "gcm.ssp", "sdm.climate", "residuals")], 1, FUN = function(x) 
    names(which(x == sort(x, decreasing = TRUE)[1])))
  anova_sp$typemax2 <- apply(anova_sp[,c("gcm", "ssp", "sdm", "gcm.ssp", "sdm.climate", "residuals")], 1, FUN = function(x) 
    names(which(x == sort(x, decreasing = TRUE)[2])))
  
  anova_ss <- anova_sp
  anova_ss$tot <- rowSums(anova_ss[,c("gcm", "ssp", "sdm", "gcm.ssp", "sdm.climate", "residuals")])
  
  anova_r <- rast(lapply(cols, function(i)
    rast(anova_ss[c("x", "y", i)], type = "xyz")))
  crs(anova_r) <- "EPSG:4326"
  
  
  sim_r <- simulations %>% 
    filter(species == sp) %>%
    group_by(id_cell, x, y) %>%
    summarise(mean = mean(mean)) %>%
    ungroup() %>% 
    dplyr::select(x,y, mean) %>% 
    rast()
  crs(sim_r) <- "EPSG:4326"
  
  boot_mean <- function(x, i) mean(x[i])
  
  world_map <- ne_countries(scale="medium",returnclass = 'sf')
  
  anova_countries <- sapply(countries, function(country){
    cat(country)
    country_map <- world_map %>% 
      filter(sovereignt %in% country) %>%
      sf::st_crop(sf::st_bbox(c(xmin = -12, xmax = 45, ymax = 71, ymin = 32), crs = sf::st_crs(4326))) %>%
      vect()
    
    country_anova <- crop(anova_r, country_map, mask = TRUE)
    
    country_fit <- crop(sim_r, country_map, mask = TRUE)
    mean_fit <- t(global(country_fit, mean, na.rm = TRUE))
    sd_fit <- t(global(country_fit, sd, na.rm = TRUE))
    median_fit <- t(global(country_fit, median, na.rm = TRUE))
    
    # p_hat <- values(country_fit,na.rm=TRUE)
    # alpha <- 0.95
    # b <- boot::boot(p_hat, boot_mean, R = 5000)
    # ci <- boot::boot.ci(b, conf = alpha, type = "basic")  
    # boot.ci.min <- ci[["basic"]][4]
    # boot.ci.max <- ci[["basic"]][5]
    boot.ci.min <- NA
    boot.ci.max <- NA
    
    c(country, t(global(country_anova, mean, na.rm = TRUE)), mean_fit, sd_fit, median_fit, boot.ci.min, boot.ci.max)
  }) %>% t() %>% data.frame() 
  names(anova_countries) <- c("country", cols, "mean", "sd", "median", "boot.ci.min", "boot.ci.max")
  
  data_plot <- anova_countries %>%
    # tidyr::pivot_longer(cols = cols, names_to = "source", values_to = "prop") %>%
    mutate(mean = as.numeric(mean), 
           sd = as.numeric(sd),
           boot.ci.min = as.numeric(boot.ci.min), boot.ci.max = as.numeric(boot.ci.max),
           country = factor(country, levels = rev(c("Finland", "Sweden", "Poland","Germany", "Romania", "France", "Spain")))) %>%
    mutate_at(cols, as.numeric) %>%
    mutate(tot = ssp + gcm + sdm + sdm.climate + residuals, species = sp)
  
  data_plot
}

data_barplot$species.pos <- sapply(data_barplot$species, function(s) which(species_list == s)/10 + (which(species_list == s))*0.03)

cols <- c("residuals" = "#937668", "sdm" = "#7fa688", "sdm.climate" = "#ddb166",
          "gcm" = "#D98B65", ssp = "#a25752")

mid <- 0.39
wid <- 0.13

barplot_prop_uncertainties <- ggplot(data = data_barplot) +
  geom_rect(
    data = data.frame(ymin = rep(-0.65,7), ymax = rep(0.65,7),
                      xmin = seq(0.5, 6.5, 1), xmax = seq(1.5, 7.5, 1),
                      alpha = c("0","1","0","1","0","1","0")),
    aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, alpha = alpha),
    fill = "grey90") +
  scale_alpha_manual(values = c(0,0.5)) +
  # facet_wrap(~ species) +
  geom_hline(aes(yintercept = 0), colour = "white", linewidth=0.3) +
  geom_hline(aes(yintercept = 0), colour = "grey50", linewidth=0.3, linetype = "dashed") +
  geom_rect(aes(ymin = mean - 1.645*sd * (ssp + gcm + sdm.climate + sdm + residuals)/tot, 
                ymax = mean + 1.645*sd * (ssp + gcm + sdm.climate + sdm + residuals)/tot,
                xmin = as.numeric(country)-mid+(species.pos-wid), xmax = as.numeric(country)-mid+species.pos, fill = "residuals", group = species),
            color = NA) +
  geom_rect(aes(ymin = mean - 1.645*sd * (ssp + gcm + sdm.climate + sdm)/tot, 
                ymax = mean + 1.645*sd * (ssp + gcm + sdm.climate + sdm)/tot,
                xmin = as.numeric(country)-mid+(species.pos-wid), xmax = as.numeric(country)-mid+species.pos, fill = "sdm"),
            color = NA) +
  geom_rect(aes(ymin = mean - 1.645*sd * (ssp + gcm + sdm.climate)/tot, 
                ymax = mean + 1.645*sd * (ssp + gcm + sdm.climate)/tot,
                xmin = as.numeric(country)-mid+(species.pos-wid), xmax = as.numeric(country)-mid+species.pos, fill = "sdm.climate"),
            color = NA) +
  geom_rect(aes(ymin = mean - 1.645*sd * (ssp + gcm)/tot, 
                ymax = mean + 1.645*sd *  (ssp + gcm)/tot,
                xmin = as.numeric(country)-mid+(species.pos-wid), xmax = as.numeric(country)-mid+species.pos, fill = "gcm"),
            color = NA) +
  geom_rect(aes(ymin = mean - 1.645*sd * (ssp)/tot, 
                ymax = mean + 1.645*sd * (ssp)/tot,
                xmin = as.numeric(country)-mid+(species.pos-wid), xmax = as.numeric(country)-mid+species.pos, fill = "ssp"),
            color = NA) +
  
  
  geom_rect(aes(ymin = mean - 1.645*sd, ymax = mean + 1.645*sd,
                xmin = as.numeric(country)-mid+(species.pos-wid), xmax = as.numeric(country)-mid+species.pos),
            fill = NA, color = "grey30", linewidth = 0.2) +
  
  geom_errorbarh(aes(y = mean, 
                     xmin = as.numeric(country)-mid+(species.pos-wid) +0.01, 
                     xmax = as.numeric(country)-mid+species.pos-0.01), 
                 height = 0, color = "white", linewidth = 0.8) +
  # geom_point(aes(x = as.numeric(country)-mid+species.pos-(wid/2), y = mean), color = "white", size = 0.2) +
  # geom_point(aes(x = as.numeric(country)-mid+species.pos-(wid/2), y = mean), color = "grey30", size = 0.1) +
  geom_errorbarh(aes(y = mean, 
                     xmin = as.numeric(country)-mid+(species.pos-wid) +0.015, 
                     xmax = as.numeric(country)-mid+species.pos-0.015), 
                 height = 0, color = "grey30", linewidth = 0.4) +
  
  scale_fill_manual(name = "", 
                    breaks = c("ssp", "gcm", "sdm.climate", "sdm", "residuals"),
                    labels = c("SSP", "GCM", "Climate - SDM", "SDM", "Residuals"),
                    values=cols) +
  
  theme_bw() +
  theme(
    plot.margin = margin(t = 0, b = 0, r = 5.5, l = 8.5),
    legend.position = 'top', legend.text = element_text(size = 6.5, margin = margin(l = 1,  r = 2.5)),
    legend.key.height  = unit(6, "pt"), legend.key.width  = unit(6, "pt"),
    panel.border = element_rect(colour = "grey30", fill=NA, linewidth=0.4),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(), axis.title.x = element_blank(),
    axis.title = element_text(size = 8.5),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0, size = 8)) +
  scale_y_continuous(position = "right", expand = c(0,0), limits = c(-0.65,0.65),
                     breaks = round(seq(-0.6, 0.6, 0.2),1)) +
  labs(y = "Average projected change in suitability") +
  scale_x_continuous(
    limits = c(0.5,7.5), expand = c(0,0),
    breaks=seq(1,7,1), 
    labels = levels(data_barplot$country))

