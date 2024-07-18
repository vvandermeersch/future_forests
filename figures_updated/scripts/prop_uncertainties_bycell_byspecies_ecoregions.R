
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
  
  sim_r <- rast(lapply(unique(simulations$ssp), function(S){
    rast(lapply(unique(simulations$gcm), function(M){
      rast(lapply(unique(simulations$method), function(m){
        simulations %>% 
          filter(ssp == S, gcm == M, method == m, species == sp) %>%
          group_by(id_cell, x, y) %>%
          summarise(mean = mean(mean)) %>%
          ungroup() %>% 
          dplyr::select(x,y, mean) %>% rast()
      }))
    }))
  }))
  
  crs(sim_r) <- "EPSG:4326"
  
  #boot_mean <- function(x, i) mean(x[i])
  
  anova_ecoregions <- sapply(ecoregions, function(ecoreg){
    cat(ecoreg)
    
    ecoreg_map <- ecorast_wgs84 %>% filter(val == ecoreg)
    
    ecoreg_anova <- crop(anova_r, ecoreg_map, mask = TRUE)
    
    ecoreg_fit <- crop(sim_r, ecoreg_map, mask = TRUE)
    mean_fit <- mean(t(global(ecoreg_fit, mean, na.rm = TRUE)))
    var_fit <- t(global(ecoreg_fit, var, na.rm = TRUE))
    sd_fit <- sqrt(mean(var_fit))
    # median_fit <- median(t(global(ecoreg_fit, median, na.rm = TRUE)))
    
    # p_hat <- values(country_fit,na.rm=TRUE)
    # alpha <- 0.95
    # b <- boot::boot(p_hat, boot_mean, R = 5000)
    # ci <- boot::boot.ci(b, conf = alpha, type = "basic")  
    # boot.ci.min <- ci[["basic"]][4]
    # boot.ci.max <- ci[["basic"]][5]
    boot.ci.min <- NA
    boot.ci.max <- NA
    
    c(ecoreg, t(global(ecoreg_anova, mean, na.rm = TRUE)), mean_fit, sd_fit, boot.ci.min, boot.ci.max)
  }) %>% t() %>% data.frame() 
  names(anova_ecoregions) <- c("ecoregion", cols, "mean", "sd", "boot.ci.min", "boot.ci.max")
  
  data_plot <- anova_ecoregions %>%
    # tidyr::pivot_longer(cols = cols, names_to = "source", values_to = "prop") %>%
    mutate(mean = as.numeric(mean), 
           sd = as.numeric(sd),
           boot.ci.min = as.numeric(boot.ci.min), boot.ci.max = as.numeric(boot.ci.max),
           ecoregion = factor(ecoregion, levels = rev(c("Boreal", "Alpine", "Continental", "Atlantic", "Mediterranean")))) %>%
    mutate_at(cols, as.numeric) %>%
    mutate(tot = ssp + gcm + sdm + sdm.climate + residuals, species = sp)
  
  data_plot
}

data_barplot$species.pos <- sapply(data_barplot$species, function(s) which(species_list == s)/10 + (which(species_list == s))*0.03)

cols <- c("residuals" = "#937668", "sdm" = "#7fa688", "sdm.climate" = "#ddb166",
          "gcm" = "#D98B65", "ssp" = "#a25752") #"#BD8985"

mid <- 0.39
wid <- 0.13

barplot_prop_uncertainties <- ggplot(data = data_barplot) +
  geom_rect(
    data = data.frame(ymin = rep(-0.65,7), ymax = rep(0.85,7),
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
                xmin = as.numeric(ecoregion)-mid+(species.pos-wid), xmax = as.numeric(ecoregion)-mid+species.pos, fill = "residuals", group = species),
            color = NA) +
  geom_rect(aes(ymin = mean - 1.645*sd * (ssp + gcm + sdm.climate + sdm)/tot, 
                ymax = mean + 1.645*sd * (ssp + gcm + sdm.climate + sdm)/tot,
                xmin = as.numeric(ecoregion)-mid+(species.pos-wid), xmax = as.numeric(ecoregion)-mid+species.pos, fill = "sdm"),
            color = NA) +
  geom_rect(aes(ymin = mean - 1.645*sd * (ssp + gcm + sdm.climate)/tot, 
                ymax = mean + 1.645*sd * (ssp + gcm + sdm.climate)/tot,
                xmin = as.numeric(ecoregion)-mid+(species.pos-wid), xmax = as.numeric(ecoregion)-mid+species.pos, fill = "sdm.climate"),
            color = NA) +
  geom_rect(aes(ymin = mean - 1.645*sd * (ssp + gcm)/tot, 
                ymax = mean + 1.645*sd *  (ssp + gcm)/tot,
                xmin = as.numeric(ecoregion)-mid+(species.pos-wid), xmax = as.numeric(ecoregion)-mid+species.pos, fill = "gcm"),
            color = NA) +
  geom_rect(aes(ymin = mean - 1.645*sd * (ssp)/tot, 
                ymax = mean + 1.645*sd * (ssp)/tot,
                xmin = as.numeric(ecoregion)-mid+(species.pos-wid), xmax = as.numeric(ecoregion)-mid+species.pos, fill = "ssp"),
            color = NA) +
  
  
  geom_rect(aes(ymin = mean - 1.645*sd, ymax = mean + 1.645*sd,
                xmin = as.numeric(ecoregion)-mid+(species.pos-wid), xmax = as.numeric(ecoregion)-mid+species.pos),
            fill = NA, color = "grey30", linewidth = 0.2) +
  
  geom_errorbarh(aes(y = mean, 
                     xmin = as.numeric(ecoregion)-mid+(species.pos-wid) +0.01, 
                     xmax = as.numeric(ecoregion)-mid+species.pos-0.01), 
                 height = 0, color = "white", linewidth = 0.8) +
  # geom_point(aes(x = as.numeric(ecoregion)-mid+species.pos-(wid/2), y = mean), color = "white", size = 0.2) +
  # geom_point(aes(x = as.numeric(ecoregion)-mid+species.pos-(wid/2), y = mean), color = "grey30", size = 0.1) +
  geom_errorbarh(aes(y = mean, 
                     xmin = as.numeric(ecoregion)-mid+(species.pos-wid) +0.015, 
                     xmax = as.numeric(ecoregion)-mid+species.pos-0.015), 
                 height = 0, color = "grey30", linewidth = 0.4) +
  
  scale_fill_manual(name = "", 
                    breaks = c("ssp", "gcm", "sdm.climate", "sdm", "residuals"),
                    labels = c("SSP", "GCM", "Climate - SDM", "SDM", "Residuals"),
                    values=cols) +
  
  theme_bw() +
  theme(
    plot.margin = margin(t = 0, b = 0, r = 5.5, l = 10.5),
    legend.position = 'top', legend.text = element_text(size = 6.5, margin = margin(l = 1,  r = 2.5)),
    legend.key.height  = unit(6, "pt"), legend.key.width  = unit(6, "pt"),
    panel.border = element_rect(colour = "grey30", fill=NA, linewidth=0.4),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(), axis.title.x = element_blank(),
    axis.title = element_text(size = 8.5),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(angle = 0, size = 7, vjust = 0)) +
  scale_y_continuous(position = "right", expand = c(0,0), limits = c(-0.65,0.85),
                     breaks = round(seq(-0.6, 0.8, 0.2),1)) +
  labs(y = "Average projected change in suitability") +
  scale_x_continuous(
    limits = c(0.5,5.5), expand = c(0,0),
    breaks=seq(1,5,1), 
    labels = levels(data_barplot$ecoregion))

source(file.path(wd, "figures_updated", "scripts", "inset_legend.R"))

# ggdraw() +
#   draw_plot(barplot_prop_uncertainties + theme(legend.position = 'none')) +
#   draw_plot(insetlegend, x = 0.65, y = .05, width = .22, height = .29)
