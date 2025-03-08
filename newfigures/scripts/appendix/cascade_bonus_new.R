
simulations <- foreach(species = species_list, .combine=rbind) %do% {
  
  simulations_pbm <- readRDS(file = file.path(wd,"newfigures","data", "sims", paste0("pbm_", species, "_Europe", ".rds")))
  simulations_pbm <- simulations_pbm %>% filter(cal %in% c("expert", paste0("subset",rep(1:2, each = 5),"_rep", 1:5)))
  simulations_csdm <- readRDS(file = file.path(wd,"newfigures","data", "sims", paste0("csdm_", species, "_Europe", ".rds")))
  
  simulations <- rbind(simulations_pbm,simulations_csdm)
  simulations$method <- ifelse(simulations$cal == "expert", "expert", 
                               ifelse(simulations$cal %in% csdms, "csdm", "inverse"))
  simulations$species = species
  
  simulations
}

sdm_means <- simulations %>%
  group_by(method, gcm, ssp, year) %>% 
  reframe(sdm_mean = mean(y))

gcm_means <- simulations %>%
  group_by(method, gcm, ssp, year) %>% 
  reframe(y = mean(y)) %>%
  group_by(gcm, ssp, year) %>% 
  reframe(gcm_mean = mean(y))

ssp_means <- simulations %>%
  group_by(method, gcm, ssp, year) %>% 
  reframe(y = mean(y)) %>%
  group_by(gcm, ssp, year) %>% 
  reframe(y = mean(y)) %>% 
  group_by(ssp, year) %>% 
  reframe(ssp_mean = mean(y))

data <- simulations %>% 
  group_by(cal, method, gcm, ssp, year) %>% 
  reframe(ind_mean = mean(y)) %>%
  left_join(sdm_means, by = join_by(year, gcm, method, ssp)) %>%
  left_join(gcm_means, by = join_by(year, gcm, ssp)) %>%
  left_join(ssp_means, by = join_by(year, ssp)) %>%
  tidyr::pivot_longer(cols = c("ind_mean", "sdm_mean", "gcm_mean", "ssp_mean"),
                      names_to = "type", values_to = "x") %>%
  mutate(y = ifelse(type == "ind_mean", 0, 
                    ifelse(type == "sdm_mean", 1, 
                           ifelse(type == "gcm_mean", 2, 
                                  ifelse(type == "ssp_mean", 3, NA))))) %>%
  dplyr::filter(year == 2050 | year == 2090) %>%
  mutate(year = ifelse(year == 2050, "2040-2060", "2080-2100"))


data_csdm <- simulations %>% 
  dplyr::filter(method == 'csdm') %>%
  group_by(cal, method, gcm, ssp, year) %>% 
  reframe(ind_mean = mean(y)) %>%
  dplyr::filter(year == 2050 | year == 2090) 

cascade_all_models <- ggplot() +
  geom_rect(aes(xmin = min(data_csdm$ind_mean), xmax = max(data_csdm$ind_mean),
                ymin = 0, ymax = 3.8), col = NA, fill = 'grey80', alpha = 0.3) +
  geom_errorbar(aes(xmin = min(data_csdm$ind_mean), xmax = max(data_csdm$ind_mean),
                    y = 3.8), width = 0.2, col = 'grey30') +
  geom_text(data = data.frame(year = "2020-2040", y = 3.55, 
                              x = (min(data_csdm$ind_mean)+max(data_csdm$ind_mean))/2, text = "Only correlative models"),
            aes(x = x, y = y, label = text),
            color = "grey30", size = 2.4, hjust = 0.5) +
  geom_errorbar(aes(xmin = min(data$x), xmax = max(data$x),
                    y = 4.2), width = 0.2, col = 'grey30') +
  geom_text(data = data.frame(year = "2020-2040", y = 4.45, 
                              x = (min(data$x)+max(data$x))/2, text = "Full model range"),
            aes(x = x, y = y, label = text),
            color = "grey30", size = 2.5) +
  # ggh4x::facet_wrap2(~ year, nrow = 1, strip.position="left", axes = "all", remove_labels = "all") +
  geom_segment(data = data.frame(y = c(1,2,3), x = c(-0.2,-0.2,-0.2), xend = c(0.31,0.31,0.31)), aes(y = y, x = x, xend = xend),
             linetype = "dashed", color = "grey50", alpha = 0.8, linewidth = 0.4) +
  geom_line(data = data %>% dplyr::filter(y < 2 & method == "inverse" & year == yr) %>% unique(),
            aes(x = x, y = y, group = paste0(cal, method, ssp, gcm), 
                color = ssp, alpha = method), linewidth = 0.4) +
  geom_line(data = data %>% dplyr::filter(y < 2 & method == "csdm" & year == yr) %>% unique(),
            aes(x = x, y = y, group = paste0(cal, method, ssp, gcm), 
                color = ssp, alpha = method), linewidth = 0.4) +
  geom_line(data = data %>% dplyr::filter(y < 2 & method == "partial" & year == yr) %>% unique(),
            aes(x = x, y = y, group = paste0(cal, method, ssp, gcm), 
                color = ssp, alpha = method), linewidth = 0.4) +
  geom_line(data = data %>% dplyr::filter(y < 2 & method == "expert" & year == yr) %>% unique(),
            aes(x = x, y = y, group = paste0(cal, method, ssp, gcm), 
                color = ssp, alpha = method), linewidth = 0.4) +
  scale_alpha_manual(values = c("expert" = 0.5, "partial" = 0.2, "inverse" = 0.5, "csdm" = 0.5), guide = "none") +
  geom_line(data = data %>% dplyr::filter(y > 0 & y < 3 & year == yr),
            aes(x = x, y = y, group = paste0(method, ssp, gcm), color = ssp), alpha = 0.7, linewidth = 0.45) +
  geom_line(data = data %>% dplyr::filter(y > 1  & year == yr) %>% select (x,y,gcm, ssp) %>%  unique(),
            aes(x = x, y = y, group = paste0(gcm, ssp), color = ssp), alpha = 0.7, linewidth = 0.5) +
  geom_segment(data = data %>% dplyr::filter(y > 2  & year == yr) %>% select (x,y,ssp) %>%  unique(),
               aes(x = x, xend= x, y = y, yend = y+0.25, 
                   group = paste0(ssp), color = ssp), alpha = 1, linewidth = 0.5) +
  theme_bw() + 
  geom_text(data = data.frame(year = "2020-2040", y = c(0.35,1.35, 2.35, 3.35),
                              text = c("Individual calibrations\n(ensemble members)", "Ecological models\n(ensemble means)", "Climate models\n(ensemble means)", "SSPs\n(multi-model means)")),
            aes(y = y, label = text, x = 0.2), color = "grey50", size = 2.3, hjust = 0) +
  scale_color_manual(values = c(ssp245 = "#f79320", ssp585 = "#951b1e"), guide = "none") +
  labs(x = "Average projected change in climatic suitability\n(all species, relative to 1970-2000)") +
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(-0.2,0.2,0.1),
                     labels = c("-0.2", "", "0", "", "0.2")) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(clip = 'off', xlim = c(-.20,.20), ylim = c(0, 4.6)) + 
  geom_text(data = data.frame(year = "2020-2040", y = 2.4, 
                              x = c(0.095+0.03), text = c("SSP2-4.5")),
            aes(x = x, y = y, label = text),
            color = "#f69320", size = 2.3) +
  geom_text(data = data.frame(year = "2020-2040", y = 2.4, 
                              x = c(-0.02-0.03), text = c("SSP5-8.5")),
            aes(x = x, y = y, label = text),
            color = "#980002", size = 2.3) +
  theme(axis.line.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
        panel.grid = element_blank(), axis.ticks.y = element_blank(), panel.border = element_blank(),
        axis.line.x = element_line(color = "grey30"),
        axis.ticks.x = element_line(color = "grey30"),
        plot.margin = margin(t = 20, b = 5.5, r = 150, l = 7.5),
        strip.background = element_blank(), strip.text = element_text(vjust = 5, size = 8.5),
        axis.title.x = element_text(size = 7, color = "grey30"), axis.text.x = element_text(size = 6, color = "grey30"),
        legend.title = element_blank(), legend.text = element_text(size = 8.5, margin = margin(l=0, r = -3)),
        legend.position = 'none')
  
