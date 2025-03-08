
gcm_means <- simulations %>%
  group_by(gcm, method, ssp, year) %>% 
  reframe(gcm_mean = mean(y))

method_means <- simulations %>%
  group_by(method, ssp, year) %>% 
  reframe(method_mean = mean(y))

ssp_means <- simulations %>%
  group_by(method, ssp, year) %>% 
  reframe(y = mean(y)) %>% 
  group_by(ssp, year) %>% 
  reframe(ssp_mean = mean(y))


data <- simulations %>% 
  mutate(ind_mean = y) %>% 
  select(-y) %>% 
  left_join(gcm_means, by = join_by(year, gcm, method, ssp)) %>%
  left_join(method_means, by = join_by(year, method, ssp)) %>%
  left_join(ssp_means, by = join_by(year, ssp)) %>%
  tidyr::pivot_longer(cols = c("ind_mean", "gcm_mean", "method_mean", "ssp_mean"),
                      names_to = "type", values_to = "x") %>%
  mutate(y = ifelse(type == "ind_mean", 0, 
                    ifelse(type == "gcm_mean", 1, 
                           ifelse(type == "method_mean", 2, 
                                  ifelse(type == "ssp_mean", 3, NA))))) %>%
  dplyr::filter(year == 2050 | year == 2090) %>%
  mutate(year = ifelse(year == 2050, "2040-2060", "2080-2100"))

yr <- "2080-2100"
cascade_all_models <- ggplot() +
  # ggh4x::facet_wrap2(~ year, nrow = 1, strip.position="left", axes = "all", remove_labels = "all") +
  geom_hline(data = data.frame(y = c(1,2,3)), aes(yintercept = y),
             linetype = "dashed", color = "grey70", alpha = 0.8) +
  geom_line(data = data %>% dplyr::filter(y < 2 & method == "inverse" & year == yr) %>% unique(),
            aes(x = x, y = y, group = paste0(cal, method, ssp, gcm), 
                color = ssp, alpha = method), linewidth = 0.5) +
  geom_line(data = data %>% dplyr::filter(y < 2 & method == "csdm" & year == yr) %>% unique(),
            aes(x = x, y = y, group = paste0(cal, method, ssp, gcm), 
                color = ssp, alpha = method), linewidth = 0.5) +
  geom_line(data = data %>% dplyr::filter(y < 2 & method == "partial" & year == yr) %>% unique(),
            aes(x = x, y = y, group = paste0(cal, method, ssp, gcm), 
                color = ssp, alpha = method), linewidth = 0.5) +
  geom_line(data = data %>% dplyr::filter(y < 2 & method == "expert" & year == yr) %>% unique(),
            aes(x = x, y = y, group = paste0(cal, method, ssp, gcm), 
                color = ssp, alpha = method), linewidth = 0.5) +
  scale_alpha_manual(values = c("expert" = 0.5, "partial" = 0.2, "inverse" = 0.1, "csdm" = 0.2), guide = "none") +
  geom_line(data = data %>% dplyr::filter(y > 0 & y < 3 & year == yr),
            aes(x = x, y = y, group = paste0(method, ssp, gcm), color = ssp), alpha = 0.7, linewidth = 0.6) +
  geom_line(data = data %>% dplyr::filter(y > 1  & year == yr) %>% select (x,y,method, ssp) %>%  unique(),
            aes(x = x, y = y, group = paste0(method, ssp), color = ssp), alpha = 0.7, linewidth = 0.6) +
  geom_segment(data = data %>% dplyr::filter(y > 2  & year == yr) %>% select (x,y,ssp) %>%  unique(),
               aes(x = x, xend= x, y = y, yend = y+0.5, 
                   group = paste0(ssp), color = ssp), alpha = 1, linewidth = 0.6) +
  theme_bw() + 
  # geom_text(data = data.frame(year = "2020-2040", y = c(0.2,1.2, 2.2, 3.2), 
  #                             text = c("Individual\ncalibration", "GCMs multi-\ncalibration means", "Individual method\nensemble means", "SSPs multi-\nmethod means")),
  #           aes(y = y, label = text, x = -75), color = "grey50", size = 2.3) +
  # scale_color_manual(values = c(ssp245 = "#f79320", ssp585 = "#951b1e"), guide = "none") +
  scale_color_manual(values = c(ssp245 = "#f69320", ssp585 = "#980002"),
                     breaks = c("ssp245", "ssp585"),
                     labels = c("SSP2-4.5", "SSP5-8.5")) + 
  theme(axis.line.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
        panel.grid = element_blank(), axis.ticks.y = element_blank(), panel.border = element_blank(),
        axis.line.x = element_line(color = "grey20"),
        plot.margin = margin(t = 5.5, b = 5.5, r = 7.5, l = 7.5),
        strip.background = element_blank(), strip.text = element_text(vjust = 5, size = 8.5),
        axis.title.x = element_text(size = 7.5), axis.text.x = element_text(size = 7.5) 
  ) +
  guides(color=guide_legend(nrow=2,byrow=TRUE)) +
  labs(x = "Projected change in fitness\n(relative to 1970-2000, %)") +
  # coord_cartesian(ylim = c(-100,50), xlim = c(3.4, 0), expand = FALSE, clip = "off") +
  # guides(shape = guide_legend(byrow = TRUE, ncol = 1, keywidth=10, keyheight=10,default.unit="pt", override.aes = c(size = 1.5))) +
  theme(
    legend.title = element_blank(), legend.text = element_text(size = 8.5, margin = margin(l=0, r = -3)),
    legend.position = 'none') +
  # coord_flip(xlim = c(-100,50), ylim = c(3.4, 0), expand = FALSE, clip = "off") +
  scale_x_continuous(limits = c(-.5,.50), expand = c(0,0),
                     breaks = c(-.5, -.25, 0, .25, .50),
                     labels = c("-0.5", "-0.25", "0", "0.25", "0.5")) +
  scale_y_continuous(limits = c(0, 3.7), expand = c(0,0)) +
  geom_text(data = data.frame(year = "2020-2040", y = 3.3, 
                              x = c(0.23), text = c("SSP2-4.5")),
            aes(x = x, y = y, label = text),
            color = "#f69320", size = 3) +
  geom_text(data = data.frame(year = "2020-2040", y = 3.3, 
                              x = c(-0.02), text = c("SSP5-8.5")),
            aes(x = x, y = y, label = text),
            color = "#980002", size = 3)


gcm_means <- simulations %>%
  dplyr::filter(method == "csdm") %>%
  group_by(gcm, method, ssp, year) %>% 
  reframe(gcm_mean = mean(y))

method_means <- simulations %>%
  dplyr::filter(method == "csdm") %>%
  group_by(method, ssp, year) %>% 
  reframe(method_mean = mean(y))

ssp_means <- simulations %>%
  dplyr::filter(method == "csdm") %>%
  group_by(method, ssp, year) %>% 
  reframe(y = mean(y)) %>% 
  group_by(ssp, year) %>% 
  reframe(ssp_mean = mean(y))


data <- simulations %>% 
  mutate(ind_mean = y) %>% 
  select(-y) %>% 
  left_join(gcm_means, by = join_by(year, gcm, method, ssp)) %>%
  left_join(method_means, by = join_by(year, method, ssp)) %>%
  left_join(ssp_means, by = join_by(year, ssp)) %>%
  tidyr::pivot_longer(cols = c("ind_mean", "gcm_mean", "method_mean", "ssp_mean"),
                      names_to = "type", values_to = "x") %>%
  mutate(y = ifelse(type == "ind_mean", 0, 
                    ifelse(type == "gcm_mean", 1, 
                           ifelse(type == "method_mean", 2, 
                                  ifelse(type == "ssp_mean", 3, NA))))) %>%
  dplyr::filter(year == 2050 | year == 2090) %>%
  mutate(year = ifelse(year == 2050, "2040-2060", "2080-2100")) %>%
  dplyr::filter(method == "csdm")




yr <- "2080-2100"
cascade_only_csdms <- ggplot() +
  # ggh4x::facet_wrap2(~ year, nrow = 1, strip.position="left", axes = "all", remove_labels = "all") +
  geom_hline(data = data.frame(y = c(1,2,3)), aes(yintercept = y),
             linetype = "dashed", color = "grey70", alpha = 0.8) +
  geom_line(data = data %>% dplyr::filter(y < 2 & method == "csdm" & year == yr) %>% unique(),
            aes(x = x, y = y, group = paste0(cal, method, ssp, gcm), 
                color = ssp, alpha = method), linewidth = 0.5) +
  scale_alpha_manual(values = c("expert" = 0.5, "partial" = 0.2, "inverse" = 0.1, "csdm" = 0.5), guide = "none") +
  geom_line(data = data %>% dplyr::filter(y > 0 & y < 3 & year == yr),
            aes(x = x, y = y, group = paste0(method, ssp, gcm), color = ssp), alpha = 0.7, linewidth = 0.6) +
  geom_line(data = data %>% dplyr::filter(y > 1  & year == yr) %>% select (x,y,method, ssp) %>%  unique(),
            aes(x = x, y = y, group = paste0(method, ssp), color = ssp), alpha = 0.7, linewidth = 0.6) +
  geom_segment(data = data %>% dplyr::filter(y > 2  & year == yr) %>% select (x,y,ssp) %>%  unique(),
               aes(x = x, xend= x, y = y, yend = y+0.5, 
                   group = paste0(ssp), color = ssp), alpha = 1, linewidth = 0.6) +
  theme_bw() + 
  # geom_text(data = data.frame(year = "2020-2040", y = c(0.2,1.2, 2.2, 3.2), 
  #                             text = c("Individual\ncalibration", "GCMs multi-\ncalibration means", "Individual method\nensemble means", "SSPs multi-\nmethod means")),
  #           aes(y = y, label = text, x = -75), color = "grey50", size = 2.3) +
  # scale_color_manual(values = c(ssp245 = "#f79320", ssp585 = "#951b1e"), guide = "none") +
  scale_color_manual(values = c(ssp245 = "#f69320", ssp585 = "#980002"),
                     breaks = c("ssp245", "ssp585"),
                     labels = c("SSP2-4.5", "SSP5-8.5")) + 
  theme(axis.line.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
        panel.grid = element_blank(), axis.ticks.y = element_blank(), panel.border = element_blank(),
        axis.line.x = element_line(color = "grey20"),
        plot.margin = margin(t = 5.5, b = 5.5, r = 7.5, l = 7.5),
        strip.background = element_blank(), strip.text = element_text(vjust = 5, size = 8.5),
        axis.title.x = element_text(size = 7.5), axis.text.x = element_text(size = 7.5) 
  ) +
  guides(color=guide_legend(nrow=2,byrow=TRUE)) +
  labs(x = "Projected change in fitness\n(relative to 1970-2000, %)") +
  # coord_cartesian(ylim = c(-100,50), xlim = c(3.4, 0), expand = FALSE, clip = "off") +
  # guides(shape = guide_legend(byrow = TRUE, ncol = 1, keywidth=10, keyheight=10,default.unit="pt", override.aes = c(size = 1.5))) +
  theme(
    legend.title = element_blank(), legend.text = element_text(size = 8.5, margin = margin(l=0, r = -3)),
    legend.position = 'none') +
  # coord_flip(xlim = c(-100,50), ylim = c(3.4, 0), expand = FALSE, clip = "off") +
  scale_x_continuous(limits = c(-.5,.50), expand = c(0,0),
                     breaks = c(-.5, -.25, 0, .25, .50),
                     labels = c("-0.5", "-0.25", "0", "0.25", "0.5")) +
  scale_y_continuous(limits = c(0, 3.7), expand = c(0,0)) +
  geom_text(data = data.frame(year = "2020-2040", y = c(0.4,1.4, 2.4, 3.4), 
                              text = c("Individual\ncalibration", "GCMs multi-\ncalibration means", "SDMs multi-\nGCM means", "SSPs multi-\nSDM means")),
            aes(y = y, label = text, x = -0.35), color = "grey50", size = 2.7)
  
