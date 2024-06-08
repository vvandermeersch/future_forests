
simulations$method <- factor(ifelse(simulations$cal == "expert", "expert",
                                    ifelse(simulations$cal %in% models, "csdm", "inverse")),
                             levels = c("expert", "inverse", "csdm"))


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
  mutate(year = ifelse(year == 2050, "2050-2060", "2080-2100")) %>%
  dplyr::filter(ssp == "ssp245")


yr <- "2050-2060"
cascade1 <- ggplot() +
  # ggh4x::facet_wrap2(~ year, nrow = 1, strip.position="left", axes = "all", remove_labels = "all") +
  geom_vline(data = data.frame(y = c(1,2)), aes(xintercept = y),
             linetype = "dashed", color = "grey70", alpha = 0.8) +
  geom_line(data = data %>% dplyr::filter(y < 2 & method == "inverse" & year == yr) %>% unique(),
            aes(x = y, y = x, group = paste0(cal, method, ssp, gcm), 
                color = method, alpha = method), linewidth = 0.5) +
  geom_line(data = data %>% dplyr::filter(y < 2 & method == "csdm" & year == yr) %>% unique(),
            aes(x = y, y = x, group = paste0(cal, method, ssp, gcm), 
                color = method, alpha = method), linewidth = 0.5) +
  geom_line(data = data %>% dplyr::filter(y < 2 & method == "expert" & year == yr) %>% unique(),
            aes(x = y, y = x, group = paste0(cal, method, ssp, gcm), 
                color = method, alpha = method), linewidth = 0.5) +
  scale_alpha_manual(values = c("expert" = 0.5, "inverse" = 0.1, "csdm" = 0.2), guide = "none") +
  geom_line(data = data %>% dplyr::filter(y > 0 & y < 3 & year == yr),
            aes(x = y, y = x, group = paste0(method, ssp, gcm), color = method), alpha = 0.7, linewidth = 0.6) +
  # geom_line(data = data %>% dplyr::filter(y > 1 & year == yr),
  #           aes(x = x*100, y = y, group = paste0(method, ssp), color = ssp), alpha = 0.9, linewidth = 0.6) +
  # geom_line(data = rbind(data %>% dplyr::filter(y > 2 & year == yr), data %>% filter(y > 2) %>% mutate(y = 3.2)),
  #           aes(x = x*100, y = y, group = paste0(ssp), color = ssp), alpha = 1, linewidth = 1) +
  # geom_text(data = data %>% filter(y > 2 & year == yr) %>% mutate(y = 3.3) %>% select(year, ssp, x, y) %>% unique() %>% filter(year == "2080-2100"),
  #           aes(x = y, y = x*100, label = ifelse(ssp == "ssp245", "SSP2", "SSP5"), color = ssp), size = 2.3) +
  geom_point(data = data %>% dplyr::filter(y == 2 & year == yr) %>% dplyr::select(-c(gcm, ref, cal)) %>% unique(),
             aes(x = y, y = x), size = 2, fill = "white", color = "white", shape = 22) +
  geom_point(data = data %>% dplyr::filter(y == 2 & year == yr) %>% dplyr::select(-c(gcm, ref, cal)) %>% unique(),
             aes(x = y, y = x, color = method), size = 2, fill = NA, shape = 22) +
  # scale_shape_manual(values = c("expert" = 22, "inverse" = 21, "csdm" = 24)) +
  theme_bw() + 
  # geom_text(data = data.frame(year = "2020-2040", y = c(0.2,1.2, 2.2, 3.2), 
  #                             text = c("Individual\ncalibration", "GCMs multi-\ncalibration means", "Individual method\nensemble means", "SSPs multi-\nmethod means")),
  #           aes(y = y, label = text, x = -75), color = "grey50", size = 2.3) +
  #scale_color_manual(values = c(ssp245 = "#f79320", ssp585 = "#951b1e"), guide = "none") +
  scale_color_manual(values = c(expert = "#018530", inverse = "#995D81", csdm = "#457b9d"),
                     breaks = c("expert", "inverse", "csdm"),
                     labels = c("Expert PEM", "Fitted PEM", "Correlative SDM")) + 
  theme(axis.line.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
        panel.grid = element_blank(), axis.ticks.x = element_blank(), panel.border = element_blank(),
        axis.line.y = element_line(color = "grey20"),
        plot.margin = margin(t = 5.5, b = 5.5, r = 0, l = 5.5),
        strip.background = element_blank(), strip.text = element_text(vjust = 5, size = 8.5),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 7, color = NA)
  ) +
  labs(x = "Projected change in fitness\nrelative to 1970-2000 (%)") +
  # coord_cartesian(ylim = c(-100,50), xlim = c(3.4, 0), expand = FALSE, clip = "off") +
  # guides(shape = guide_legend(byrow = TRUE, ncol = 1, keywidth=10, keyheight=10,default.unit="pt", override.aes = c(size = 1.5))) +
  # theme(
  #   legend.position.inside = c(0.87, 0.35), legend.position = "inside",
  #   legend.title = element_blank(), legend.text = element_text(size = 6.5, margin = margin(r = 0)),
  #   legend.spacing.x = unit(0, 'cm')) + 
  # coord_flip(xlim = c(-100,50), ylim = c(3.4, 0), expand = FALSE, clip = "off") + 
  theme(legend.position = 'none') +
  scale_y_continuous(position = "right", limits = c(-.25,.50), expand = c(0,0), 
                     breaks = c(-.25, 0, .25, .50),
                     labels = c("-0.25", "0   ", "0.25", "0.5 ")) + 
  scale_x_reverse(limits = c(2.2, 0), expand = c(0,0))

yr <- "2080-2100"
cascade2 <- ggplot() +
  # ggh4x::facet_wrap2(~ year, nrow = 1, strip.position="left", axes = "all", remove_labels = "all") +
  geom_vline(data = data.frame(y = c(1,2)), aes(xintercept = y),
             linetype = "dashed", color = "grey70", alpha = 0.8) +
  geom_line(data = data %>% dplyr::filter(y < 2 & method == "inverse" & year == yr) %>% unique(),
            aes(x = y, y = x, group = paste0(cal, method, ssp, gcm), 
                color = method, alpha = method), linewidth = 0.5) +
  geom_line(data = data %>% dplyr::filter(y < 2 & method == "csdm" & year == yr) %>% unique(),
            aes(x = y, y = x, group = paste0(cal, method, ssp, gcm), 
                color = method, alpha = method), linewidth = 0.5) +
  geom_line(data = data %>% dplyr::filter(y < 2 & method == "expert" & year == yr) %>% unique(),
            aes(x = y, y = x, group = paste0(cal, method, ssp, gcm), 
                color = method, alpha = method), linewidth = 0.5) +
  scale_alpha_manual(values = c("expert" = 0.5, "inverse" = 0.1, "csdm" = 0.2), guide = "none") +
  geom_line(data = data %>% dplyr::filter(y > 0 & y < 3 & year == yr),
            aes(x = y, y = x, group = paste0(method, ssp, gcm), color = method), alpha = 0.7, linewidth = 0.6) +
  # geom_line(data = data %>% dplyr::filter(y > 1 & year == yr),
  #           aes(x = x*100, y = y, group = paste0(method, ssp), color = ssp), alpha = 0.9, linewidth = 0.6) +
  # geom_line(data = rbind(data %>% dplyr::filter(y > 2 & year == yr), data %>% filter(y > 2) %>% mutate(y = 3.2)),
  #           aes(x = x*100, y = y, group = paste0(ssp), color = ssp), alpha = 1, linewidth = 1) +
  # geom_text(data = data %>% filter(y > 2 & year == yr) %>% mutate(y = 3.3) %>% select(year, ssp, x, y) %>% unique() %>% filter(year == "2080-2100"),
  #           aes(x = y, y = x*100, label = ifelse(ssp == "ssp245", "SSP2", "SSP5"), color = ssp), size = 2.3) +
  geom_point(data = data %>% dplyr::filter(y == 2 & year == yr) %>% dplyr::select(-c(gcm, ref, cal)) %>% unique(),
             aes(x = y, y = x), size = 2, fill = "white", color = "white", shape = 22) +
  geom_point(data = data %>% dplyr::filter(y == 2 & year == yr) %>% dplyr::select(-c(gcm, ref, cal)) %>% unique(),
             aes(x = y, y = x, color = method), size = 2, fill = NA, shape = 22) +
  # scale_shape_manual(values = c("expert" = 22, "inverse" = 21, "csdm" = 24)) +
  theme_bw() + 
  # geom_text(data = data.frame(year = "2020-2040", y = c(0.2,1.2, 2.2, 3.2), 
  #                             text = c("Individual\ncalibration", "GCMs multi-\ncalibration means", "Individual method\nensemble means", "SSPs multi-\nmethod means")),
  #           aes(y = y, label = text, x = -75), color = "grey50", size = 2.3) +
  # scale_color_manual(values = c(ssp245 = "#f79320", ssp585 = "#951b1e"), guide = "none") +
  scale_color_manual(values = c(expert = "#018530", inverse = "#995D81", csdm = "#457b9d"),
                     breaks = c("expert", "inverse", "csdm"),
                     labels = c("Expert PEM", "Fitted PEM", "Correlative SDM")) + 
  theme(axis.line.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
        panel.grid = element_blank(), axis.ticks.x = element_blank(), panel.border = element_blank(),
        axis.line.y = element_line(color = "grey20"),
        plot.margin = margin(t = 5.5, b = 5.5, r = 5.5, l = -10),
        strip.background = element_blank(), strip.text = element_text(vjust = 5, size = 8.5),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 7) 
  ) +
  labs(x = "Projected change in fitness\nrelative to 1970-2000 (%)") +
  # coord_cartesian(ylim = c(-100,50), xlim = c(3.4, 0), expand = FALSE, clip = "off") +
  # guides(shape = guide_legend(byrow = TRUE, ncol = 1, keywidth=10, keyheight=10,default.unit="pt", override.aes = c(size = 1.5))) +
  theme(
    legend.title = element_blank(), legend.text = element_text(size = 6.5),
    legend.position = 'none') +
  # coord_flip(xlim = c(-100,50), ylim = c(3.4, 0), expand = FALSE, clip = "off") +
  scale_y_continuous(position = "left", limits = c(-.25,.50), expand = c(0,0), 
                     breaks = c(-.25, 0, .25, .50),
                     labels = c("-0.25", "0   ", "0.25", "0.5 ")) + 
  scale_x_continuous(limits = c(0, 2.2), expand = c(0,0))




