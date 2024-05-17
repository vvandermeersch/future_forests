
simulations <- rbind(simulations_pbm,simulations_csdm)

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
  dplyr::filter(year == 2030 | year == 2090) %>%
  mutate(year = ifelse(year == 2030, "2020-2040", "2080-2100"))

unc_cascade <- ggplot() +
  ggh4x::facet_wrap2(~ year, nrow = 2, strip.position="left", axes = "all", remove_labels = "all") +
  geom_hline(data = data.frame(year = c("2020-2040", "2080-2100"), y = c(1,2,3,3,2,1)), aes(yintercept = y), 
             linetype = "dashed", color = "grey70", alpha = 0.8) +
  geom_line(data = data %>% dplyr::filter(y < 2 & method == "inverse") %>% unique(),
            aes(x = x, y = y, group = paste0(cal, method, ssp, gcm), 
                color = ssp, alpha = method), linewidth = 0.5) +
  geom_line(data = data %>% dplyr::filter(y < 2 & method == "csdm") %>% unique(),
            aes(x = x, y = y, group = paste0(cal, method, ssp, gcm), 
                color = ssp, alpha = method), linewidth = 0.5) +
  geom_line(data = data %>% dplyr::filter(y < 2 & method == "expert") %>% unique(),
            aes(x = x, y = y, group = paste0(cal, method, ssp, gcm), 
                color = ssp, alpha = method), linewidth = 0.5) +
  scale_alpha_manual(values = c("expert" = 0.5, "inverse" = 0.1, "csdm" = 0.2), guide = "none") +
  geom_line(data = data %>% dplyr::filter(y > 0 & y < 3),
            aes(x = x, y = y, group = paste0(method, ssp, gcm), color = ssp), alpha = 0.7, linewidth = 0.6) +
  geom_line(data = data %>% dplyr::filter(y > 1),
            aes(x = x, y = y, group = paste0(method, ssp), color = ssp), alpha = 0.9, linewidth = 0.6) +
  geom_line(data = rbind(data %>% dplyr::filter(y > 2), data %>% filter(y > 2) %>% mutate(y = 3.2)),
            aes(x = x, y = y, group = paste0(ssp), color = ssp), alpha = 1, linewidth = 1) +
  geom_text(data = data %>% filter(y > 2) %>% mutate(y = 3.3) %>% select(year, ssp, x, y) %>% unique() %>% filter(year == "2080-2100"),
            aes(x = x, y = y, label = ifelse(ssp == "ssp245", "SSP2", "SSP5"), color = ssp), size = 2.3) +
  geom_point(data = data %>% dplyr::filter(y == 2) %>% dplyr::select(-c(gcm, ref, cal)) %>% unique(),
             aes(x = x, y = y, shape = method), size = 2, fill = "white", color = "white") +
  geom_point(data = data %>% dplyr::filter(y == 2) %>% dplyr::select(-c(gcm, ref, cal)) %>% unique(),
             aes(x = x, y = y, shape = method, color = ssp), size = 2, fill = NA) +
  scale_shape_manual(values = c("expert" = 22, "inverse" = 21, "csdm" = 24)) +
  theme_bw() + 
  geom_text(data = data.frame(year = "2020-2040", y = c(0.2,1.2, 2.2, 3.2), 
                              text = c("Individual\ncalibration", "GCMs multi-\ncalibration means", "Individual method\nensemble means", "SSPs multi-\nmethod means")),
            aes(y = y, label = text, x = -75), color = "grey50", size = 2.3) +
  scale_color_manual(values = c(ssp245 = "#f79320", ssp585 = "#951b1e"), guide = "none") +
  theme(axis.line.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
        panel.grid = element_blank(), axis.ticks.y = element_blank(), panel.border = element_blank(),
        axis.line.x = element_line(color = "grey20"),
        plot.margin = margin(t = 5.5, b = 5.5, r = 8.5, l = 5.5),
        strip.background = element_blank(), strip.text = element_text(vjust = 5, size = 8.5),
        axis.title.x = element_text(size = 8), axis.text.x = element_text(size = 7)
  ) +
  labs(x = "Projected change in fitness\nrelative to 1970-2000 (%)") +
  coord_cartesian(xlim = c(-100,50), ylim = c(0, 3.4), expand = FALSE, clip = "off") +
  guides(shape = guide_legend(byrow = TRUE, ncol = 1, keywidth=10, keyheight=10,default.unit="pt", override.aes = c(size = 1.5))) +
  theme(
    legend.position.inside = c(0.87, 0.35), legend.position = "inside",
    legend.title = element_blank(), legend.text = element_text(size = 6.5, margin = margin(r = 0)),
    legend.spacing.x = unit(0, 'cm'))
  


