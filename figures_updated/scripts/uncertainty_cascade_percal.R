
head(simulations)

cal_means <- simulations %>%
  group_by(cal, ssp, year) %>% 
  reframe(cal_mean = mean(y))

ssp_means <- simulations %>%
  group_by(ssp, year) %>% 
  reframe(ssp_mean = mean(y))

data <- simulations %>% 
  mutate(ind_mean = y) %>% 
  select(-y) %>% 
  left_join(cal_means, by = join_by(year, cal, ssp)) %>%
  left_join(ssp_means, by = join_by(year, ssp)) %>%
  pivot_longer(cols = c("ind_mean", "cal_mean", "ssp_mean"),
               names_to = "type", values_to = "x") %>%
  mutate(y = ifelse(type == "ind_mean", 0, ifelse(type == "cal_mean", 1, ifelse(type == "ssp_mean", 2, NA)))) %>%
  dplyr::filter(year == 2090)

ggplot() +
  # geom_point() +
  geom_line(data = data %>% dplyr::filter(y < 2) %>% unique(),
            aes(x = x, y = y, group = paste0(cal, ssp, gcm), color = ssp), alpha = 0.1, linewidth = 0.5) +
  geom_line(data = data %>% dplyr::filter(y > 0),
            aes(x = x, y = y, group = paste0(ssp, cal), color = ssp), alpha = 0.1, linewidth = 0.5) +
  geom_line(data = rbind(data %>% dplyr::filter(y > 1), data %>% filter(y > 1) %>% mutate(y = 2.2)),
            aes(x = x, y = y, group = paste0(ssp), color = ssp), alpha = 1, linewidth = 1) +
  geom_text(data = data %>% filter(y > 1) %>% mutate(y = 2.3) %>% select(ssp, x, y) %>% unique(),
            aes(x = x, y = y, label = ifelse(ssp == "ssp245", "SSP2-4.5", "SSP5-8.5"), color = ssp)) +
  theme_bw() + 
  scale_color_manual(values = c(ssp245 = "#f79320", ssp585 = "#951b1e")) +
  theme(axis.line.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
        panel.grid = element_blank(), axis.ticks.y = element_blank(), panel.border = element_blank(),
        axis.line.x = element_line(color = "grey20"), legend.position = "none",
        plot.margin = margin(t = 5.5, b = 5.5, r = 5.5, l = 5.5)) +
  labs(x = "Relative change in fitness") +
  coord_cartesian(xlim = c(-100,50), ylim = c(0, 2.4), expand = FALSE)


