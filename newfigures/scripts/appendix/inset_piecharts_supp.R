data_piechart <- data_barplot %>%
  group_by(ecoregion) %>%
  summarise(sdm = mean(sdm/tot*100), 
            sdm.climate = mean((gcm.sdm+ssp.sdm)/tot*100), residuals = mean((residuals/tot*100)),
            gcm = mean((gcm)/tot*100), 
            gcm.ssp = mean((gcm.ssp)/tot*100),
            ssp = mean((ssp)/tot*100)
  )

data_piechart <- 
  pivot_longer(data_piechart, cols = c("sdm", "sdm.climate", "gcm", "gcm.ssp", "ssp", "residuals"), names_to = "var", values_to = "frac") %>%
  group_by(ecoregion) %>%
  mutate(ymax = cumsum(frac),
         ymin = c(0, head(ymax, n=-1)))

data_piechart$pos <- (data_piechart$ymax + data_piechart$ymin) / 2


# Make the plot
piecharts <- ggplot(data_piechart, aes(ymax=ymax, ymin=ymin, xmax=3, xmin=2, fill=var)) +
  facet_wrap(~ ecoregion, nrow = 1)  +
  geom_rect(color = "grey30", linewidth = 0.1) +
  coord_polar(theta="y") +
  xlim(c(1, 4)) +
  theme_void() +
  theme(legend.position = "none") + 
  scale_fill_manual(name = NULL, 
                    breaks = c("ssp", "gcm.ssp", "gcm", "sdm.climate", "sdm", "residuals"),
                    labels = c("SSP", "SSP - GCM", "GCM", "Climate - SDM", "SDM", "Residuals"),
                    values=cols) +
  theme(strip.text = element_text(vjust = -1.5, colour = "grey30"),
        panel.spacing = unit(-1, "lines"),
        plot.margin = margin(r = 0),
        panel.background = element_rect(fill = "transparent", colour = NA)) +
  shadowtext::geom_shadowtext(data = data_piechart[data_piechart$var == "sdm",], aes(x = 2.5, y = pos, label = paste0(round(frac,0), "%")), 
                              color = "white", size = 2.3, bg.colour='#72957a', bg.r = 0.13)

ggsave(piecharts, file = file.path(wd, 'newfigures', 'files', 'piecharts_onlycsdm.pdf' ), dev = cairo_pdf)
