
ecoregions_unc_trend <- plot_grid(
  plot_grid(NULL,
            trend_unc_Continental + labs(y = "\n") +
              theme(panel.border = element_rect(colour = "#e9c46a", fill=NA, linewidth=0.6)),
            NULL,
            trend_unc_Boreal + labs(y = "\n") +
              scale_y_continuous(position = "right") +
              theme(panel.border = element_rect(colour = "#2a9d8f", fill=NA, linewidth=0.6)),
            NULL,
            rel_widths = c(0.2, 1, 0.2, 1, 0.1), nrow = 1),
  plot_grid(trend_unc_Atlantic +
              theme(panel.border = element_rect(colour = "#80b25b", fill=NA, linewidth=0.6)),
            NULL,
            ecoregions_map,
            NULL,
            rel_widths = c(1, 0.2, 0.9, 0.4), nrow = 1),
  plot_grid(NULL, trend_unc_Mediterranean + labs(y = "\n") +
              theme(panel.border = element_rect(colour = "#e76f51", fill=NA, linewidth=0.6)),
            NULL,
            trend_unc_Alpine + labs(y = "\n") +
              scale_y_continuous(position = "right") +
              theme(panel.border = element_rect(colour = "#c0ddf0", fill=NA, linewidth=0.6)),
            NULL,
            rel_widths = c(0.2, 1, 0.2, 1, 0.1), nrow = 1),
  cowplot::get_plot_component(trend_unc + theme(legend.position = "bottom"), 'guide-box-bottom', return_all = TRUE),
  rel_heights = c(1,1,1,0.3), ncol = 1, axis = "tb", align = "v") +
  draw_line(
    x = c(0.479, 0.51),
    y = c(0.26, 0.41)*122/110,
    color = "#e76f51", size = 0.3
  ) +
  draw_grob(pointsGrob(x = 0.51, y = 0.41*122/110, size = unit(2, "pt"), pch = 21, gp = gpar(fill = "#e76f51", col = "white", lex = 0.6))) +
  draw_line(
    x = c(0.66, 0.57),
    y = c(0.34, 0.45)*122/110,
    color = "#c0ddf0", size = 0.3
  ) +
  draw_grob(pointsGrob(x = 0.57, y = 0.45*122/110, size = unit(2, "pt"), pch = 21, gp = gpar(fill = "#c0ddf0", col = "white", lex = 0.6))) +
  draw_line(
    x = c(1/2.5, 0.535),
    y = c(0.5, 0.46)*122/110,
    color = "#80b25b", size = 0.3
  ) +
  draw_grob(pointsGrob(x = 0.535, y = 0.46*122/110, size = unit(2, "pt"), pch = 21, gp = gpar(fill = "#80b25b", col = "white", lex = 0.5))) +
  draw_line(
    x = c(0.479, 0.58),
    y = c(0.715, 0.475)*122/110,
    color = "#e9c46a", size = 0.3
  ) +
  draw_grob(pointsGrob(x = 0.58, y = 0.475*122/110, size = unit(2, "pt"), pch = 21, gp = gpar(fill = "#e9c46a", col = "white", lex = 0.5))) +
  draw_line(
    x = c(0.63, 0.65),
    y = c(0.54, 0.675)*122/110,
    color = "#2a9d8f", size = 0.3
  ) +
  draw_grob(pointsGrob(x = 0.63, y = 0.54*122/110, size = unit(2, "pt"), pch = 21, gp = gpar(fill = "#2a9d8f", col = "white", lex = 0.5)))