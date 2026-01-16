

binbreaks <- seq(-6,26,1)
bins <- as.integer(cut(tmean, breaks = binbreaks, right = FALSE))


plot(x = NULL,
     y = NULL,
     xlim = c(-6, 26),
     ylim = c(-1, 1),
     xaxt = 'n',
     yaxt = 'n',
     xlab = 'Temperature',
     ylab = 'Fitness',
     bty = "n")

delta <- 0.05
axis(1, at = seq(-5,25,5))

axis(2, at = seq(0, 1, 0.25)+delta, labels = c(0, NA, 0.5, NA, 1), cex.axis = 0.85)
axis(2, at = seq(-1, 0, 0.25)-delta, labels = c(1, NA, 0.5, NA, 0), cex.axis = 0.85)

legend("topright",
       legend = c("Continental", "Boreal"),
       col = c("#d5af7b", "#4faba0"),
       lwd = c(2,2),
       bty = "n",
       cex = 0.8)

for(b in min(bins):max(bins)){
  
  csdm_fitness_b <- csdm_fitness[bins == b]
  quantiles_b <- quantile(csdm_fitness_b, c(0.025, 0.25, 0.5, 0.75, 0.975))

  rect(xleft = binbreaks[b],
       ybottom = quantiles_b[1] + delta,
       xright = binbreaks[b+1],
       ytop =  quantiles_b[5]+ delta,
       col =  'grey90',
       border = NA)

  rect(xleft = binbreaks[b],
       ybottom = quantiles_b[2] + delta,
       xright = binbreaks[b+1],
       ytop =  quantiles_b[4] + delta,
       col =  'grey85',
       border = NA)
  
  mech_fitness_b <- mech_fitness[bins == b]
  quantiles_b <- quantile(mech_fitness_b, c(0.025, 0.25, 0.5, 0.75, 0.975))
  
  rect(xleft = binbreaks[b],
       ybottom = -quantiles_b[1] - delta,
       xright = binbreaks[b+1],
       ytop =  -quantiles_b[5]- delta,
       col =  'grey90',
       border = NA) 
  
  rect(xleft = binbreaks[b],
       ybottom = -quantiles_b[2]- delta,
       xright = binbreaks[b+1],
       ytop =  -quantiles_b[4]- delta,
       col =  'grey85',
       border = NA) 
  
}

text(x = -5, y = 0.9, labels = 'Correlative\nmodel', adj = 0, col = 'grey20')
text(x = -5, y = -0.9, labels = 'Mechanistic\nmodel', adj = 0, col = 'grey20')

for(b in  min(bins):max(bins)){
  
  # csdm_fitness_b <- csdm_fitness[bins == b]
  # quantiles_b <- quantile(csdm_fitness_b, c(0.025, 0.25, 0.5, 0.75, 0.975))
  # 
  # segments(x0 = binbreaks[b],
  #          x1 = binbreaks[b+1],
  #          y0 = quantiles_b[3]+ delta,
  #          col =  'white', lwd = 6)
  # 
  # segments(x0 = binbreaks[b],
  #          x1 = binbreaks[b+1],
  #          y0 = quantiles_b[3]+ delta,
  #          col =  '#F6CF71', lwd = 2)
  # 
  # mech_fitness_b <- mech_fitness[bins == b,]
  # quantiles_b <- quantile(mech_fitness_b, c(0.025, 0.25, 0.5, 0.75, 0.975))
  # 
  # segments(x0 = binbreaks[b],
  #          x1 = binbreaks[b+1],
  #          y0 = -quantiles_b[3] - delta,
  #          col =  'white', lwd = 6)
  # 
  # segments(x0 = binbreaks[b],
  #          x1 = binbreaks[b+1],
  #          y0 = -quantiles_b[3] - delta,
  #          col =  '#9EB9F3', lwd = 2)
}


segments(x0 = quantile(tmean_hist_cont, c(0.25)), x1 = quantile(tmean_hist_cont, c(0.75)),
         y0 = -quantile(mech_fitness_cont_hist , c(0.5)) - delta,
         col = 'white', lwd = 4.5)

segments(x0 = quantile(tmean_hist_cont, c(0.5)),
         y0 = -quantile(mech_fitness_cont_hist , c(0.25)) - delta, y1 = -quantile(mech_fitness_cont_hist , c(0.75)) - delta,
         col = 'white', lwd = 4.5)

points(x = quantile(tmean_hist_cont, c(0.5)), y = -quantile(mech_fitness_cont_hist , c(0.5))- delta,
       pch = 19, col = 'white', cex = 1.3)

segments(x0 = quantile(tmean_hist_cont, c(0.25)), x1 = quantile(tmean_hist_cont, c(0.75)),
         y0 = -quantile(mech_fitness_cont_hist , c(0.5)) - delta,
         col = '#d5af7b', lwd = 2.5)

segments(x0 = quantile(tmean_hist_cont, c(0.5)),
         y0 = -quantile(mech_fitness_cont_hist , c(0.25)) - delta, y1 = -quantile(mech_fitness_cont_hist , c(0.75))- delta,
         col = '#d5af7b', lwd = 2.5)

points(x = quantile(tmean_hist_cont, c(0.5)), y = -quantile(mech_fitness_cont_hist , c(0.5))- delta,
       pch = 19, col = '#d5af7b', cex = 1)



segments(x0 = quantile(tmean_20802100_cont, c(0.25)), x1 = quantile(tmean_20802100_cont, c(0.75)),
         y0 = -quantile(mech_fitness_cont_20802100 , c(0.5)) - delta,
         col = 'white', lwd = 4.5)

segments(x0 = quantile(tmean_20802100_cont, c(0.5)),
         y0 = -quantile(mech_fitness_cont_20802100 , c(0.25))- delta, y1 = -quantile(mech_fitness_cont_20802100 , c(0.75)) - delta,
         col = 'white', lwd = 4.5)

points(x = quantile(tmean_20802100_cont, c(0.5)), y = -quantile(mech_fitness_cont_20802100 , c(0.5))- delta,
       pch = 19, col = 'white', cex = 1.3)

segments(x0 = quantile(tmean_20802100_cont, c(0.25)), x1 = quantile(tmean_20802100_cont, c(0.75)),
         y0 = -quantile(mech_fitness_cont_20802100 , c(0.5)) - delta,
         col = '#d5af7b', lwd = 2.5)

segments(x0 = quantile(tmean_20802100_cont, c(0.5)),
         y0 = -quantile(mech_fitness_cont_20802100 , c(0.25)) - delta, y1 = -quantile(mech_fitness_cont_20802100 , c(0.75)) - delta,
         col = '#d5af7b', lwd = 2.5)

points(x = quantile(tmean_20802100_cont, c(0.5)), y = -quantile(mech_fitness_cont_20802100 , c(0.5))- delta,
       pch = 19, col = '#d5af7b', cex = 1)


arrows(x0 = quantile(tmean_hist_cont, c(0.5))+0.2, x1 = quantile(tmean_20802100_cont, c(0.5))-0.2,
       y0 = -quantile(mech_fitness_cont_hist, c(0.5)) - delta - 0.02, y1 = -quantile(mech_fitness_cont_20802100, c(0.5)) - delta + 0.02,
       length = 0.1, angle = 30, col = '#9f7132', lwd = 1.5)





segments(x0 = quantile(tmean_20802100_cont, c(0.25)), x1 = quantile(tmean_20802100_cont, c(0.75)),
         y0 = quantile(csdm_fitness_cont_20802100, c(0.5)) + delta,
         col = 'white', lwd = 4.5)

segments(x0 = quantile(tmean_20802100_cont, c(0.5)),
         y0 = quantile(csdm_fitness_cont_20802100, c(0.25)) + delta, y1 = quantile(csdm_fitness_cont_20802100, c(0.75)) + delta,
         col = 'white', lwd = 4.5)

points(x = quantile(tmean_20802100_cont, c(0.5)), y = quantile(csdm_fitness_cont_20802100, c(0.5))+ delta,
       pch = 19, col = 'white', cex = 1.3)

segments(x0 = quantile(tmean_20802100_cont, c(0.25)), x1 = quantile(tmean_20802100_cont, c(0.75)),
         y0 = quantile(csdm_fitness_cont_20802100, c(0.5)) + delta,
         col = '#d5af7b', lwd = 2.5)

segments(x0 = quantile(tmean_20802100_cont, c(0.5)),
         y0 = quantile(csdm_fitness_cont_20802100, c(0.25)) + delta, y1 = quantile(csdm_fitness_cont_20802100, c(0.75)) + delta,
         col = '#d5af7b', lwd = 2.5)

arrows(x0 = quantile(tmean_hist_cont, c(0.5))+0.2, x1 = quantile(tmean_20802100_cont, c(0.5))-0.2,
       y0 = quantile(csdm_fitness_cont_hist, c(0.5)) + delta, y1 = quantile(csdm_fitness_cont_20802100, c(0.5)) + delta,
       length = 0.1, angle = 30, col = '#9f7132', lwd = 1.5)

points(x = quantile(tmean_20802100_cont, c(0.5)), y = quantile(csdm_fitness_cont_20802100, c(0.5))+ delta,
       pch = 19, col = '#d5af7b', cex = 1)

segments(x0 = quantile(tmean_hist_cont, c(0.25)), x1 = quantile(tmean_hist_cont, c(0.75)),
         y0 = quantile(csdm_fitness_cont_hist, c(0.5)) + delta,
         col = 'white', lwd = 4.5)

segments(x0 = quantile(tmean_hist_cont, c(0.5)),
         y0 = quantile(csdm_fitness_cont_hist, c(0.25)) + delta, y1 = quantile(csdm_fitness_cont_hist, c(0.75)) + delta,
         col = 'white', lwd = 4.5)

points(x = quantile(tmean_hist_cont, c(0.5)), y = quantile(csdm_fitness_cont_hist, c(0.5))+ delta,
       pch = 19, col = 'white', cex = 1.3)

segments(x0 = quantile(tmean_hist_cont, c(0.25)), x1 = quantile(tmean_hist_cont, c(0.75)),
         y0 = quantile(csdm_fitness_cont_hist, c(0.5)) + delta,
         col = '#d5af7b', lwd = 2.5)

segments(x0 = quantile(tmean_hist_cont, c(0.5)),
         y0 = quantile(csdm_fitness_cont_hist, c(0.25)) + delta, y1 = quantile(csdm_fitness_cont_hist, c(0.75)) + delta,
         col = '#d5af7b', lwd = 2.5)

points(x = quantile(tmean_hist_cont, c(0.5)), y = quantile(csdm_fitness_cont_hist, c(0.5))+ delta,
       pch = 19, col = '#d5af7b', cex = 1)





# Mediterranean


segments(x0 = quantile(tmean_hist_med, c(0.25)), x1 = quantile(tmean_hist_med, c(0.75)),
         y0 = -quantile(mech_fitness_med_hist , c(0.5)) - delta,
         col = 'white', lwd = 4.5)

segments(x0 = quantile(tmean_hist_med, c(0.5)),
         y0 = -quantile(mech_fitness_med_hist , c(0.25)) - delta, y1 = -quantile(mech_fitness_med_hist , c(0.75)) - delta,
         col = 'white', lwd = 4.5)

points(x = quantile(tmean_hist_med, c(0.5)), y = -quantile(mech_fitness_med_hist , c(0.5))- delta,
       pch = 19, col = 'white', cex = 1.3)

segments(x0 = quantile(tmean_hist_med, c(0.25)), x1 = quantile(tmean_hist_med, c(0.75)),
         y0 = -quantile(mech_fitness_med_hist , c(0.5)) - delta,
         col = '#ed947e', lwd = 2.5)

segments(x0 = quantile(tmean_hist_med, c(0.5)),
         y0 = -quantile(mech_fitness_med_hist , c(0.25)) - delta, y1 = -quantile(mech_fitness_med_hist , c(0.75))- delta,
         col = '#ed947e', lwd = 2.5)

points(x = quantile(tmean_hist_med, c(0.5)), y = -quantile(mech_fitness_med_hist , c(0.5))- delta,
       pch = 19, col = '#ed947e', cex = 1)



segments(x0 = quantile(tmean_20802100_med, c(0.25)), x1 = quantile(tmean_20802100_med, c(0.75)),
         y0 = -quantile(mech_fitness_med_20802100 , c(0.5)) - delta,
         col = 'white', lwd = 4.5)

segments(x0 = quantile(tmean_20802100_med, c(0.5)),
         y0 = -quantile(mech_fitness_med_20802100 , c(0.25))- delta, y1 = -quantile(mech_fitness_med_20802100 , c(0.75)) - delta,
         col = 'white', lwd = 4.5)

points(x = quantile(tmean_20802100_med, c(0.5)), y = -quantile(mech_fitness_med_20802100 , c(0.5))- delta,
       pch = 19, col = 'white', cex = 1.3)

segments(x0 = quantile(tmean_20802100_med, c(0.25)), x1 = quantile(tmean_20802100_med, c(0.75)),
         y0 = -quantile(mech_fitness_med_20802100 , c(0.5)) - delta,
         col = '#ed947e', lwd = 2.5)

segments(x0 = quantile(tmean_20802100_med, c(0.5)),
         y0 = -quantile(mech_fitness_med_20802100 , c(0.25)) - delta, y1 = -quantile(mech_fitness_med_20802100 , c(0.75)) - delta,
         col = '#ed947e', lwd = 2.5)

points(x = quantile(tmean_20802100_med, c(0.5)), y = -quantile(mech_fitness_med_20802100 , c(0.5))- delta,
       pch = 19, col = '#ed947e', cex = 1)



segments(x0 = quantile(tmean_hist_med, c(0.25)), x1 = quantile(tmean_hist_med, c(0.75)),
         y0 = quantile(csdm_fitness_med_hist, c(0.5)) + delta,
         col = 'white', lwd = 4.5)

segments(x0 = quantile(tmean_hist_med, c(0.5)),
         y0 = quantile(csdm_fitness_med_hist, c(0.25)) + delta, y1 = quantile(csdm_fitness_med_hist, c(0.75)) + delta,
         col = 'white', lwd = 4.5)

points(x = quantile(tmean_hist_med, c(0.5)), y = quantile(csdm_fitness_med_hist, c(0.5))+ delta,
       pch = 19, col = 'white', cex = 1.3)

segments(x0 = quantile(tmean_hist_med, c(0.25)), x1 = quantile(tmean_hist_med, c(0.75)),
         y0 = quantile(csdm_fitness_med_hist, c(0.5)) + delta,
         col = '#ed947e', lwd = 2.5)

segments(x0 = quantile(tmean_hist_med, c(0.5)),
         y0 = quantile(csdm_fitness_med_hist, c(0.25)) + delta, y1 = quantile(csdm_fitness_med_hist, c(0.75)) + delta,
         col = '#ed947e', lwd = 2.5)

points(x = quantile(tmean_hist_med, c(0.5)), y = quantile(csdm_fitness_med_hist, c(0.5))+ delta,
       pch = 19, col = '#ed947e', cex = 1)



segments(x0 = quantile(tmean_20802100_med, c(0.25)), x1 = quantile(tmean_20802100_med, c(0.75)),
         y0 = quantile(csdm_fitness_med_20802100, c(0.5)) + delta,
         col = 'white', lwd = 4.5)

segments(x0 = quantile(tmean_20802100_med, c(0.5)),
         y0 = quantile(csdm_fitness_med_20802100, c(0.25)) + delta, y1 = quantile(csdm_fitness_med_20802100, c(0.75)) + delta,
         col = 'white', lwd = 4.5)

points(x = quantile(tmean_20802100_med, c(0.5)), y = quantile(csdm_fitness_med_20802100, c(0.5))+ delta,
       pch = 19, col = 'white', cex = 1.3)

segments(x0 = quantile(tmean_20802100_med, c(0.25)), x1 = quantile(tmean_20802100_med, c(0.75)),
         y0 = quantile(csdm_fitness_med_20802100, c(0.5)) + delta,
         col = '#ed947e', lwd = 2.5)

segments(x0 = quantile(tmean_20802100_med, c(0.5)),
         y0 = quantile(csdm_fitness_med_20802100, c(0.25)) + delta, y1 = quantile(csdm_fitness_med_20802100, c(0.75)) + delta,
         col = '#ed947e', lwd = 2.5)

points(x = quantile(tmean_20802100_med, c(0.5)), y = quantile(csdm_fitness_med_20802100, c(0.5))+ delta,
       pch = 19, col = '#ed947e', cex = 1)




# Boreal



segments(x0 = quantile(tmean_hist_bor, c(0.25)), x1 = quantile(tmean_hist_bor, c(0.75)),
         y0 = -quantile(mech_fitness_bor_hist , c(0.5)) - delta,
         col = 'white', lwd = 4.5)

segments(x0 = quantile(tmean_hist_bor, c(0.5)),
         y0 = -quantile(mech_fitness_bor_hist , c(0.25)) - delta, y1 = -quantile(mech_fitness_bor_hist , c(0.75)) - delta,
         col = 'white', lwd = 4.5)

points(x = quantile(tmean_hist_bor, c(0.5)), y = -quantile(mech_fitness_bor_hist , c(0.5))- delta,
       pch = 19, col = 'white', cex = 1.3)

segments(x0 = quantile(tmean_hist_bor, c(0.25)), x1 = quantile(tmean_hist_bor, c(0.75)),
         y0 = -quantile(mech_fitness_bor_hist , c(0.5)) - delta,
         col = '#4faba0', lwd = 2.5)

segments(x0 = quantile(tmean_hist_bor, c(0.5)),
         y0 = -quantile(mech_fitness_bor_hist , c(0.25)) - delta, y1 = -quantile(mech_fitness_bor_hist , c(0.75))- delta,
         col = '#4faba0', lwd = 2.5)

points(x = quantile(tmean_hist_bor, c(0.5)), y = -quantile(mech_fitness_bor_hist , c(0.5))- delta,
       pch = 19, col = '#4faba0', cex = 1)



segments(x0 = quantile(tmean_20802100_bor, c(0.25)), x1 = quantile(tmean_20802100_bor, c(0.75)),
         y0 = -quantile(mech_fitness_bor_20802100 , c(0.5)) - delta,
         col = 'white', lwd = 4.5)

segments(x0 = quantile(tmean_20802100_bor, c(0.5)),
         y0 = -quantile(mech_fitness_bor_20802100 , c(0.25))- delta, y1 = -quantile(mech_fitness_bor_20802100 , c(0.75)) - delta,
         col = 'white', lwd = 4.5)

points(x = quantile(tmean_20802100_bor, c(0.5)), y = -quantile(mech_fitness_bor_20802100 , c(0.5))- delta,
       pch = 19, col = 'white', cex = 1.3)

segments(x0 = quantile(tmean_20802100_bor, c(0.25)), x1 = quantile(tmean_20802100_bor, c(0.75)),
         y0 = -quantile(mech_fitness_bor_20802100 , c(0.5)) - delta,
         col = '#4faba0', lwd = 2.5)

segments(x0 = quantile(tmean_20802100_bor, c(0.5)),
         y0 = -quantile(mech_fitness_bor_20802100 , c(0.25)) - delta, y1 = -quantile(mech_fitness_bor_20802100 , c(0.75)) - delta,
         col = '#4faba0', lwd = 2.5)

points(x = quantile(tmean_20802100_bor, c(0.5)), y = -quantile(mech_fitness_bor_20802100 , c(0.5))- delta,
       pch = 19, col = '#4faba0', cex = 1)

arrows(x0 = quantile(tmean_hist_bor, c(0.5))+0.2, x1 = quantile(tmean_20802100_bor, c(0.5))-0.2,
       y0 = -quantile(mech_fitness_bor_hist, c(0.5)) - delta - 0.02, y1 = -quantile(mech_fitness_bor_20802100, c(0.5)) - delta + 0.02,
       length = 0.1, angle = 30, col = '#37776f', lwd = 1.5)



segments(x0 = quantile(tmean_hist_bor, c(0.25)), x1 = quantile(tmean_hist_bor, c(0.75)),
         y0 = quantile(csdm_fitness_bor_hist, c(0.5)) + delta,
         col = 'white', lwd = 4.5)

segments(x0 = quantile(tmean_hist_bor, c(0.5)),
         y0 = quantile(csdm_fitness_bor_hist, c(0.25)) + delta, y1 = quantile(csdm_fitness_bor_hist, c(0.75)) + delta,
         col = 'white', lwd = 4.5)

points(x = quantile(tmean_hist_bor, c(0.5)), y = quantile(csdm_fitness_bor_hist, c(0.5))+ delta,
       pch = 19, col = 'white', cex = 1.3)

segments(x0 = quantile(tmean_hist_bor, c(0.25)), x1 = quantile(tmean_hist_bor, c(0.75)),
         y0 = quantile(csdm_fitness_bor_hist, c(0.5)) + delta,
         col = '#4faba0', lwd = 2.5)

segments(x0 = quantile(tmean_hist_bor, c(0.5)),
         y0 = quantile(csdm_fitness_bor_hist, c(0.25)) + delta, y1 = quantile(csdm_fitness_bor_hist, c(0.75)) + delta,
         col = '#4faba0', lwd = 2.5)

points(x = quantile(tmean_hist_bor, c(0.5)), y = quantile(csdm_fitness_bor_hist, c(0.5))+ delta,
       pch = 19, col = '#4faba0', cex = 1)



segments(x0 = quantile(tmean_20802100_bor, c(0.25)), x1 = quantile(tmean_20802100_bor, c(0.75)),
         y0 = quantile(csdm_fitness_bor_20802100, c(0.5)) + delta,
         col = 'white', lwd = 4.5)

segments(x0 = quantile(tmean_20802100_bor, c(0.5)),
         y0 = quantile(csdm_fitness_bor_20802100, c(0.25)) + delta, y1 = quantile(csdm_fitness_bor_20802100, c(0.75)) + delta,
         col = 'white', lwd = 4.5)

points(x = quantile(tmean_20802100_bor, c(0.5)), y = quantile(csdm_fitness_bor_20802100, c(0.5))+ delta,
       pch = 19, col = 'white', cex = 1.3)

segments(x0 = quantile(tmean_20802100_bor, c(0.25)), x1 = quantile(tmean_20802100_bor, c(0.75)),
         y0 = quantile(csdm_fitness_bor_20802100, c(0.5)) + delta,
         col = '#4faba0', lwd = 2.5)

segments(x0 = quantile(tmean_20802100_bor, c(0.5)),
         y0 = quantile(csdm_fitness_bor_20802100, c(0.25)) + delta, y1 = quantile(csdm_fitness_bor_20802100, c(0.75)) + delta,
         col = '#4faba0', lwd = 2.5)

points(x = quantile(tmean_20802100_bor, c(0.5)), y = quantile(csdm_fitness_bor_20802100, c(0.5))+ delta,
       pch = 19, col = '#4faba0', cex = 1)

arrows(x0 = quantile(tmean_hist_bor, c(0.5))+0.4, x1 = quantile(tmean_20802100_bor, c(0.5))-0.3,
       y0 = quantile(csdm_fitness_bor_hist, c(0.5)) + delta + 0.03, y1 = quantile(csdm_fitness_bor_20802100, c(0.5)) + delta - 0.03,
       length = 0.1, angle = 25, col = '#37776f', lwd = 1.5)

