
methods <- c("expert", "fitted", "correlative")
Ngcm <- 5
Nssp <- 2

simulations$smy <- NA
for(m in 1:3){
  
  ci <- methods[m]
  
  for(m in 1:Ngcm){
    
    mi <- gcms[m]
    
    for(s in 1:Nssp){
      
      si <- scenarios[s]
      
      y <- simulations %>% dplyr::filter(method == ci & ssp == si & gcm == mi) %>% dplyr::select(y) %>% unlist()
      x <- simulations %>% dplyr::filter(method == ci & ssp == si & gcm == mi) %>% dplyr::select(year) %>% unlist()
      x <- x - (min(x)) + 1
      
      # fit 4th order polynomial
      # fit <- lm(y~poly(x,ndeg_poly,raw=TRUE))
      # smy <- predict(fit,data.frame(x=x))
      
      # fit cubic spline
      if(is.null(df)){
        fit <- smooth.spline(x = x, y = y)
      }else{
        fit <- smooth.spline(x = x, y = y, df = df)
      }
      smy <- unlist(predict(fit,data.frame(x=x))$y)
      
      # plot(y~x) ; lines(y~x) ; lines(smy~x, col = "red", lty = 2, lwd = 2) ; lines(smy2~x, col = "blue", lty = 2, lwd = 2)
      
      simulations[simulations$method == ci & simulations$ssp == si & simulations$gcm == mi, "smy"] <- smy
      
    }
  }
}
