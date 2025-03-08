

woodpro <- rast('/home/victor/Downloads/WoodProductionMaps/woodprod_average.tif')

# woodpro <- woodpro*1000/100
plot(woodpro)
total<- global(woodpro, fun = sum, na.rm = T)*1000 # 4.2e+09 m3?
lines(ctrcont, col = 'white')

total2 = global(test1, fun = sum, na.rm = T)*1000+
  global(test2, fun = sum, na.rm = T)*1000+
  global(test3, fun = sum, na.rm = T)*1000

ctrcont <- ifel(ecorast2 %in% c('8','7', '1', '4', '6', 'Other')& agreement == 0, 1, 0) %>% as.polygons() %>% disagg()
ctrcont$exp <- expanse(ctrcont)
ctrcont <- ctrcont[ctrcont$exp < 1e13] %>% terra::aggregate()
test1<- mask(woodpro, ctrcont, touches = TRUE)
global(test1, fun = sum, na.rm = T)*1000/total2*100 # 1.7e+09 m3? 41.9% harvested timber volumes for 2000–2010

ctrbor <- ifel(ecorast2 %in% c('8','7', '1', '4', '6', 'Other')& suit_r > 0 & agreement == 1, 1, 0) %>% as.polygons() %>% disagg()
ctrbor$exp <- expanse(ctrbor)
ctrbor <- ctrbor[ctrbor$exp < 1e13] %>% terra::aggregate()
test2<- mask(woodpro, ctrbor, touches = TRUE)
global(test2, fun = sum, na.rm = T)*1000/total2*100 # 1.3e+09 m3? 32.1% harvested timber volumes for 2000–2010

ctratlmed <- ifel(ecorast2 %in% c('8','7', '1', '4', '6', 'Other') & suit_r <= 0 & agreement == 1, 1, 0) %>% as.polygons() %>% disagg()
ctratlmed$exp <- expanse(ctratlmed)
ctratlmed <- ctratlmed[ctratlmed$exp < 1e13] %>% terra::aggregate()
test3<- mask(woodpro, ctratlmed, touches = TRUE)
global(test3, fun = sum, na.rm = T)*1000/total2*100 # 1.3e+09 m3? 26.0% harvested timber volumes for 2000–2010

plot(merge(test1, test2, test3))

plot(ctrcont+ctrbor+ctratlmed)
