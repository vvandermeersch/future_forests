rm(list=ls())
library(terra)
wd <- '/home/victor/projects/future_forests'

files <- list.files(path = file.path(wd, 'data/misc/EU_TreeMap'), pattern = "\\.tif$", full.names = TRUE)
speciesnames <- list.files(path = file.path(wd, 'data/misc/EU_TreeMap'), pattern = "\\.tif$", full.names = FALSE)
species <- rast(files)
names(species) <- speciesnames
domspecies <- as.factor(which.max(species))
valdom <- values(domspecies)
values(domspecies) <- speciesnames[valdom]
plot(domspecies)


list.species <- c('AbiesSpp.tif', 'BetulaSpp.tif', 'FagusSpp.tif', 'FraxinusSpp.tif',
                   'PinusSylvestris.tif', 'QuercusMisc.tif', 'QuercusRoburPetraea.tif')
selspecies <- ifel(domspecies %in% list.species, 1, NA)
plot(selspecies, add = TRUE)


selsurf/totsurf*100

# plot(domspecies)
# selspecies <- ifel(domspecies %in% c('PinesMisc.tif'), 1, NA)
# plot(selspecies)



surface <- rast(file.path(wd, 'data/misc/forest_v2011_GISdata', 'forest.img'))
surface <- ifel(surface > 100, NA, surface)
values(surface) <- as.numeric(values(surface))
surf50 <- ifel(surface < 10, NA, 1)
surf50 <- crop(surf50, selspecies)
surf50 <- resample(surf50, selspecies)

domspecies2 <- mask(domspecies, surf50)
selspecies2 <- mask(selspecies, surf50)

plot(selspecies)
plot(surf50, add = TRUE, col = 'green')
plot(selspecies2)

totsurf <- nrow(values(domspecies2, na.rm = TRUE))*1 # in km2
selsurf <- nrow(values(selspecies2, na.rm = TRUE))*1 # in km2
selsurf/totsurf*100

pin <- ifel(domspecies %in% c('QuercusMisc.tif'), 1, NA)
pin2 <- mask(pin, surf50)
pinsurf <- nrow(values(pin2, na.rm = TRUE))*1 # in km2
pinsurf/totsurf*100
