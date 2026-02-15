

surf_per_species <- data.frame()
for(s in speciesnames){
  
  sp <- ifel(domspecies %in% s, 1, NA)
  sp <- mask(sp, surf50)
  spsurf <- nrow(values(sp, na.rm = TRUE))*1 # in km2
  
  surf_per_species <- rbind(surf_per_species,
                            data.frame(sp = s, surf = spsurf))
  
}

convert_names <-
  data.frame(
    sp = speciesnames,
    spname = c('Abies', 'Others', 'Betula', 'Others', 'Others',
               'Others','Others', 'Others', 'Fagus',
               'Fraxinus', 'Others', 'Picea', 'Others',
               'Others', 'Pinus sylvestris', 
               'Others', 'Others', 'Other Quercus', 
               'Quercus robur & Quercus petraea', 'Others'
               ),
    modeled = c('Modeled in this study', 'Not in this study', 'Modeled in this study', 'Not in this study', 'Not in this study',
               'Not in this study','Not in this study', 'Not in this study', 'Modeled in this study',
               'Modeled in this study', 'Not in this study', 'Not in this study', 'Not in this study',
               'Not in this study', 'Modeled in this study', 
               'Not in this study', 'Not in this study', 'Modeled in this study', 
               'Modeled in this study', 'Not in this study'
    )
  )

piedf <- merge(convert_names, surf_per_species)

library(ggplot2)
library(webr)
library(dplyr)

piedf <- piedf %>% group_by(spname, modeled) %>% summarise(surf = sum(surf)) %>% arrange(surf)

PieDonut(piedf, aes(modeled, spname, count=surf), explode = 1, 
         showPieName = FALSE, ratioByGroup = FALSE, labelpositionThreshold = 1,
         donutLabelSize = 4, labelposition = 5)
