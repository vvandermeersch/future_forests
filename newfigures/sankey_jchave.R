
wd <- "~/projects/future_forests"

species <- "fagus_sylvatica"
scenario <- "ssp245"
type <- "csdm"
nsim <- 20
ths_ag_dist <- 0.5*nsim

sim_dist_2090 <- readRDS(file = file.path(wd,"newfigures","data","maps", paste0(species, "_", type, "_2090_dist_", scenario, ".rds")))

sim_distribution <- readRDS(file = file.path(wd,"newfigures","data","maps", paste0(species, "_", type, "_distribution_", scenario, ".rds")))
crs(sim_distribution) <- "EPSG:4326"

histemp <- subset(sim_dist_2090, 2)
histemp <- ifel(histemp == '-1', 1, 0)
histemp <- ifel(is.na(histemp), 0, histemp)
histr <- subset(sim_dist_2090, 1)
histr <- histr + histemp

dist <- sim_distribution %>% crop(ext(-10.5, 31.7, 34.6, 71.2)) %>% project("EPSG:3035")
dist <- ifel(dist > ths_ag_dist, 1, 0)
dist <- c(histr, dist)

test <- as.data.frame(dist) 
test <- test[which(!(test$historical == 0 & test$`2050` == 0 & test$`2090` == 0)), ]
test <- test[which(!(test$historical == 0 & test$`2050` == 0 & test$`2090` == 0)), ]
test[which((test$`2050` == 0 & test$`2090` == 1)), '2050'] <- 2
test[which((test$historical == 0 & test$`2050` == 1)), 'historical'] <- 2
#test[which((test$historical == 0 & test$`2050` ==  2)), 'historical'] <- NA

# test[which((test$`2050` == 2 & test$`2090` == 1)), '2090'] <- 2
test <- test %>%
  make_long(historical, `2050`, `2090`)


test2 <- test[which(!(test$node %in% 0 & test$next_node%in%0 & test$next_x %in% 2090)),]

plottest <- ggplot(test, 
       aes(x = x, 
           next_x = next_x, 
           node = node, 
           next_node = next_node,
           fill = factor(node))) +
  geom_alluvial(flow.alpha = 0.5, node.color = 0, width = 0.03, space = 5000) +
  theme_void()


ggsave(plottest, filename = file.path(wd, 'test2.pdf'))

temp <- test[which((test$node == 1 & test$next_node == 0)),]


data.frame(
  x = rep(c(2050, 2090), 2),
  next_x = rep(c(2090, NA), 2),
  node = c(NA,1,0,0),
  next_node = c(1, NA, 0, NA)
) %>% 
  ggplot(aes(x = x, 
             next_x = next_x, 
             node = node, 
             next_node = next_node,
             fill = as.character(node))) +
  geom_sankey(flow.alpha = 0.5, node.color = NA, width = 0.03, type = "alluvial", space = 1) 
