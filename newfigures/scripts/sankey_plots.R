

wd <- "~/projects/future_forests"

species <- "fagus_sylvatica"
scenario <- "ssp245"

type <- "csdm"
nsim <- 20
data.sankey <- prepare_sankey_data(species, type, nsim)

sankey.plot <- ggplot(data.sankey, 
                      aes(x = x, next_x = next_x, 
                          node = node, 
                          next_node = next_node,
                          fill = factor(node))) +
  geom_alluvial(flow.alpha = 0.5, node.color = 0, width = 0.03, space = 5000) +
  theme_void()


ggsave(sankey.plot, filename = file.path(wd,  'newfigures/files/sankey','sankey_fagus_csdm.pdf'), height = 2, width = 8)



type <- "fitted"
nsim <- 50
data.sankey <- prepare_sankey_data(species, type, nsim)

sankey.plot <- ggplot(data.sankey, 
                      aes(x = x, next_x = next_x, 
                          node = node, 
                          next_node = next_node,
                          fill = factor(node))) +
  geom_alluvial(flow.alpha = 0.5, node.color = 0, width = 0.03, space = 5000) +
  theme_void()


ggsave(sankey.plot, filename = file.path(wd,  'newfigures/files/sankey','sankey_fagus_fitted.pdf'), height = 2, width = 8)



type <- "expert"
nsim <- 5
data.sankey <- prepare_sankey_data(species, type, nsim)

sankey.plot <- ggplot(data.sankey, 
                      aes(x = x, next_x = next_x, 
                          node = node, 
                          next_node = next_node,
                          fill = factor(node))) +
  geom_alluvial(flow.alpha = 0.5, node.color = 0, width = 0.03, space = 5000) +
  theme_void()


ggsave(sankey.plot, filename = file.path(wd,  'newfigures/files/sankey','sankey_fagus_expert.pdf'), height = 2, width = 8)
