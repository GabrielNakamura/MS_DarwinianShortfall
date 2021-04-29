
# libraries and data ------------------------------------------------------
library(rgdal)     
library(ggplot2)  
library(ggmap)
library(cowplot)
phylo_drainages <- readRDS(here::here("output", "phylo_drainages.rds"))
shapefile <- rgdal::readOGR(here::here("data", "Basin042017_3119.shp"))
PD_deficit_all <- readRDS(here::here("output", "PD_deficit_all.rds"))
sort(table(phylo_drainages$Insertions_data$o), decreasing = T)
phylo_drainages$Insertions_data[which(phylo_drainages$Insertions_data$o == "Siluriformes"), ]
# formating shapefile for plot --------------------------------------------

for(i in 1:length(shapefile@polygons)){
  shapefile@polygons[[i]]@ID <- shapefile@data$BasinName[i]
}
shapefile_df <- fortify(shapefile)
PD_dataframe <- data.frame(deficit = unlist(PD_deficit_all), id = names(PD_deficit_all))
PD_dataframe_siluriformes <- data.frame(deficit = unlist(PD_deficit_Siluriformes), id = names(PD_deficit_Siluriformes))
PD_dataframe_characiformes <- data.frame(deficit = unlist(PD_deficit_Characiformes), id = names(PD_deficit_Characiformes))
PD_dataframe_cichliformes <- data.frame(deficit = unlist(PD_deficit_Cichliformes), id = names(PD_deficit_Cichliformes))

deficit_value <- unlist(lapply(shapefile_df$id, function(x) 
  PD_dataframe[which(x == PD_dataframe$id), "deficit"]
)
)
deficit_value_siluriformes <- unlist(lapply(shapefile_df$id, function(x) 
  PD_dataframe_siluriformes[which(x == PD_dataframe_siluriformes$id), "deficit"]
)
)
deficit_value_characiformes <- unlist(lapply(shapefile_df$id, function(x) 
  PD_dataframe_characiformes[which(x == PD_dataframe_characiformes$id), "deficit"]
)
)
deficit_value_cichliformes <- unlist(lapply(shapefile_df$id, function(x) 
  PD_dataframe_cichliformes[which(x == PD_dataframe_cichliformes$id), "deficit"]
)
)

data_map_PDeficit <- data.frame(shapefile_df, deficit = deficit_value)
data_map_PDeficit_siluriformes <- data.frame(shapefile_df, deficit = deficit_value_siluriformes)
data_map_PDeficit_characiformes <- data.frame(shapefile_df, deficit = deficit_value_characiformes)
data_map_PDeficit_cichliformes <- data.frame(shapefile_df, deficit = deficit_value_cichliformes)


# plotting map ------------------------------------------------------------

p <- ggplot(data = data_map_PDeficit) +
  geom_polygon(aes(x = long, y = lat, group = id, fill = deficit),
               colour = "black")
Map_darwinian_shortfall <- p + scale_fill_viridis_c()


# plotting together maps for orders ---------------------------------------

# plotting map ------------------------------------------------------------
p_siluri <- ggplot(data = data_map_PDeficit_siluriformes) +
  geom_polygon(aes(x = long, y = lat, group = id, fill = deficit),
               colour = "black")
Map_darwinian_shortfall_siluri <- p_siluri + scale_fill_viridis_c()

p_characiformes <- ggplot(data = data_map_PDeficit_characiformes) +
  geom_polygon(aes(x = long, y = lat, group = id, fill = deficit),
               colour = "black")
Map_darwinian_shortfall_characi <- p_characiformes + scale_fill_viridis_c()

p_cichli <- ggplot(data = data_map_PDeficit_cichliformes) +
  geom_polygon(aes(x = long, y = lat, group = id, fill = deficit),
               colour = "black")
Map_darwinian_shortfall_cichli <- p_cichli + scale_fill_viridis_c()

pgrid_darwinian_short <- plot_grid(
  Map_darwinian_shortfall_characi + theme(legend.position="none"),
  Map_darwinian_shortfall_cichli + theme(legend.position="none"),
  Map_darwinian_shortfall_siluri + theme(legend.position="none"),
  Map_darwinian_shortfall + theme(legend.position="none"), 
  labels = c('Characiformes', 'Cichliformes', "Siluriformes", "All"),
  nrow = 4
)

# extract the legend from one of the plots
legend <- get_legend(
  Map_darwinian_shortfall + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "right")
)

plot_all_darwinianShorfall <- plot_grid(pgrid_darwinian_short, 
                                        legend, ncol = 1, rel_heights = c(1, .1)
)




# saving map --------------------------------------------------------------
pdf("Map_darwinian.pdf", width = 15, height = 5)
print(Map_darwinian_shortfall)
dev.off()

pdf(here::here("output", "figures", "plot_all_darwinianShortfall.pdf"), width = 15, height = 15)
print(plot_all_darwinianShorfall)
dev.off()
