indClusters <- function(data, clust, alphabet2) {
  outPlot <- SpatialDimPlot(
    data,
    cells.highlight = CellsByIdentities(object = data, idents = clust), # Choose Clusters to Present
    facet.highlight = TRUE,
    pt.size.factor = 3.5,
    stroke = 0,
    cols.highlight = c(alphabet2, 'grey80')
  ) & 
  scale_x_continuous(name="X",expand = expansion(mult = c(0.008, 0.008))) &
    scale_y_continuous(name="Y",expand = expansion(mult = c(0.008, 0.008))) &
    theme(plot.background =  element_rect(fill = "transparent", color=NA),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          title = element_blank())
  outPlot$layers[[1]]$aes_params <- c(outPlot$layers[[1]]$aes_params, shape=15) 
  outPlot
}