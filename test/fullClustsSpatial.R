fullClustsSpatial <- function(data,alphabet) {
  outPlot <- SpatialDimPlot(data, 
                 label.size = 3, 
                 pt.size = 3.5,
                 cols = alphabet[1:(data@active.ident %>% unique %>% length )], 
                 stroke = 0) &
    scale_x_continuous(name="X",expand = expansion(mult = c(0.008, 0.008))) &
    scale_y_continuous(name="Y",expand = expansion(mult = c(0.008, 0.008))) &
    theme(legend.position = "right", 
          plot.background =  element_rect(fill = "transparent", color=NA),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
  outPlot$layers[[1]]$aes_params <- c(outPlot$layers[[1]]$aes_params, shape=15) 
  outPlot
}