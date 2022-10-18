fullClusters <- function(data,alphabet) {
  DimPlot(data, reduction = "umap", label = TRUE) + 
    scale_color_manual(values = alphabet[1:(data@active.ident %>% unique %>% length)])
}