Ind1$X = row.names(Ind1)
Ind1 <- Ind1 %>% separate(X, c("A","B"),sep = "x")
IndPlotx <-
  ggplot(Ind2, aes(
    x = as.numeric(A),
    y = as.numeric(B),
    color = Ind2[, "RBFOX3"]
  )) +
  scale_color_gradientn(colours = c("lemonchiffon","lightsalmon2","darkviolet"),
                        oob = scales::squish) +
  ggtitle(i) +
  guides(colour = guide_colourbar(barwidth = 1, barheight = 10)) +
  geom_point(shape = 15, size = 1, alpha = 1) +
  expand_limits(x = 0, y = 0) +
  scale_x_continuous(
    name = "X",
    limits = c(NA, NA),
    expand = expansion(mult = c(-0.013, -0.013))
  ) +
  scale_y_reverse(
    name = "Y",
    limits = c(NA, NA),
    expand = expansion(mult = c(-0.013, 0.008))
  ) +
  coord_equal(xlim = c(0, 51), ylim = c(51, 1)) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 25,
      face = "bold"
    ),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    #legend.title = element_text(colour="black", size=15, face="bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) + NoAxes()
ggsave("PROX1new3.png", plot = IndPlotx, path = dir, dpi = 300, bg = NULL)
