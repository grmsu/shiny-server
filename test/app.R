library(shiny)
library(shinyjs)
library(shinyWidgets)
library(Seurat)
library(dplyr)
library(tidyr)
library(ggplot2)
library(BuenColors)
source("umiHeatmap.R")
source("geneHeatmap.R")
source("indGenePlot.R")
source("fullClustsSpatial.R")
source("indClusters.R")

shinySamp <- readRDS("data/testClust.rds")
indList <- readRDS("data/magictest.rds")
counts_Matrix2 <- readRDS("data/countsMatrix.rds")
count <- readRDS("data/UMI.rds")
gene_count <- readRDS("data/geneCount.rds")

meanUMI <- as.character(mean(shinySamp$nFeature_Spatial))
meanGene <- as.character(mean(shinySamp$nCount_Spatial))
totGene <- as.character(length(rownames(shinySamp[["Spatial"]]@data)))
alphabet = c("lightskyblue","gold","darkolivegreen4","purple","red3","blue3","darkgoldenrod3")
#numList <- c(1:(shinySamp@active.ident %>% unique %>% length), (shinySamp@active.ident %>% unique %>% length)+1)
numList <- (1:((shinySamp@meta.data$seurat_clusters %>% unique %>% length)+2))
names(numList) <- c("None",(as.character(sort(shinySamp@meta.data$seurat_clusters %>% unique))),"All")
# Define UI ----
ui <- fluidPage(
  theme = shinythemes::shinytheme("readable"),
  useShinyjs(),
  #titlePanel("EZdim-GS"),
  
  sidebarLayout(
    sidebarPanel(
     helpText(p(paste0("Total Genes Detected: ", totGene)), 
              p(paste0("Average UMIs per pixel: ",meanUMI)), 
              p(paste0("Average genes per pixel: ",meanGene)))
    ),
    mainPanel(
      fluidRow(

        selectInput("Mode", h3("Mode"), 
                    choices = list("None" = 1,"Quality Control" = 2, "Clusters" = 3,
                                   "Gene Expression" = 4), selected = 1)),
      switchInput("qcSwitch",onLabel = "UMI",offLabel = "Gene", handleWidth = '50'),
      sliderInput("qcSlider", h5("Select Range"),
                  min = 0, max = 5000, value = 2000),
      selectInput("Select", h4("Select Cluster"), 
                  choices = numList, selected = 1),
      textInput("geneText", h4("Enter Gene Name"), 
                value = ""),
      plotOutput("qcUMI"),
      plotOutput("qcGene"),
      plotOutput("indGenePlot"),
      plotOutput("clustPlot"),
      plotOutput("indClustPlot")
    )
  )
)
  # Define server logic ----
  server <- function(input, output) {
    
    shinyjs::hide("qcSwitch")
    shinyjs::hide("qcSlider")
    shinyjs::hide("Select")
    shinyjs::hide("geneText")
    shinyjs::hide("qcUMI")
    shinyjs::hide("qcGene")
    shinyjs::hide("indGenePlot")
    shinyjs::hide("indClustPlot")
    shinyjs::hide("clustPlot")

    observeEvent(input$Mode,
                 {if (input$Mode == "1") {
                   shinyjs::hide("qcSwitch")
                   shinyjs::hide("Select")
                   shinyjs::hide("geneText")
                   shinyjs::hide("qcUMI")
                   shinyjs::hide("qcGene")
                   shinyjs::hide("indGenePlot")
                   shinyjs::hide("indClustPlot")
                   shinyjs::hide("clustPlot")
                 }
                   else if (input$Mode == "2") {
                  shinyjs::show("qcSwitch")
                  shinyjs::hide("Select")
                  shinyjs::hide("geneText")
                  shinyjs::hide("indGenePlot")
                  shinyjs::hide("indClustPlot")
                  shinyjs::hide("clustPlot")
                 }
                   else if (input$Mode == "3") {
                     shinyjs::hide("qcSwitch")
                     shinyjs::show("Select")
                     shinyjs::hide("geneText")
                     shinyjs::hide("qcUMI")
                     shinyjs::hide("qcGene")
                     shinyjs::hide("indGenePlot")
                   }
                   else if (input$Mode == "4") {
                     shinyjs::hide("qcSwitch")
                     shinyjs::hide("Select")
                     shinyjs::show("geneText")
                     shinyjs::hide("qcUMI")
                     shinyjs::hide("qcGene")
                     shinyjs::show("indGenePlot")
                     shinyjs::hide("indClustPlot")
                     shinyjs::hide("clustPlot")
                   }
                   })
    observeEvent(input$Mode,
                 {if (input$Mode == "2") {
                   shinyjs::show("qcSlider")
                 }
                   else {
                     shinyjs::hide("qcSlider")
                   }
                 })
    ### UMI Gene heatmap Code
    observeEvent(c(input$qcSwitch,input$Mode),{
      if ((input$qcSwitch == TRUE)&(input$Mode == "2")) {
        output$qcUMI <- renderPlot({
          umiHeatmap(counts_Matrix2, input$qcSlider, count)
        })
        shinyjs::show("qcUMI")
        shinyjs::hide("qcGene")
      }
      else if ((input$qcSwitch == FALSE)&(input$Mode == "2")){
        output$qcGene <- renderPlot({
          geneHeatmap(counts_Matrix2, input$qcSlider, gene_count)
        })
        shinyjs::hide("qcUMI")
        shinyjs::show("qcGene")
      }
    })
    ### Clustering Code
    observeEvent(input$Select, {
      if ((as.numeric(input$Select) < length(numList))&(as.numeric(input$Select)>1)) {
          output$indClustPlot <- renderPlot({
            indClusters(shinySamp, (as.numeric(input$Select)-2), alphabet[[(as.numeric(input$Select)-1)]])
          })
          shinyjs::hide("clustPlot")
          shinyjs::show("indClustPlot")
        }
        else if (as.numeric(input$Select) == length(numList)){
          output$clustPlot <- renderPlot({
            fullClustsSpatial(shinySamp, alphabet)
            })
          shinyjs::hide("indClustPlot")
          shinyjs::show("clustPlot")
        }
      else {
        shinyjs::hide("indClustPlot")
        shinyjs::hide("clustPlot")
      }
    }
)
    ### Individual Gene Expression Code
    output$indGenePlot <- renderPlot({
      indGenePlot(indList, input$geneText)
    })
  }

# Run the app ----
shinyApp(ui = ui, server = server)