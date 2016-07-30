# K-means clustering model diagnostics

km_diagnostics <- function() {
  
  require("shiny")
  require("plotly")
  require("miniUI")
  require("cluster")
  require("clValid")
  
  ui <- miniPage(
    gadgetTitleBar("k-means clustering model diagnostics"),
    miniContentPanel(
      fluidRow(
        fluidRow(align = "center",
                 fluidRow(
                   column(6,selectInput("model", "Select model object", choices = c("",unlist(ls(envir = parent.frame(1)))), selected = NULL, width = "50%")),
                   column(6,selectInput("indata", "Select the input data object", choices = c("",unlist(ls(envir = parent.frame(1)))), selected = NULL, width = "50%"))
                 ),br(),
                 actionButton("load", "Load model object")
        ),
        fluidRow( uiOutput("performance"))
      )
    )
  )
  
  server <- function(input, output) {
    
    values <- reactiveValues()
    values$fit_obj <- list()
    values$inData <- data.frame()
    values$dunn <- c()
    
    observeEvent(input$load, {
      values$fit_obj <- get(input$model, envir = parent.frame(1))
      values$inData <- get(input$indata, envir = parent.frame(1))
      values$dunn <- dunn(clusters = values$fit_obj[["cluster"]], Data = values$inData)
    })
    
    
    output$performance <- renderUI({
      if (inherits(values$fit_obj, "kmeans")){
        uiOutput("kmModel")
      } else {
        fluidRow(align = "center", HTML("<br><br>Select and load a k-means clustering model object into Workspace.
                                        "))
      }
      })
    
    
    output$kmModel <- renderUI({
      fluidRow(
        br(),br(),
        fluidRow(align = "center", HTML("<h3>Model Summary:</h3>")),
        br(),
        fluidRow(align="center",
                 column(1),
                 column(5,verbatimTextOutput("km_summary")),
                 column(5),
                 column(1)),
        br(),
        fluidRow(align = "center",br(),
                 HTML("<h3>Clustering performance indices and plots:</h3>"),br(),
                 fluidRow(align = "center",
                          column(1),
                          column(5,uiOutput("km_plot")),
                          column(5,uiOutput("silhouette")),
                          column(1)
                 ),
                 br()
        )
      )
    })
    
    output$km_summary <- renderPrint({
      fit <- values$fit_obj
      print(fit)
    })
    
    output$km_plot <- renderUI({
      fluidRow(
        HTML("<b><u>Visualize the clusters: two variable plot</u></b><br>"),br(),
        fluidRow(
          column(6, selectInput("x", "Select X:", choices = names(values$inData), selected = NULL)),
          column(6, selectInput("y", "Select Y:", choices = names(values$inData), selected = NULL))
        ),
        plotlyOutput("varplot", width = "90%")
      )
    })
    
    output$varplot <- renderPlotly({
      dat <- values$inData
      fit <- values$fit_obj
      plt <- plot_ly(x = dat[,input$x], y = dat[,input$y], mode = "markers", color = factor(fit$cluster))
      plt %>% layout(xaxis = list(title = input$x), yaxis = list(title = input$y))
    })
    
    output$silhouette <- renderUI({
      fluidRow(
        fluidRow(align = "center",
                 HTML("<br><b><u>Silhouette Plot</u></b>")
        ), 
        fluidRow(align = "center",
                 plotOutput("silPlot", width = "90%")
        ),
        HTML(paste('<br><br><div align = "center"><b><u> Dunn Index: </u></b>', '<h4>', round(values$dunn, digits = 3), '</h4></div>'))
      )
    })
    
    output$silPlot <- renderPlot({
      fit <- values$fit_obj
      dat <- values$inData
      len <- length(fit$size)
      dissE <- daisy(dat)
      plot(silhouette(fit$cluster, dissE), col = 1:len, main = "")
    })
    
    observeEvent(input$done, {
      stopApp()
    })
    }
  
  runGadget(ui, server, viewer = paneViewer())
}

