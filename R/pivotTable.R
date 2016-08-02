# pivot table 

pivot_table <- function() {
  
  require("shiny")
  require("miniUI")
  require("rpivotTable")
  
  ui <- miniPage(
    gadgetTitleBar("Pivot table"),
    miniContentPanel(
      fluidRow(column(1),
               column(10,fluidRow(align = "center", selectInput("dat", "select data frame",choices = c(unlist(ls(envir = parent.frame(1)))), selected = NULL, width = "50%")),
                      br(),br(),
                      uiOutput("pivotTable")),
               column(1)
      )
    )
  )
  
  server <- function(input, output) {
    
    values <- reactiveValues()
    values$df <- data.frame()
    
    observeEvent(input$dat,{
      values$df <- get(input$dat, envir = parent.frame(1))
    })
    
    output$pivotTable <- renderUI({
      if(!is.null(input$dat)){
        rpivotTableOutput("ptable")
      }
    })
    
    output$ptable <- renderRpivotTable({
      dat <- values$df
      rpivotTable(dat)
    })
    
    observeEvent(input$done, {
      stopApp()
    })
  }
  
  runGadget(ui, server, viewer = paneViewer())
}

