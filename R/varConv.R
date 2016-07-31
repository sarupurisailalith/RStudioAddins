# convert variable types in a data frame 

var_conv <- function() {
  
  require("shiny")
  require("miniUI")
  
  ui <- miniPage(
    gadgetTitleBar("Convert variable types in a data frame"),
    miniContentPanel(
      fluidRow(align = "center",
               column(1),
               column(10, br(),
                      HTML('<b> Creates a data frame "df_converted" after clicking on "Done"'),br(),
                      fluidRow(
                        column(6, selectInput("dat", "Select data frame", choices = c(unlist(ls(envir = parent.frame(1)))), selected = NULL)),
                        column(6, selectInput("vars", "Select variables", choices = NULL, multiple = TRUE))
                      ),
                      br(),
                      fluidRow(
                        column(4),
                        column(4, uiOutput("buttons")),
                        column(4)
                      ),
                      br(),
                      fluidRow(verbatimTextOutput("struct"))),
               column(1)
      )
    )
  )
  
  server <- function(input, output, session) {
    
    values <- reactiveValues()
    values$dat <- data.frame()
    
    observeEvent(input$dat,{
      values$dat <- get(input$dat, envir = parent.frame(1))
      updateSelectInput(session, inputId = "vars",label = "Select variables", 
                        choices = names(values$dat), selected = NULL)
    })
    
    output$buttons <- renderUI({
      if(!is.null(input$vars)){
        fluidRow(
          column(4,actionButton("num","Numeric")),
          column(4,actionButton("fact","Factor")),
          column(4,actionButton("char","Character"))
        )
      }
    })
    
    observeEvent(input$num,{
      func <- function(x){
        return(as.numeric(as.character(x)))
      }
      values$dat[,input$vars]<- lapply(values$dat[,input$vars, drop = FALSE], FUN = func)
    })
    
    observeEvent(input$fact,{
      values$dat[,input$vars]<- lapply(values$dat[,input$vars, drop = FALSE], FUN = factor)
    })
    
    observeEvent(input$char,{
      func <- function(x){
        return(as.character(x))
      }
      values$dat[,input$vars]<- lapply(values$dat[,input$vars, drop = FALSE], FUN = func)
    })
    
    output$struct <- renderPrint({
      str(values$dat)
    })
    
    observeEvent(input$done, {
      stopApp('<<-'(df_converted, values$dat))
    })
  }
  
  runGadget(ui, server, viewer = paneViewer())
}

