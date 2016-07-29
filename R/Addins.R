# Some addins that might come in handy during modelling activity
# The plots in the outputs are made using functions from plotly

# Linear regression diagnostics

lm_diagnostics <- function() {

  require("shiny")
  require("plotly")
  require("miniUI")

  # UI components of the gadget
  ui <- miniPage(
    gadgetTitleBar("Linear regression : Model Diagnostics"),
    miniContentPanel(
      fluidRow(
        fluidRow(align = "center",
                 selectInput("model", "Select model object", choices = c("",unlist(ls(envir = parent.frame(1)))), selected = NULL, width = "50%"),
                 br(),
                 actionButton("load", "Load model object")
        ),
        fluidRow( uiOutput("performance"))
      )
    )
  )

  server <- function(input, output) {

    values <- reactiveValues()
    values$fit_obj <- list()
    values$diagPlots <- list()

    # getting the model object into a reactive value obj on clicking the action button
    observeEvent(input$load, {
      values$fit_obj <- get(input$model, envir = parent.frame(1))
    })

    output$performance <- renderUI({
      if (inherits(values$fit_obj, "lm")){
        uiOutput("linearModel")
      } else {
        fluidRow(align = "center", HTML("<br><br>Select and load a linear regression model object into Workspace.
                                        "))
      }
      })

    output$linearModel <- renderUI({
      fluidRow(
        br(),br(),
        fluidRow(align = "center", HTML("<b>Model Summary:</b>")),
        br(),
        fluidRow(align="center",
                 verbatimTextOutput("lm_summary")),
        fluidRow(align = "center",br(),
                 HTML("<b>Diagnostic plots:</b>"),br(),
                 uiOutput("lm_diagPlots")
        )
      )
    })

    # printing the summary 
    output$lm_summary <- renderPrint({
      fit <- values$fit_obj
      tab <- summary(fit)
      #names(tab) <- c("Coefficients")
      print(tab)
    })

    output$lm_diagPlots <- renderUI({
      fluidRow(
        fluidRow(column(6,plotlyOutput("residvsfit")),
                 column(6,plotlyOutput("qqplot"))),
        br(),
        fluidRow(column(6,plotlyOutput("scale")),
                 column(6,plotlyOutput("leverage")))
      )
    })

    output$residvsfit <- renderPlotly({
      values$diagPlots[[1]]
    })

    output$qqplot <- renderPlotly({
      values$diagPlots[[2]]
    })

    output$scale <- renderPlotly({
      values$diagPlots[[3]]
    })

    output$leverage <- renderPlotly({
      values$diagPlots[[4]]
    })

    # generating plots using plotly and storing them in a list 
    observeEvent(input$load,{
      fit <- values$fit_obj
      class(fit) <- "lm"
      lm_df <- data.frame(Fitted.Values = fitted(fit),
                          Residuals =  resid(fit),
                          Standardized.Residuals = rstandard(fit),
                          Theoretical.Quantiles = qqnorm(resid(fit), plot.it = F)$x,
                          Root.Residuals = sqrt(abs(rstandard(fit))),
                          Leverage = lm.influence(fit)$hat)

      LOESS1 <- loess.smooth(lm_df$Fitted.Values, lm_df$Residuals)
      LOESS2 <- loess.smooth(lm_df$Fitted.Values, lm_df$Root.Residuals)
      LOESS3 <- loess.smooth(lm_df$Leverage, lm_df$Residuals)

      plot1 <- lm_df %>%
        plot_ly(x = Fitted.Values, y = Residuals,
                type = "scatter", mode = "markers", name = "Data",
                marker = list(size = 10, opacity = 0.8), showlegend = F) %>%
        add_trace(x = LOESS1$x, y = LOESS1$y, type = "scatter", mode = "line", name = "Smooth",
                  line = list(width = 1.5)) %>%
        layout(title = "Residuals vs Fitted Values", plot_bgcolor = "gray96")

      plot2 <- lm_df %>%
        plot_ly(x = Theoretical.Quantiles, y = Standardized.Residuals,
                type = "scatter", mode = "markers", name = "Data",
                marker = list(size = 10, opacity = 0.8), showlegend = F) %>%
        add_trace(x = Theoretical.Quantiles, y = Theoretical.Quantiles, type = "scatter", mode = "line", name = "",
                  line = list(width = 1.5)) %>%
        layout(title = "Q-Q Plot", plot_bgcolor = "gray96")

      plot3 <- lm_df %>%
        plot_ly(x = Fitted.Values, y = Root.Residuals,
                type = "scatter", mode = "markers", name = "Data",
                marker = list(size = 10, opacity = 0.8), showlegend = F) %>%
        add_trace(x = LOESS2$x, y = LOESS2$y, type = "scatter", mode = "line", name = "Smooth",
                  line = list(width = 1.5)) %>%
        layout(title = "Scale Location", plot_bgcolor = "gray96")

      plot4 <- lm_df %>%
        plot_ly(x = Leverage, y = Residuals,
                type = "scatter", mode = "markers", name = "Data",
                marker = list(size = 10, opacity = 0.8), showlegend = F) %>%
        add_trace(x = LOESS3$x, y = LOESS3$y, type = "scatter", mode = "line", name = "Smooth",
                  line = list(width = 2)) %>%
        layout(title = "Leverage vs Residuals", plot_bgcolor = "gray96")

      values$diagPlots <- list(plot1, plot2, plot3, plot4)

    })

    observeEvent(input$done, {
      stopApp()
    })

    }

  runGadget(ui, server, viewer = paneViewer())
}

#dialogViewer(dialogName = "linear regression model summary and performance", width = 1350, height = 1000)
