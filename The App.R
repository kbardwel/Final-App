###########################################################

library(shiny)
library(fpp3)
library(plotly)

ui <- fluidPage(
radioButtons(
  inputId = "selected_plot",
      label = "Select a plot to view",
      choices = c( "Seasonal Plot", "Autocorrelation Plot", "Decomposition Plot",
        "Naive", "Seasonal Naive", "Mean",
        "Drift", "Holts", "Holts Winters", "ARIMA Manual", "ARIMA Selected"
),
  checkboxInput(inputId = 'print_plot',
                label = 'Print plot?'
                ),
plotOutput("plotted_series")
),
server <- function(input, output) {
  plots <-reactive({
    if ( "Seasonal Plot" %in% input$selected_plot) return(SeasonalPlot)
    if ( "NAutocorrelation Plot" %in% input$selected_plot) return(AutocorrelationPlot)
    if ( "Decomposition Plot" %in% input$selected_plot) return(Decomposition)
    if ( "Naive" %in% input$selected_plot) return(NPlot)
    if ( "Seasonal Naive" %in% input$selected_plot) return(SNPlot)
    if ( "Mean" %in% input$selected_plot) return(MeanPlot)
    if ( "Drift" %in% input$selected_plot) return(DriftPlot)
    if ( "Holts" %in% input$selected_plot) return(HPlot)
    if ( "Holts Winters" %in% input$selected_plot) return(HWPlot)
    if ( "ARIMA Manual" %in% input$selected_plot) return(ARIMAMPlot)
    if ( "ARIMA Selected" %in% input$selected_plot) return(ARIMASPlot)
    })
    
    output$plotted_series <- renderPlotl({   
      dataplots = plots()
      print(dataplots)
    }) 

  })

shinyApp(ui, server)


