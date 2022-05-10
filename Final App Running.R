###########################################################

library(shiny)
library(fpp3)
library(ggplot2)
library(rsconnect)
deployApp()
ui <- fluidPage(
 h2("HOTELS DATA PLOTS",style="color:blue"),
  h3("Instructions:"),
 p("To use this app, please select the type of plot you would like to visualize."),
 p("The first 3 plots will show you different plots of the original data set. The"),
 p("other plots forecast the data. When you select a plot, below the plot there"),
 p("will be an interpretation of the selected plot. Enjoy!"),
radioButtons(
  inputId = "selected_plot",
      label = "Select a plot to view",
      choices = c( "Seasonal Plot", "Autocorrelation Plot", "Decomposition Plot",
        "Naive", "Seasonal Naive", "Mean",
        "Drift", "Holts", "Holts Winters", "ARIMA Manual", "ARIMA Selected")
),
  checkboxInput(inputId = 'print_plot',
                label = 'Print plot?'
                ),
plotOutput("selected_plot"),
textOutput("text")
)
server <- function(input, output) {
  output$selected_plot <-renderPlot(
    if ( "Seasonal Plot"== input$selected_plot){TShotels%>% gg_season()}
    else if("Autocorrelation Plot" == input$selected_plot){TShotels %>% ACF(revenue) %>% autoplot()}
    else if ( "Decomposition Plot" == input$selected_plot){TShotels %>%
        model(classical_decomposition(revenue)) %>% components() %>% autoplot()}
    else if( "Naive" == input$selected_plot){Naive<- TShotels %>% model(NAIVE(revenue))
    Naive%>% forecast %>% autoplot(TShotels)}
    else if ( "Seasonal Naive" == input$selected_plot){SNaive<- TShotels %>% model(SNAIVE(revenue))
    SNaive%>% forecast %>% autoplot(TShotels)}
    else if( "Mean" == input$selected_plot){Meanm<- TShotels %>% model(MEAN(revenue))
    Meanm%>% forecast %>% autoplot(TShotels)}
    else if ( "Drift" == input$selected_plot){Drift<- TShotels %>% model(RW(revenue ~ drift()))
    Drift%>% forecast %>% autoplot(TShotels)}
    else if ( "Holts" == input$selected_plot){Hfit<-TShotels %>%
      model(`Holt's method` = ETS(revenue ~ error("A") +
                                    trend("A") + season("N")))
    Hfit%>% forecast %>% autoplot(TShotels)}
    else if ( "Holts Winters" == input$selected_plot){HW<-TShotels %>%
      model(
        additive = ETS(revenue ~ error("A") + trend("A") +
                         season("A")),
        multiplicative = ETS(revenue ~ error("M") + trend("A") +
                               season("M"))
      )
    HW%>% forecast %>% autoplot(TShotels)}
    else if ( "ARIMA Manual" == input$selected_plot){Arimafit<-TShotels %>% model(ARIMA(revenue~pdq(2,1,0)))
    Arimafit %>% forecast %>% autoplot(TShotels)}
    else if( "ARIMA Selected" == input$selected_plot){ArimaSfit<-TShotels %>% model(ARIMA(revenue))
    
    ArimaSfit %>% forecast %>% autoplot(TShotels)}
  )
  output$text<- renderText(
    if ( "Seasonal Plot"== input$selected_plot){paste("Seasonality: When we look at the seasonality plot, we see that there are clear dips and peaks. In the Month of Parch we have our larger peaks and in the summer month we see our larger dips along with December. Overall this plot shows that we do see seasonality in our data.")}
    else if("Autocorrelation Plot" == input$selected_plot){paste("Autocorrelation: Our autocorrelation plot shows both trend and seasonality. The decrease in ACF as lag increases shows the trend and the scalloped shapes at the top indicating the seasonality if the data.")}
    else if ( "Decomposition Plot" == input$selected_plot){paste("Decomposition: Our classical decomposition models show our upward trending data that is greatly seasonal.")}
    else if( "Naive" == input$selected_plot){paste("Naive: the classic naive model does not seem to predict very well as our 95% confidence interval is quite wide. This is not our best forecasting model.")}
    else if ( "Seasonal Naive" == input$selected_plot){paste("Seasonal Naive: Our Seasonal Naive model predicts much better than the classic. This shows a less wide confidence interval with a seasonality that we obviously have in our data.")}
    else if( "Mean" == input$selected_plot){paste("Mean: Our mean model is not reasonable for our data. We have upward trending data that is not being taken into consideration. ")}
    else if ( "Drift" == input$selected_plot){paste("Drift: Our drift model does seem to recognize the upward trend in in data, but it doesn't help forecast considering seasonality.")}
    else if ( "Holts" == input$selected_plot){paste("Holts: Our holts model is accurate to give us an upward trending forecast, but it disregards our seasonality, but our confidence intervals help us cover the seasonality.")}
    else if ( "Holts Winters" == input$selected_plot){paste("Holts Winters: Our Holts Winters model does a great job of showing both our upward trending data as well as seasonality in this forecast.")}
    else if ( "ARIMA Manual" == input$selected_plot){paste("ARIMA Manual: This is a great model that continues seasonality and the upward trend in the forecast with a realatively small confidence interval.")}
    else if( "ARIMA Selected" == input$selected_plot){paste("ARIMA Selected: This model has a wider confidence interval than our ARIMA Manual, but this model does predict very well and takes into consideration our seasonality and upward trending data.")}
  )

  }

shinyApp(ui, server)

