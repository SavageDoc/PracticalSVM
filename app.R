#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## Library load ----
# shiny for the app and infrastructure
library( shiny )
# e1071 for SVM functions
library( e1071 )
# ggplot2 for diagnostic plot
library( ggplot2 )

## Generate regression data ----
x <- seq( from=-1
          , to=1
          , by=0.011 # Miss 0
          )
y <- sin( 2*pi*x )/( 2*pi*x )



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("SVM Demo"),
   
   # Sidebar for inputs
   sidebarLayout(
      sidebarPanel(
      radioButtons( 'regType'
                      , 'Regression type:'
                      , choices=c( 'Epsilon'='eps-regression'
                                  , 'Nu'='nu-regression' ) 
                      , selected='eps-regression' )
      , numericInput( 'regEps', 'Epsilon:', value=0.1, min=0, max=1, step=0.1 )
      , conditionalPanel( condition='input.regType == "nu-regression"'
                          , sliderInput("regNu",
                                        "Nu:",
                                        min = 0,
                                        max = 1,
                                        value = 0.05,
                                        step=0.01
                          )
      )
      , numericInput( 'regCost', 'Cost', value=1, min=1, max=100 )
      )
      
      # Plots in mainPanel
      , mainPanel(
         plotOutput("regPlot")
         , plotOutput( "predPlot" )
      )
   )
)

# Define server logic required to graph the results
server <- function(input, output, session) {

  # Reactive to compute the SVM object   
   fitSVM <- reactive({
     mySVM <- svm( x
                   , y
                   , scale=FALSE
                   , tolerance=1e-6
                   , type=input$regType
                   , epsilon=input$regEps
                   , nu=input$regNu
                   , cost=input$regCost )
     return( mySVM )
   })
   
   # Output plots
   output$regPlot <- renderPlot({
     # Fit the SVM
     mySVM <- fitSVM()
     
     # Get predictions
     predY <- fitted( mySVM )
     
     # Build a data frame for plotting
     tempDF <- data.frame( x=x, y=y, predY=predY, SV=FALSE )
     # Flag the support vectors
     tempDF[mySVM$index, 'SV'] <- TRUE
     tempDF <- tempDF %>%
       # Add some derived fields
       mutate( withinEps=if_else( abs( predY-y ) <= input$regEps, TRUE, FALSE )
               , minEps=y-input$regEps
               , maxEps=y+input$regEps )
     
     # Build the plot...
     regPlot <- ggplot( tempDF ) +
       geom_line( aes( x=x, y=y, colour='Raw Data' ), size=2 ) +
       geom_point( aes( x=x, y=predY, shape=withinEps, colour='Predicted', size=SV ) ) + 
       geom_line( aes( x=x, y=predY, colour='Predicted' ), size=2 ) + 
       geom_ribbon( data=tempDF, aes( x=x
                                      , ymin=minEps
                                      , ymax=maxEps
                                      , colour='Predicted'
                                      , fill='Predicted' )
                    , alpha=0.2
       ) +
       scale_shape_manual( values=c("TRUE"=1, "FALSE"=18 ) ) +
       scale_colour_manual( values=c("Raw Data"='black', "Predicted"='goldenrod')) +
       scale_size_manual( values=c("TRUE"=8, "FALSE"=0 ) ) +
       theme( legend.position='bottom' ) +
       labs( title='Results from Support Vector Regression'
             , subtitle=paste( 'Using', mySVM$tot.nSV, 'Support Vectors' ) 
       ) +
       theme( legend.position='bottom' )
     
     regPlot
   })
   
   output$predPlot <- renderPlot({
     mySVM <- fitSVM()
     predY <- fitted( mySVM )
     
     tempDF <- data.frame( x=x, y=y, predY=predY, SV=FALSE )
     tempDF[mySVM$index, 'SV'] <- TRUE
     tempDF <- tempDF %>%
       mutate( withinEps=if_else( abs( predY-y ) <= input$regEps, TRUE, FALSE )
               , minEps=y-input$regEps
               , maxEps=y+input$regEps )
     
     predPlot <- ggplot( data=tempDF ) +
       geom_point( aes( x=y, y=predY, shape=withinEps, colour=withinEps, size=SV ) ) + 
       geom_abline( slope=1, intercept=0, linetype=3, size=2, colour='black' ) +
       geom_ribbon( aes( x=y, ymin=minEps, ymax=maxEps ), alpha=0.2, colour='red', fill='red' ) +
       scale_shape_manual( values=c("TRUE"=1, "FALSE"=18 ) ) +
       scale_colour_manual( values=c("TRUE"='darkcyan', "FALSE"='orange')) +
       scale_size_manual( values=c("TRUE"=8, "FALSE"=2 ) ) +
       theme( legend.position='bottom' )
     
     predPlot
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

