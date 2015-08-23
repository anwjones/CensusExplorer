library (ggplot2)
library(scales)
library(data.table)
library(shiny)

SA2 <- data.table(read.csv(file='SA2_DATA.csv'))

var.names <- read.csv('VAR_NAMES.csv', strip.white=TRUE, stringsAsFactors = FALSE)

#function to create the data.frame we want to plot
createPlotData <- function(X, Y){
    X.NAME <- var.names[var.names$DISPLAY==X,'NAME']
    Y.NAME <- var.names[var.names$DISPLAY==Y,'NAME']
    df <- SA2[,c('DEATHS','EXPDEATHS',X.NAME, Y.NAME), with=FALSE]
    setnames(df, 3, 'X') 
    setnames(df, 4, 'Y')
    
    if (!is.na(var.names[var.names$DISPLAY==X,'LO'])){
        df$X.group <- cut(df$X, quantile(df$X, probs=seq(0,1,0.1),na.rm=TRUE)
              ,labels=c('01','02','03','04','05','06','07','08','09','10'))
    } else {
        df$X.group <- df$X
    }    

    if (!is.na(var.names[var.names$DISPLAY==Y,'LO'])){
        df$Y.group <- cut(df$Y, quantile(df$Y, probs=seq(0,1,0.1),na.rm=TRUE)
                          ,labels=c('01','02','03','04','05','06','07','08','09','10'))
    } else {
        df$Y.group <- df$Y
    }
    df
}


shinyServer(function(input, output) {

    #create a data.table with the plotted variables
    df <- reactive({
        createPlotData(input$X.Axis, input$Y.Axis)    
        })
    
    #create a summarised data.table by the grouped variables
    df.1 <- reactive({ 
        df()[complete.cases(df())
             ,list(A=sum(DEATHS, na.rm=TRUE), E=sum(EXPDEATHS, na.rm=TRUE))
             ,by=list(X.group, Y.group)]
    })
    
    #create the scatter plot
    output$scatter <- renderPlot({

      scatter <- ggplot(data=df(), aes(x=X, y=Y, colour=DEATHS/EXPDEATHS)) +
          geom_point() + 
          scale_color_gradient2(low='darkgreen', mid='grey', space='Lab'
                                ,high='darkred', midpoint=1
                                ,limits=c(0,2), na.value='grey') +
          labs(x=input$X.Axis, y=input$Y.Axis)

      X.VARTYPE <- var.names[var.names$DISPLAY==input$X.Axis,'VARTYPE']
      Y.VARTYPE <- var.names[var.names$DISPLAY==input$Y.Axis,'VARTYPE']
      X.LO <- var.names[var.names$DISPLAY==input$X.Axis,'LO']
      Y.LO <- var.names[var.names$DISPLAY==input$Y.Axis,'LO']
      X.HI <- var.names[var.names$DISPLAY==input$X.Axis,'HI']
      Y.HI <- var.names[var.names$DISPLAY==input$Y.Axis,'HI']
      
      
      if (!is.na(X.LO)){
           scatter <- scatter + scale_x_continuous(limits=c(X.LO,X.HI))   
      } else {
          scatter <- scatter + scale_x_discrete()   
      }
      
      if (!is.na(Y.LO)){
          scatter <- scatter + scale_y_continuous(limits=c(Y.LO,Y.HI))   
      } else {
          scatter <- scatter + scale_y_discrete()   
      }
      
     scatter
     
    })
    
    #create the grouped plot
    output$group <- renderPlot({
     
        grouped <- ggplot(data=df.1(), aes(x=X.group, y=Y.group, colour=A/E, size=E)) +
            geom_point(shape=15) +
            scale_size_continuous(range=c(0,20), guide='none') +
            scale_colour_gradient2(low='green', mid='grey', space='Lab'
                                   ,high='red', midpoint=1
                                   ,limits=c(0.8,1.2), na.value='grey') +
            labs(x=paste0(input$X.Axis, ' grouped'), y= paste0(input$Y.Axis, ' grouped'))
        
        grouped
     
    })

})
