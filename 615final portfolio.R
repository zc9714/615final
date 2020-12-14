library(tidyquant)
library(dplyr)
library(tidyverse)
library(lubridate)
library(bizdays)
library(shiny)
library(shinythemes)
library(DT)

stock <- c("GOOG","AMZN","FB","NFLX","SPDN")
stockdata <-  stock%>% tq_get(get="stock.prices",from = "2020-07-01", to = "2020-12-01")
ui <- fluidPage(theme = shinytheme("readable"),
                titlePanel("Portfolio Manager"),
                br(),
                div(style="display:inline-block",selectInput("date1", "Start:",unique(stockdata$date))),
                div(style="display:inline-block",uiOutput("end")),
                helpText("weight must be a decimal from 0 to 1 with a max sum 1"),
                div(style="display:inline-block",numericInput("GOOG",
                             "GOOG WEIGHT:",
                             0.2,0,1,0.05)),
                div(style="display:inline-block",numericInput("AMZN",
                             "AMZN WEIGHT:",
                             0.2,0,1,0.05)),
                div(style="display:inline-block",numericInput("FB",
                             "FB WEIGHT:",
                             0.2,0,1,0.05)),
                div(style="display:inline-block",numericInput("NFLX",
                             "NFLX WEIGHT:",
                             0.2,0,1,0.05)),
                div(style="display:inline-block",numericInput("SPDN",
                             "SPDN WEIGHT:",
                             0.2,0,1,0.05)),
                
                
                br(),
                helpText("available advice:https://www.ullandinvestment.com/2020/06/"),
                
                mainPanel(
                    tabsetPanel(
                    tabPanel("Portfolio",
                             plotOutput("plot")),
                    
                    tabPanel("Data", 
                             selectInput("stocktype", "stockts:",list("Google"="GOOG","Amazon"="AMZN","Facebook"="FB","Netflix"="NFLX","SP500Bear"="SPDN")),
                             plotOutput("Data"),DT::dataTableOutput("table2"))
                )))


server <- function(input, output) {
    
    output$end <- renderUI({
        selectInput("date2", "End:",subset(unique(stockdata$date),unique(stockdata$date)>input$date1),selected = tail(stockdata$date, n=1)
        )
    })
    
    output$table2 <- DT::renderDataTable(DT::datatable({
        options("getSymbols.warning4.0"=FALSE)
        options("getSymbols.yahoo.warning"=FALSE)
        data <- data.frame(input$stocktype%>% tq_get(get="stock.prices",from = input$date1, to = input$date2))
        data[,-1]
        
    }))
    
    output$Data =renderPlot({
        options("getSymbols.warning4.0"=FALSE)
        options("getSymbols.yahoo.warning"=FALSE)
        a <- getSymbols(input$stocktype, from = input$date1,
                        to = input$date2,warnings = FALSE,
                        auto.assign = TRUE)
        if(input$stocktype=="GOOG"){chartSeries(GOOG, theme=chartTheme('white'))}
        if(input$stocktype=="AMZN"){chartSeries(AMZN, theme=chartTheme('white'))}
        if(input$stocktype=="FB"){chartSeries(FB, theme=chartTheme('white'))}
        if(input$stocktype=="NFLX"){chartSeries(NFLX, theme=chartTheme('white'))}
        if(input$stocktype=="SPDN"){chartSeries(SPDN, theme=chartTheme('white'))}
    })
    
    
    output$plot =renderPlot({

        
            w <-c(input$GOOG,input$AMZN,input$FB,input$NFLX,input$SPDN)
            if(sum(w)<=1){
                stock <- c("GOOG","AMZN","FB","NFLX","SPDN")
                stockdata <-  stock%>% tq_get(get="stock.prices",from = "2020-07-01", to = "2020-12-01")
                returnmoney <-stockdata %>%group_by(symbol)%>%tq_transmute(select= adjusted,mutate_fun = periodReturn,
                                                                           period = "monthly", col_rename = "money")
                portfolio <- tq_portfolio(returnmoney,assets_col = symbol, returns_col= money,  weights=w,col_rename   = "current_portfolio",wealth.index = T)
                ggplot(portfolio,aes(x=as.Date(date), y = current_portfolio*250000))+labs(x = "Date", y = "current portfolio")+geom_line( color="grey")+
                    geom_point(shape=21, color="black", fill="#69b3a2", size=6)+geom_text(aes(label = round(portfolio$current_portfolio*250000)),nudge_x=0.1,nudge_y=0.1)+
                    ggtitle("Portfolio tracking")
            }else{
                tag$h1("your sum of 5 stocks must be less than 1")
            }
        
    })
}


shinyApp(ui = ui, server = server)
