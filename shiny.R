library(shiny)
library(jsonlite)

setwd("C:/공공데이터")
knitr::opts_chunk$set(echo = TRUE)
data_path <- "C:/공공데이터/fire.json"
dat <- fromJSON(data_path)

fire <- dat$records 

d1 <- as.POSIXct("2023-04-01 10:10:30")
d2 <- as.POSIXct("2023-04-01 11:20:50")

fire$startdate <- as.POSIXct(paste(fire$startyear, fire$startmonth, fire$startday, fire$starttime), format="%Y %m %d %H:%M:%S")
fire$enddate <- as.POSIXct(paste(fire$endyear, fire$endmonth, fire$endday, fire$endtime), format="%Y %m %d %H:%M:%S")

fire$dur <- as.numeric(difftime(fire$enddate, fire$startdate, units="hours")) 

ui <- fluidPage(
  
  titlePanel("과제4 산불현황"),
  sidebarLayout(
    sidebarPanel(
      selectInput("data", "구분",
                  c("광역시도별" = "locsi",
                    "화재원인별" = "firecause",
                    "요일별" = "startdayofweek",
                    "월별" = "startmonth"),
      ),
    ),
    mainPanel(
      tableOutput("outputTable"),
      plotOutput("plot"),
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    if(input$data == "locsi" ){
      do <- table(fire[input$data])
      barplot(do)
    }else if(input$data == "firecause"){
      do <- table(fire[input$data])
      barplot(do)
    }else if(input$data == "startdayofweek"){
      do <- table(fire[input$data])[c(5,4,7,3,2,1,6)]
      barplot(do)
    }else if(input$data == "startmonth"){
      do <- table(fire[input$data])
      barplot(do)
    }
  })
  output$outputTable <- renderTable({
    if (input$data == "locsi") {
      table_data <- table(fire[input$data])
      table_data <- as.data.frame(table_data)
      colnames(table_data) <- c("광역시도", "화재건수")
      table_data
    } else if (input$data == "firecause") {
      table_data <- table(fire[input$data])
      table_data <- as.data.frame(table_data)
      colnames(table_data) <- c("화재원인", "화재건수")
      table_data
    } else if (input$data == "startdayofweek") {
      table_data <- table(fire[input$data])[c(5, 4, 7, 3, 2, 1, 6)]
      table_data <- as.data.frame(table_data)
      colnames(table_data) <- c("요일별", "화재건수")
      table_data
    } else if (input$data == "startmonth") {
      table_data <- table(fire[input$data])
      table_data <- as.data.frame(table_data)
      colnames(table_data) <- c("월별", "화재건수")
      table_data
    } 
  })
  
}

library(rsconnect)
shinyApp(ui = ui, server = server)



