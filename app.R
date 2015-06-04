# stormdash
## app.R ##
library(shinydashboard)
library(gridExtra)
library(shiny)
library(DT)
library(gdata)

ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "Areas of Interest"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Home", tabName = "home", icon = icon("home")),
                        menuItem("Public Awareness", tabName = "aware", icon = icon("bar-chart")),
                        menuItem("Physical Data", tabName = "datatab", icon = icon("database")),
                        menuItem("Inspections", tabName = "inspect", icon = icon("file-pdf-o"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        
                        # First tab content
                        tabItem(tabName = "home",
                                h2("Home tab content")
                        ),
                        
                        # Second tab content
                        tabItem(tabName = "aware",
                                fluidRow(
                                  box(
                                    title = "Amount Spent", width = 5, solidHeader = TRUE, status =
                                      "primary", collapsible = TRUE, plotOutput("plot2", height = 250
                                      )
                                  ),
                                  
                                  box(
                                    title = "Percent Community Reached", width = 5, solidHeader = TRUE, status =
                                      "primary", collapsible = TRUE, plotOutput("plot3", height = 250
                                      )
                                  )
                                ),
                                
                                fluidRow(
                                  box(
                                    title = "Histogram", status = "primary", solidHeader = TRUE,
                                    collapsible = TRUE,
                                    plotOutput("plot1", height = 250
                                    )
                                  ),
                                  
                                  box(
                                    title = "Inputs", background = "black", color = "black",
                                    solidHeader = TRUE,
                                    sliderInput("slider", "Number of observations:", 1, 100, 50)
                                  )
                                )
                        ),
                        
                        # Third tab content
                        tabItem(tabName = "datatab",
                                fluidPage(
                                  titlePanel("STORMWATER DATA"),
                                  
                                  # Create a new Row in the UI for selectInputs
                                  fluidRow(
                                    column(4, 
                                           selectInput("county", 
                                                       "County:", 
                                                       c("All", 
                                                         unique(as.character(data$County))))
                                    ),   
                                    column(4, 
                                           selectInput("station", 
                                                       "StationCode:", 
                                                       c("All", 
                                                         unique(as.character(data$StationCode))))
                                    ),
                                    column(4, 
                                           selectInput("analyte", 
                                                       "AnalyteName:", 
                                                       c("All", 
                                                         unique(as.character(data$AnalyteName))))
                                    )  
                                  ),
                                  # Create a new row for the table.
                                  fluidRow(
                                    DT::dataTableOutput(outputId="table")
                                  )    
                                )  
                        ),
                        
                        # Fourth tab content
                        tabItem(tabName = "inspect",
                                h2("Reports tab content")
                        )
                      )
                    )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  
  output$plot2 <- renderPlot({
    grid.draw(arrangeGrob(gg.gauge(10)))
  })
  
  output$plot3 <- renderPlot({
    grid.draw(arrangeGrob(gg.gauge(60)))
  })
  
  chem = read.xls("/Users/shellymoore3/Desktop/phydata.xls")
  # Filter data based on selections
  output$table <- DT::renderDataTable({
    data <- chem
    if (input$county != "All"){
      data <- data[data$County == input$county,]
    }
    if (input$station != "All"){
      data <- data[data$StationCode == input$station,]
    }
    if (input$analyte != "All"){
      data <- data[data$AnalyteName == input$analyte,]
    }
    data
  }, extensions = 'TableTools', options = list(dom = 'T<"clear">lfrtip',
                                               tableTools = list(sSwfPath = copySWF()), pageLength = 20)
  )
  
  gg.gauge <- function(pos,breaks=c(0,30,70,100)) {
    require(ggplot2)
    get.poly <- function(a,b,r1=0.5,r2=1.0) {
      th.start <- pi*(1-a/100)
      th.end   <- pi*(1-b/100)
      th       <- seq(th.start,th.end,length=100)
      x        <- c(r1*cos(th),rev(r2*cos(th)))
      y        <- c(r1*sin(th),rev(r2*sin(th)))
      return(data.frame(x,y))
    }
    ggplot()+ 
      geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="red")+
      geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="gold")+
      geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="forestgreen")+
      geom_polygon(data=get.poly(pos-1,pos+1,0.2),aes(x,y))+
      geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
                aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(breaks,"%")))+
      annotate("text",x=0,y=0,label=pos,vjust=0,size=8,fontface="bold")+
      coord_fixed()+
      theme_bw()+
      theme(axis.text=element_blank(),
            axis.title=element_blank(),
            axis.ticks=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank()) 
  }
}
shinyApp(ui, server)
