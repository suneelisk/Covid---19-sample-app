library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyanimate)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(coindeskr) #bitcoin price extraction from coindesk
library(ggthemes) 
library(ggpubr)
library(ggplot2)
library(plyr)
library(dplyr)
library(plotly)
library(DT)
library(rworldmap)
library(ggmap)
library(leaflet)
library(googleway)
library(XML)
library(RCurl)


#===========
## FUNCTIONS
#===========
## SIMPLE GREETING

register_google(key = "AIzaSyDUcfjDRiqexZjk3He8mHkBCFgllTu9Dis")


#========
# SERVER
#========


ui <- fluidPage(
  
  tags$style(HTML('body {font-family:"Verdana",Georgia,Serif; background-color:white}')),
  theme = shinytheme("united"),
  withAnim(),
  #setBackgroundImage(src = "w.jpg"),
  tags$head(
    tags$style(type = 'text/css', 
               HTML('
                              .navbar-default .navbar-brand{color: ;}
                              .tab-panel{ background-color: #; color: #}
                              .navbar-default .navbar-nav > .active > a, 
                              .navbar-default .navbar-nav > .active > a:focus, 
                              .navbar-default .navbar-nav > .active > a:hover {
                              color: #e6e6e6;
                              background-color: #;
                              
                              }')
    )
  ),
  
  tags$style(HTML(".navbar  {
                            background-color:#116bac; }
                            
                            .navbar .navbar-nav {float: right; margin-right: 35px;
                            margin-top: 26px;
                            color: #; 
                            font-size: 18px; 
                            background-color: #; }
                            
                            .navbar.navbar-default.navbar-static-top{ 
                            color: #; 
                            font-size: 23px; 
                            background-color: # ;}
                            
                            .navbar .navbar-header {
                            float: left;
                            background-color: # ;}
                            
                            .navbar-default .navbar-brand { color: #e6e6e6; 
                            margin-top: 10px;
                            font-size: 24px; 
                            background-color: # ;} 
                            
                            ")),
  tags$style(type="text/css",
             "#well0{
                       padding: 100px;
                       background: white;
                       border: 1px;
                       box-shadow:2px 2px;}"),
  tags$style(type="text/css",
             "#well2{
                       padding: 100px;
                       background: #;
                       border: 1px;
                       box-shadow:2px 2px;}"),
  tags$style(type="text/css",
             "#well8{
                       padding: 100px;
                       background: #;
                       border: 1px;
                       box-shadow: 2px 2px;}"),
  tags$style(type="text/css",
             "#rrr{
                       padding: 100px;
                       background: #;
                       border: 0px;
                       box-shadow: 0px 0px;}"),
  tags$head(
    tags$style(HTML("
                              input[type=\"number\"] {
                              font-size: 20px;height:50px;
                              }
                              
                              "))
  ),
  
  tags$head(HTML("<title>Covid-19</title> <link rel='icon' type='image/gif/png' href='icon.png'>")),
  

  navbarPage(id="tabset",tags$li(class = "dropdown",
                                 tags$style(".navbar {min-height:100px }")
  ),
  #title = ,position = "fixed-top",selected = "Upload",inverse = TRUE,
  title = tags$div(img(src="icon.png","COVID-19 REPORT", style="margin-top: -4px;margin-left: 30px;", height = 60)),position = "fixed-top",selected = "World",inverse = F,
  
  tabPanel(title = "World",icon = icon("table"),
           
           fluidPage(
             
             tags$style(" #modal1 .modal-header {background-color:#; border-top-left-radius: 0px; border-top-right-radius: 0px}"),
             
             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
             ),
             tags$head(tags$style("#pppp{color:black; font-size:35px; font-style:italic; text-align=center;
                                  overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
             tags$head(tags$style("#roi{color:black; font-size:35px; font-style:italic; text-align=center;
                                  overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
             
             br(),
             br(),
             br(),
             br(),
             br(),
             downloadButton('download2'),br(),br(),
             
               fluidPage(
                 fluidRow(
                   column(6,wellPanel(tags$div(h4(strong(htmlOutput("confirmedtxt")),style="color:orange;font-weight:100%;"),align="center", style = 'color:green'),
                                      tags$div(verbatimTextOutput("confirmed"), style ="font-size:115%;color:#024b74;text-align:center;",align="center"))),
                   column(6,wellPanel(tags$div(h4(strong(htmlOutput("deathstxt")),style="color:red;font-weight:100%;"),align="center", style = 'color:red'),
                                      tags$div(verbatimTextOutput("deaths"), style ="font-size:115%;color:#024b74;text-align:center;",align="center")))
                 )
               ),
            
             
             addSpinner(plotlyOutput('plot3'),spin = 'bounce', color = "#E41A1C")

             
             
             )),
  
  tabPanel(title = strong("|")),
  
  
  tabPanel(title = "Country",
           br(),
           br(),
           br(),
           br(),
           br(),
           
           uiOutput('select'),
           downloadButton('download1'),br(),br(),
           
           fluidPage(
             fluidRow(
               column(6,wellPanel(tags$div(h4(strong(htmlOutput("confirmedtxt1")),style="color:orange;font-weight:100%;"),align="center", style = 'color:green'),
                                  tags$div(verbatimTextOutput("confirmed1"), style ="font-size:115%;color:#024b74;text-align:center;",align="center"))),
               column(6,wellPanel(tags$div(h4(strong(htmlOutput("deathstxt1")),style="color:red;font-weight:100%;"),align="center", style = 'color:red'),
                                  tags$div(verbatimTextOutput("deaths1"), style ="font-size:115%;color:#024b74;text-align:center;",align="center")))
             )
           ),
           
           tags$div(h4(strong(htmlOutput("countryname")),style="color:green;font-weight:100%;"),align="center", style = 'color:green'),
           
           
           
           addSpinner(plotlyOutput('plot1'),spin = 'bounce', color = "#E41A1C"),br(),br(),
           addSpinner(plotOutput('plot2'),spin = 'bounce', color = "#E41A1C")
           
           ),
  
  tabPanel(title = strong("|")),
  
  
  tabPanel(title = "India States",
           br(),
           br(),
           br(),
           br(),
           br(),
           
           
           downloadButton('download3'),
           uiOutput('select2'),br(),
           
           tags$div(h4(strong(htmlOutput("statename")),style="color:green;font-weight:100%;"),align="center", style = 'color:green'),
           
           
           fluidPage(
             fluidRow(
               column(4,wellPanel(tags$div(h4(strong(htmlOutput("confirmedtxt2")),style="color:orange;font-weight:100%;"),align="center", style = 'color:green'),
                                  tags$div(verbatimTextOutput("confirmed2"), style ="font-size:115%;color:#024b74;text-align:center;",align="center"))),
               column(4,wellPanel(tags$div(h4(strong(htmlOutput("dischargedtxt")),style="color:green;font-weight:100%;"),align="center", style = 'color:green'),
                                  tags$div(verbatimTextOutput("discharged"), style ="font-size:115%;color:#024b74;text-align:center;",align="center"))),
               column(4,wellPanel(tags$div(h4(strong(htmlOutput("deathstxt2")),style="color:red;font-weight:100%;"),align="center", style = 'color:red'),
                                  tags$div(verbatimTextOutput("deaths2"), style ="font-size:115%;color:#024b74;text-align:center;",align="center")))
             )
           ),
           

           
           
           addSpinner(plotlyOutput('plot4'),spin = 'bounce', color = "#E41A1C")

  )
  
  
  
  
  # tabPanel(title = 'Google Map View',
  #   fluidPage(
  #     fluidRow(
  #       column(6,
  #              selectInput('select2', 'Map type', choices = c("terrain",
  #                                                             "terrain-background", "satellite", "roadmap", "hybrid", "toner",
  #                                                             "watercolor", "terrain-labels", "terrain-lines", "toner-2010",
  #                                                             "toner-2011", "toner-background", "toner-hybrid", "toner-labels",
  #                                                             "toner-lines", "toner-lite"), selected = "satellite")),
  #       column(6,
  #              selectInput('select3', 'Map Size', choices = c(1,2,3,4,5), selected = 5))
  #     ),
  #     wellPanel(
  #     addSpinner(plotOutput('googleplot'),spin = 'bounce', color = "#E41A1C")
  #     )
  #   )
  # )
  
  
)
)

server <- function(input, output, session) {
  
  data = read.csv('https://opendata.ecdc.europa.eu/covid19/casedistribution/csv')

  output[['select']] = renderUI({
    selectInput("select1", "select country", selected = "India", choices = c(as.character(unique(data$countriesAndTerritories))))
  })
  
  ############################  filtered data  ##############

  data1 = reactive({
    data2 = data[data$countriesAndTerritories == input$select1,]
    data2
  })
  
  ######################  COuntry wise sum data  33333##############
  
  data2 = reactive({
    data3 = aggregate(data$cases, by=list(Category=data$countriesAndTerritories), FUN=sum)
    names(data3) = c('Country', 'Cases')
    data3
  })
  
  data3 = reactive({
    url = 'https://www.businessinsider.in/india/news/data-of-coronavirus-patients-state-wise-in-india/articleshow/74669748.cms'
    url1 = getURL(url = url)
    
    table = readHTMLTable(url1, which=2, header = FALSE)
    table = table[-1,-1]
    
    names(table) = c('State', 'Cases', 'Cured', 'Death')
    table
  })

    output$download1 <- downloadHandler(
      filename = function() {
        paste("dataset-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(data1(), file)
      })
    
    output$download2 <- downloadHandler(
      filename = function() {
        paste("dataset-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(data2(), file)
      })
    
    output$download3 <- downloadHandler(
      filename = function() {
        paste("dataset-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(data3(), file)
      })
    
    

    output[['plot1']] = renderPlotly({
      data1 = data1()
      p = ggplot(data1, aes(y=cases, x=dateRep, fill = month)) +
        geom_bar(position="dodge", stat="identity")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      p = ggplotly(p)
      p
    })

    output[['plot2']] = renderPlot({
      data1 = data1()
      p1 = data1 %>%
        ggplot(aes(x = day, y = cases)) +
        geom_point(color = "darkorchid4") +
        facet_wrap(countriesAndTerritories~ month, ncol = 2) +
        labs(
          # subtitle = "Data plotted by year",
          y = "Corana Cases",
          x = "Day") + theme_bw(base_size = 15)+
        theme(panel.background = element_rect(fill = "lightblue",
                                              colour = "lightblue",
                                              size = 0.5, linetype = "solid"),
              panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                              colour = "white"),
              panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                              colour = "white"))

      # p1 = ggplotly(p1)
      p1

    })
    
    output[['plot3']] = renderPlotly({
      
      data3 = data2()
      
      
      p = ggplot(data=data3, aes(x=Country, y=Cases, group=1)) +
        geom_line(color="red")+
        geom_point()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      p = ggplotly(p)
      p
      
    })
    
    output[['confirmedtxt']] = renderText({
      paste0('Confirmed')
    })
    
    output[['confirmed']] = renderPrint({
      ss = sum(data$cases)[1]
      ss
    })
    
    output[['deathstxt']] = renderText({
      paste0('Deaths')
    })
    
    output[['deaths']] = renderPrint({
      ss = sum(data$deaths)[1]
      ss
    })
    
    output[['confirmedtxt1']] = renderText({
      paste0('Confirmed')
    })
    
    output[['confirmed1']] = renderPrint({
      ss = sum(data1()$cases)[1]
      ss
    })
    
    output[['deathstxt1']] = renderText({
      paste0('Deaths')
    })
    
    output[['deaths1']] = renderPrint({
      ss = sum(data1()$deaths)[1]
      ss
    })
    
    output[['countryname']] = renderText({
      paste0(unique(data1()$countriesAndTerritories))
    })
    
    
    
    ############################################  State wise for india  ##########################
    
    output[['plot4']] = renderPlotly({
      fig <- plot_ly(data3(), x = ~State, y = ~Cases, type = 'bar', name = 'Possitive Cases')
      fig <- fig %>% add_trace(y = ~Cured, name = 'Cured & Discharged')
      fig <- fig %>% add_trace(y = ~Death, name = 'Death')
      fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')

      fig
    })
    
    output[['select2']] = renderUI({
      selectInput("select3", "select State", selected = "Andhra Pradesh", choices = c(as.character(unique(data3()$State))))
    })
    
    
    data4 = reactive({
      data = data3()
      data$Cases = as.numeric(as.character(data$Cases))
      data$Cured = as.numeric(as.character(data$Cured))
      data$Death = as.numeric(as.character(data$Death))
      data2 = data[data$State == input$select3,]
      data2
    })
    
    
    
    output[['statename']] = renderText({
      paste0(input$select3)
    })
    
    
    output[['confirmedtxt2']] = renderText({
      paste0('No of Possitive Cases')
    })
    
    output[['confirmed2']] = renderPrint({
      ss = data4()$Cases[1]
      ss
    })
    
    output[['dischargedtxt']] = renderText({
      paste0('Discharged')
    })
    
    output[['discharged']] = renderPrint({
      ss = data4()$Cured[1]
      ss
    })
    
    output[['deathstxt2']] = renderText({
      paste0('No of Deaths')
    })
    
    output[['deaths2']] = renderPrint({
      ss = data4()$Death[1]
      ss
    })
    
    
  

}

shinyApp(ui, server)
