data = read.csv('https://opendata.ecdc.europa.eu/covid19/casedistribution/csv')
head(data)
library(plyr)
library(dplyr)
data1 = data%>%dplyr::filter(countriesAndTerritories == "Afghanistan")
data2 = data[data$countriesAndTerritories == "Afghanistan",]
head(data2)

library(ggplot2)
library(plotly)

data = read.csv('https://opendata.ecdc.europa.eu/covid19/casedistribution/csv')


data1 = data[data$countriesAndTerritories == "India",]
data1

sum(data1$cases)


p = ggplot(data3, aes(y=x, x=Category)) + 
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p = ggplotly(p)
p

data3 = aggregate(data$cases, by=list(Category=data$countriesAndTerritories), FUN=sum)


p1 = ggplot(data=data3, aes(x=Category, y=x, group=1)) +
  geom_line(color="red")+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p1

data2 %>%
      ggplot(aes(x = day, y = cases)) +
      geom_point(color = "darkorchid4") +
      facet_wrap( countriesAndTerritories~ month, ncol = 2) +
      labs(
        # subtitle = "Data plotted by year",
        y = "Mainboard Damage",
        x = "Day") + theme_bw(base_size = 15)+
      theme(panel.background = element_rect(fill = "lightblue",
                                            colour = "lightblue",
                                            size = 0.5, linetype = "solid"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "white"),
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "white"))

p1 = ggplotly(p1)
p1

data3$Country = as.character(data3$Country)


locations_df <- mutate_geocode(bfd, Place)

bfd = data.frame(Place = c("chenni","nellore","hyderabad"))
bfd$Place = as.character(bfd$Place)







output$googleplot = renderPlot({
  data3 = data[,c("countriesAndTerritories","cases")]
  data3 = aggregate(data3$cases, by=list(Category=data3$countriesAndTerritories), FUN=sum)
  names(data3) = c("Country", "Cases")
  lat1 = geocode(as.character(data3$Country))
  data3 = data.frame(data3, lat1)
  lat1 = data.frame(lon = 80.237617, lat = 13.067439)
  lat2 = data.frame(Country = "Chennai", Cases = 656)
  data3 = data.frame(lat1,lat2)
  msm=input$values
  map=get_map(location= "india", zoom= 3, maptype = "satellite", source = "google" )
  mapPoints <- ggmap(map) +
    geom_point(data = data3, alpha = .5, color = "red", size=3)+
    geom_text(data = data3, aes(label=paste(data3$Country,"-", data3$Cases)),hjust=0, vjust=0, color= "DarkOrange", size= 6)
  mapPoints
  
})


library(XML)
library(RCurl)
library(plotly)

url = 'https://www.businessinsider.in/india/news/data-of-coronavirus-patients-state-wise-in-india/articleshow/74669748.cms'
url1 = getURL(url = url)

table = readHTMLTable(url1, which=2, header = FALSE)
table = table[-1,-1]

names(table) = c('State', 'Cases', 'Cured', 'Death')

head(table)

fig <- plot_ly(table, x = ~State, y = ~Cases, type = 'bar', name = 'Possitive Cases')
fig <- fig %>% add_trace(y = ~Cured, name = 'Cured & Discharged')
fig <- fig %>% add_trace(y = ~Death, name = 'Death')
fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')

fig

str(table)
table$Cases = as.numeric(as.character(table$Cases))
table1 = table[table$State == 'Andhra Pradesh',]
table1$Cases[1]


