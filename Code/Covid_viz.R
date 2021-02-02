library(tidyverse)
library(gridExtra)
library(scales)
library(lubridate)
library(ggplot2)

## Preprocessing #### 
covid <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")
write_csv(covid,file="../Data/WHO-Covid-19-backup.csv")


covid <- covid %>% 
  filter(WHO_region != "Other") %>% 
  mutate(WHO_region = fct_recode(WHO_region,                            "Eastern Mediterranean"="EMRO","Europe" = "EURO","Africa" = "AFRO","Western Pacific" = "WPRO","Americas"="AMRO","South-East Asia" = "SEARO"))

range(covid$Date_reported)
length(unique(covid$Country))



## Scatterplot ####
fig.scatter.1 <- covid %>% 
  filter(Date_reported=="2021-01-28", WHO_region=="Africa") %>% 
  ggplot(aes(x=New_cases,y=New_deaths)) +
  geom_point()+
  geom_text(aes(label=Country),hjust=0, vjust=0)


fig.scatter.1


fig.scatter.2 <- covid %>% 
  filter(Date_reported=="2021-01-23", WHO_region=="Africa") %>% 
  ggplot(aes(x=Cumulative_cases,y=Cumulative_deaths)) +
  geom_point()+
  geom_text(aes(label=Country),hjust=0, vjust=0)

fig.scatter.2

gridExtra::grid.arrange(fig.scatter.1, fig.scatter.2, nrow=1, ncol=2)


## Maps #### 
library(maps)
world <- map_data("world");
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) 
worldplot

covid.today<- covid %>% 
  filter(Date_reported == "2021-1-28") %>% 
  mutate(region=Country)


covid.today.world<- inner_join(world, covid.today, by = "region")

fig.map  <- ggplot() +
  geom_polygon(data = covid.today.world, aes(x=long, y = lat, group = group,fill=New_deaths)) + 
  coord_fixed(1.3)
fig.map

# Q: Why is USA not shown in the map?
# Check out:  setdiff(unique(covid.today$Country),unique(world$region))!

## Spaghetti plot

fig.spaghetti.1 <- covid %>% 
  filter(Date_reported>= "2021-01-01", Date_reported<= "2021-01-28", WHO_region=="Africa") %>% 
  mutate(Date=as.Date(Date_reported)) %>%
  ggplot(aes(x=Date,y=New_cases,by=Country)) +
  geom_line(aes(color=Country)) +
  theme(legend.position ='none')
fig.spaghetti.1   

fig.spaghetti.2 <- covid %>% 
  filter(Date_reported>= "2021-01-01", Date_reported<= "2021-01-28", WHO_region=="Africa") %>% 
  mutate(Date=as.Date(Date_reported)) %>%
  ggplot(aes(x=Date,y=New_deaths,by=Country)) +
  geom_line(aes(color=Country)) +
  theme(legend.position ='none')
fig.spaghetti.2  

## Interactive plot ####
library(plotly)
covid %>% 
  filter(Date_reported>= "2021-01-01", Date_reported<= "2021-01-28") %>% 
  group_by(Date_reported,WHO_region) %>%   summarize(deaths = sum(New_deaths),
            cases = sum(New_cases)) %>% 
  mutate(Days_2021 = Date_reported- as.Date("2021-01-01")) %>%
  plot_ly(
    x= ~cases,
    y= ~deaths,
    frame = ~Days_2021,
    text=~WHO_region,
    hoverinfo="WHO_region",
    color=~WHO_region,
    type = 'scatter',
    mode = 'markers',
    showlegend = T
  )
