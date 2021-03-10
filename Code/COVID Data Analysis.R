setwd("../Data/Matched Data")
library(tidyverse)
library(lubridate)
library(plotly)
library(ggplot2)
library(ggpubr)
covid <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")
Country_GDP = read_csv("OECD Populations matched.csv")
countries = Country_GDP$Country


covid = covid %>% filter(Date_reported <= as.Date("2020-12-31")) %>%
  filter(Country %in% countries)

covid$Date_reported = quarter(covid$Date_reported, with_year = TRUE, fiscal_start = 1)

for(i in c(1: dim(covid)[1])){
  if(covid$Date_reported[i] == 2020.1)
    covid$Date_reported[i] = "1/3/2020"
  else if(covid$Date_reported[i] == 2020.2)
    covid$Date_reported[i] = "4/3/2020"
  else if(covid$Date_reported[i] == 2020.3)
    covid$Date_reported[i] = "7/3/2020"
  else if(covid$Date_reported[i] == 2020.4)
    covid$Date_reported[i] = "10/3/2020"
  
}


Ave_Summary = covid %>% group_by(Country, Date_reported) %>%
  summarize(Ave_NewCase = mean(New_cases),
            Ave_NewDeath = mean(New_deaths),
  ) 




Ave_Summary$CasePercent_Change = NA
Ave_Summary$DeathPercent_Change = NA

for(i in c(0:50)){
 for(j in c(1: 3)){
  Ave_Summary$CasePercent_Change[4*i+j+1] = abs(Ave_Summary$Ave_NewCase[4*i+j] - Ave_Summary$Ave_NewCase[4*i+j + 1])/Ave_Summary$Ave_NewCase[4*i+j]
  Ave_Summary$DeathPercent_Change[4*i+j+1] = abs(Ave_Summary$Ave_NewDeath[4*i+j] - Ave_Summary$Ave_NewDeath[4*i+j + 1])/Ave_Summary$Ave_NewDeath[4*i+j]
  }
}

figure1 = Ave_Summary %>%
  ggplot(aes(x=as.Date(Date_reported), y = Ave_NewDeath, by=Country)) +
  geom_line(aes(color=Country)) +
  theme(legend.position ='none')

figure2 = Ave_Summary %>%
  ggplot(aes(x=as.Date(Date_reported), y = Ave_NewCase, by=Country)) +
  geom_line(aes(color=Country)) +
  theme(legend.position ='none')

figure3 = Ave_Summary %>%
  ggplot(aes(x=as.Date(Date_reported), y = CasePercent_Change, by=Country)) +
  geom_line(aes(color=Country)) +
  theme(legend.position ='none')

figure4 = Ave_Summary %>%
  ggplot(aes(x=as.Date(Date_reported), y = DeathPercent_Change, by=Country)) +
  geom_line(aes(color=Country)) +
  theme(legend.position ='none')

ggarrange(figure1, figure2, figure3, figure4)


Ave_Summary %>% plot_ly(
  x= ~Ave_NewCase,
  y= ~Ave_NewDeath,
  frame = ~Date_reported,
  text=~Country,
  hoverinfo="Country",
  color=~Country,
  type = 'scatter',
  mode = 'markers',
  showlegend = T
) 


Cumul_Summary = covid %>% group_by(Country, Date_reported) %>%
  summarize(Cumul_Case = sum(New_cases),
            Cumul_Death = sum(New_deaths)
  )


Cumul_Summary$CasePercent_Change = NA
Cumul_Summary$DeathPercent_Change = NA

for(i in c(0:50)){
  for(j in c(1: 3)){
    Cumul_Summary$CasePercent_Change[4*i+j+1] = abs(Cumul_Summary$Cumul_Case[4*i+j] - Cumul_Summary$Cumul_Case[4*i+j + 1])/Cumul_Summary$Cumul_Case[4*i+j]
    Cumul_Summary$DeathPercent_Change[4*i+j+1] = abs(Cumul_Summary$Cumul_Death[4*i+j] - Cumul_Summary$Cumul_Death[4*i+j + 1])/Cumul_Summary$Cumul_Death[4*i+j]
  }
}

figure5 = Cumul_Summary %>%
  ggplot(aes(x=as.Date(Date_reported), y = Cumul_Case, by=Country)) +
  geom_line(aes(color=Country)) +
  theme(legend.position ='none')


figure6 = Cumul_Summary %>%
  ggplot(aes(x=as.Date(Date_reported), y = Cumul_Death, by=Country)) +
  geom_line(aes(color=Country)) +
  theme(legend.position ='none')


figure7 = Cumul_Summary %>%
  ggplot(aes(x=as.Date(Date_reported), y = CasePercent_Change, by=Country)) +
  geom_line(aes(color=Country)) +
  theme(legend.position ='none')


figure8 = Cumul_Summary %>%
  ggplot(aes(x=as.Date(Date_reported), y = DeathPercent_Change, by=Country)) +
  geom_line(aes(color=Country)) +
  theme(legend.position ='none')


ggarrange(figure5, figure6, figure7, figure8)


Cumul_Summary %>%
  plot_ly(
    x= ~Cumul_Case,
    y= ~Cumul_Death,
    frame = ~Date_reported,
    text=~Country,
    hoverinfo="Country",
    color=~Country,
    type = 'scatter',
    mode = 'markers',
    showlegend = T
  ) 


#Boxplots
figure9 = ggplot(Ave_Summary, aes(x = Date_reported, y = log(Ave_NewCase))) + geom_boxplot()
figure10 = ggplot(Ave_Summary, aes(x = Date_reported, y = log(Ave_NewDeath))) + geom_boxplot()
figure11 = ggplot(Cumul_Summary, aes(x = Date_reported, y = log(Cumul_Case))) + geom_boxplot()
figure12 = ggplot(Cumul_Summary, aes(x = Date_reported, y = log(Cumul_Death))) + geom_boxplot()

ggarrange(figure9, figure10, figure11, figure12)



covid_data = read.csv("../final_data_quarter_inner_join2.csv")
mergedSummary = merge(x = Ave_Summary, y = Cumul_Summary, by = c("Country", "Date_reported"))
finalMerge = merge(x = mergedSummary, y = covid_data, by = c("Country", "Date_reported"))

write.csv(finalMerge, '../final_data_quarter_inner_join3.csv')


data = read.csv("../final_data_quarter_inner_join3.csv")
gdpsummary = data %>% select(Country, Date_reported, GDP, X.GDP..QTR.)
gdpsummary$GDP = as.numeric(gsub(",","",gdpsummary$GDP))

figure13 = gdpsummary %>%
  ggplot(aes(x=as.Date(Date_reported), y = log(GDP), by=Country)) +
  geom_line(aes(color=Country)) +
  theme(legend.position ='none')

figure14 = gdpsummary %>%
  ggplot(aes(x=as.Date(Date_reported), y = X.GDP..QTR., by=Country)) +
  geom_line(aes(color=Country)) +
  theme(legend.position ='none')

ggarrange(figure13, figure14)


unempsummary = data %>% select(Country, Date_reported, Unemployment, X.unem..QTR.)


figure15 = unempsummary %>%
  ggplot(aes(x=as.Date(Date_reported), y = Unemployment, by=Country)) +
  geom_line(aes(color=Country)) +
  theme(legend.position ='none')

figure16 = unempsummary %>%
  ggplot(aes(x=as.Date(Date_reported), y = X.unem..QTR., by=Country)) +
  geom_line(aes(color=Country)) +
  theme(legend.position ='none')

ggarrange(figure15, figure16)

