library(tidyverse)
covid <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")
oilpriceG <- read_csv("C:\\Users\\Jasmine\\Desktop\\Test STA141 Project\\CrudeOilPrice.csv")
populationG <- read_csv("C:\\Users\\Jasmine\\Desktop\\STA141FinalProject\\Data\\Matched Data\\OECD Populations matched.csv")
covidpop <- read_csv("C:\\Users\\Jasmine\\Desktop\\STA141FinalProject\\Data\\Matched Data\\CovidperCapita.csv")


oilprice <- unlist(oilpriceG[2])
pricedate <- unlist(oilpriceG[1])


#turning dates into date format
strDates <- c(pricedate)
Date_reported <- as.Date(strDates, "%m/%d/%Y")

#remaking Oilprice chart with date format
oildf <- data.frame(Date_reported, oilprice)

#Merging Oilprice with Covid 
#(okay this was dumb because I need world cumalative cases to compare)
total <- merge(covid,oildf,by="Date_reported")


#getting global new cases perday + global cumalative cases
sumCasesDay <-aggregate(New_cases ~ Date_reported, data = covid, sum)
sumTotalCases <- aggregate(Cumulative_cases ~ Date_reported, data = covid, sum)


#merging new global cases perday with oil price
global1 <- merge(sumCasesDay, oildf, by="Date_reported")

#merge in global cumulative cases
global2 <- merge(sumTotalCases, global1, by="Date_reported")

#write.csv(global2,"C:\\Users\\Jasmine\\Desktop\\Test STA141 Project\\DailyCrudevsGlobal.csv", row.names = FALSE)


#Weighting for population
CovidWeighted <- covid

CovidWeighted2 <- merge(CovidWeighted, populationG, by = "Country")

#just checking to make sure i didnt lose any countries
count(populationG, "Country")
CovidCount <- CovidWeighted2 %>% filter( (Date_reported == "2020-01-03"))
count(CovidCount, "Country")


#write.csv(CovidWeighted2,"C:\\Users\\Jasmine\\Desktop\\Test STA141 Project\\CovidwithPopu.csv", row.names = FALSE)


#getting monthly/quarterly Rona data


#was trying to convert date back to the covid dataset format but failed :*(
#pricedate <- unlist(covidpop[2])
#strDates <- c(pricedate)
#Date_reported <- as.Date(strDates, "%m/%d/%Y")

covidpop['Date_reported'] <- as.Date(covidpop['Date_reported'], format='%y/%m/%d ')

CovidQtr <- covidpop %>% filter( (Date_reported == "1/3/2020" | Date_reported == "4/3/2020" | Date_reported == "7/3/2020" | Date_reported == "10/3/2020" ))

CovidMonth <- covidpop %>% filter( (Date_reported == "1/28/2020" | Date_reported == "1/28/2020" | Date_reported == "2/28/2020" | Date_reported == "3/28/2020" | Date_reported == "4/28/2020" | Date_reported == "5/28/2020" | Date_reported == "6/28/2020" | Date_reported == "7/28/2020" | Date_reported == "8/28/2020" | Date_reported == "9/28/2020" | Date_reported == "10/28/2020" | Date_reported == "11/28/2020" | Date_reported == "12/28/2020" | Date_reported == "1/28/2021"))

write.csv(CovidQtr,"C:\\Users\\Jasmine\\Desktop\\Test STA141 Project\\CovidbyQuarter.csv", row.names = FALSE)
write.csv(CovidMonth,"C:\\Users\\Jasmine\\Desktop\\Test STA141 Project\\CovidbyMonth.csv", row.names = FALSE)

#CovidQtr <- covidpop %>% filter( (Date_reported == "2020-01-03" | Date_reported =="2020-04-03" | Date_reported =="2020-07-03" | Date_reported =="2020-10-03" | Date_reported =="2021-01-03"))
#CovidMonth <- covid%>% filter( (Date_reported == "2020-01-28" | Date_reported == "2020-02-28" | Date_reported == "2020-03-28" | Date_reported == "2020-04-28" | Date_reported == "2020-05-28" | Date_reported == "2020-06-28" | Date_reported == "2020-07-28" | Date_reported == "2020-08-28" | Date_reported == "2020-09-28" | Date_reported == "2020-10-28" | Date_reported == "2020-11-28" | Date_reported == "2020-12-28" | Date_reported == "2021-01-28" ))





#tried to do some regression here but its ugly as sin

#time series model

install.packages('forecast', dependencies = TRUE)
library('forecast')

#regoilcases <- ()


timeseries <- ts(data = global1, start = 1, end = 329, frequency = 1)

timeseries1 <- tslm(oilprice ~ New_cases, timeseries)



help(tslm)







