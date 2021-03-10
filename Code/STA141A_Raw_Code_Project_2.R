
library(ggplot2)

inner_data = read.csv("Data/final_data_quarter_inner_join3.csv")



#Final model to share with group
model11 = lm(inner_data2$`X.unem..TOTAL.` ~ inner_data2$New_cases_pc + inner_data2$Country, inner_data2)
model11$coefficients 
summary(model11)
confint(model11) 






inner_data$Date_reported = as.Date(inner_data$Date_reported, "%m/%d/%Y")
inner_data$Date_reported = format(inner_data$Date_reported, "%Y-%m-%d")
inner_data$GDP = as.numeric(gsub(",", ".", gsub("\\.", "", inner_data$GDP)))
inner_data[is.na(inner_data)] = 0
inner_data


#Graphical Section

ggplot(data = inner_data, mapping = aes(x = inner_data$LOCATION, y = inner_data$GDP, color = inner_data$Country)) + 
  geom_boxplot()

ggplot(data = inner_data, mapping = aes(x = inner_data$LOCATION, y = inner_data$Cumulative_cases_pc, color = inner_data$Country)) + 
  geom_boxplot()

ggplot(data= inner_data, mapping = aes(x = inner_data$Cumulative_cases_pc, y = inner_data$GDP, color = inner_data$LOCATION))+ 
  geom_point() 


#Model Testing Section 
inner_data2 = inner_data
#create a moving average based on who data for daily and GDP data to compare change over time.

head(inner_data2)
model1 = lm(inner_data2$GDP ~ inner_data2$New_cases_pc + inner_data2$Country, inner_data2)
model1$coefficients

model2 = lm(inner_data2$`%GDP (QTR)` ~ inner_data2$New_cases_pc + inner_data2$Country, inner_data2)
model2$coefficients


model3 = lm(inner_data2$`%GDP (QTR)` ~ inner_data2$Cumulative_deaths, inner_data2)
model3$coefficients 



model4 = lm(inner_data2$`%GDP (QTR)` ~ inner_data2$New_cases, inner_data2)
model4$coefficients 

model5 = lm(inner_data2$`%GDP (QTR)` ~ inner_data2$Cumulative_deaths_pc, inner_data2)
model5$coefficients 

model6 = lm(inner_data2$`%GDP (QTR)` ~ inner_data2$Cumulative_cases_pc, inner_data2)
model6$coefficients

model7 = lm(inner_data2$GDP ~ inner_data2$Cumulative_cases_pc, inner_data2)
model7$coefficients
inner_data2$GDP
inner_data2$Cumulative_cases_pc

model8 = lm(inner_data2$GDP ~ inner_data2$Cumulative_cases, inner_data2)
model8$coefficients

model9 = lm(inner_data2$`%unem (QTR)` ~ inner_data2$Cumulative_cases + inner_data$Country, inner_data2)

model9$coefficients

model10 = lm(dog2 ~ inner_data2$Cumulative_cases_pc, inner_data2)  
model10$coefficients 



#Log Transformations and reformatting

dog2 = log(inner_data2$`%unem (QTR)`)
is.na(dog2) = sapply(dog2, is.infinite)


model12 = lm(inner_data2$`%GDP (QTR)` ~ dog1, inner_data2)
summary(model12)


dog1 = log(inner_data2$New_cases_pc)
is.na(dog1) = sapply(dog1, is.infinite)
dog1 

inner_data2$New_cases_pc

hist(inner_data2$New_cases_pc)
hist(inner_data2$`%unem (QTR)`)
 
inner_data2$GDP = log(inner_data2$GDP)



hist(inner_data2$Cumulative_cases_pc)
inner_data2$Cumulative_cases_pc = log(inner_data2$Cumulative_cases_pc)
is.na(inner_data2$Cumulative_cases_pc) = sapply(inner_data2$Cumulative_cases_pc, is.infinite)

confint(model9) 


summary(model6)


summary.lm(model6)

dog4 = log(inner_data3$Ave_NewCase)
dog4

# New Model Selection 

inner_data3 = read.csv("/Users/willkhouri/STA_Analysis/STA141FinalProject/Data/final_data_quarter_inner_join3.csv")


model13 = lm(inner_data3$X.GDP..QTR. ~ dog4 + inner_data3$Country, inner_data3)
summary(model13) 





  


