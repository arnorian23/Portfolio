




library(ISLR)
library(dplyr)

head(Auto)

model = lm(mpg ~ horsepower, data = Auto)
summary(model)

head(mtcars)
MTcars = data.frame(mtcars)

model1 = lm(mpg ~ hp, data = mtcars)
summary(model1)

model2 = lm(mpg ~ hp + as.factor(am) + as.factor(vs), data = mtcars)
summary(model2)

model3 = lm(mpg ~ hp + as.factor(am) + as.factor(vs) + vs*hp, data = mtcars)
summary(model3)

library(MASS)

?Boston
head(Boston)
attach(Boston)

Boston = Boston %>% mutate(Result = case_when(medv > 30 ~ 1,
                                              medv <= 30 ~ 0))

Boston_df = subset(Boston, select = -c(medv))

head(Boston_df)

model4 = glm(Result ~., data = Boston_df, family = 'logit')

summary(model4)

library(caret)
pred <-predict(model4,Boston_df, type='response')

Boston_df = cbind(Boston_df, pred)

Boston_df = Boston_df %>% mutate(prd = as.integer(pred >=0.50))

confusion = confusionMatrix(data = as.factor(Boston_df$Result), reference = as.factor(Boston_df$predictions))

print(confusion)

Boston_df
#Create a confusion matrix showing the results of the training set predicitons compared to ground truth
#Use the thresholded prediction labels as the predicted class 
#Use the known true labels as the reference case
confusion<-confusionMatrix(data=as.factor(data_Q5$pred_class),reference=data_Q5$Clicked.on.Ad)

#print/plot the resulting confusion matrix output
print(confusion)

abalone_df = read.csv('abalone.csv')
head(abalone_df)

model6 = lm(Rings ~ Diameter + Height, data = abalone_df)
summary(model6)

abalone_m = abalone_df %>% filter(Type == 'M' || Type == 'I')
abalone_i = abalone_df %>% filter(Type == 'I')
abalone_f = abalone_df %>% filter(Type == 'F')

head(abalone_m)

head(abalone_f)
head(abalone_i)

abalone_fi = rbind(abalone_i, abalone_f)

abalone_fifinal = as.tibble(abalone_fi)

abalone_fifinal %>% print(n=Inf)

(tibble. print_max = Inf)

max.print(abalone_fi)
model_m = lm(Diameter ~ Type, data = abalone_m)
summary(model_m)

model_f = lm(Diameter ~ as.factor(Type), data = abalone_fifinal)
summary(model_f)


head(admission)

admission = read.csv('admissions.csv')
head(admission)

model7 = glm(Admitted ~., data = admission)
summary(model7)

pred <-predict(model7,admission, type='response')

admission = cbind(admission, pred)

admission = admission %>% mutate(prd = as.integer(pred >=0.75))

confusion = confusionMatrix(data = as.factor(admission$predictions), reference = as.factor(admission$Admitted))

print(confusion)


library(PerformanceAnalytics)

head(managers)

buffett = transform(buffett, MktExcess = Mkt-RF, FundExcess = Brk_ret - RF)
model1 = lm(FundExcess ~ MktExcess, data = buffett)
summary(model1)

sd(managers$HAM1)
sd(managers$HAM3)
sd(managers$HAM4)
sd(managers$SP500)

names(managers) = str_replace_all(names(managers), c(" " = "." , "," = "" ))
head(managers)

managers = transform(managers, MktReturn = SP500.TR, HAM3Jensen = HAM3 - US.10Y.TR, Ham4Jensen = HAM4 - US.10Y.TR)

managers = transform(managers, MktReturn = SP500.TR, HAM1Jensen = HAM1 - US.10Y.TR)

managers = transform(managers, HAM2Jensen = HAM2 - US.10Y.TR, HAM3Jensen = HAM3 - US.10Y.TR, HAM4Jensen = HAM4 - US.10Y.TR,
                     HAM5Jensen = HAM5 - US.10Y.TR, HAM6Jensen = HAM6 - US.10Y.TR)
head(managers)

alpha1 = lm(HAM1Jensen ~ MktExcess, data = managers)
summary(alpha1)

alpha2 = lm(HAM2Jensen ~ MktExcess, data = managers)
summary(alpha2)

alpha3 = lm(HAM3Jensen ~ MktExcess, data = managers)
summary(alpha3)

alpha4 = lm(HAM4Jensen ~ MktExcess, data = managers)
summary(alpha4)

managers

TreynorRatio(managers$HAM3Jensen, managers$MktReturn, managers$US.10Y.TR)
TreynorRatio(managers$HAM4, managers$MktReturn, managers$US.10Y.TR)



CAPM.beta(managers$HAM3, managers$MktReturn, managers$US.10Y.TR)
write.csv(managers,'/Users/arno220512/Documents/School stuff/GA Tech/Fall 2021/Data Analysis for Business (MGT 6203)/R/managers.csv', row.names = TRUE)
managers[1:1848,]
library(lubridate)

#load data and create xts dataset
fund <- read.csv("contrafund.csv")

#converting dates to standard YYYY-MM-DD format
fund$Date <- mdy(fund$Date)

fund
#Sorting data by dates
fund2<- fund[order(fund$Date),]

#create an xts dataset
All.dat <- xts(fund2[,-1],order.by = fund2[,1],)

treynor = TreynorRatio(All.dat$ContraRet, All.dat$Market.Return, All.dat$Risk.Free)

head(fund2)
head(All.dat)
treynor

library(tidyverse)
library(tidyquant)

stock_prices = c('BAC','V','WFC','GS') %>% tq_get(get = 'stock.prices', from = '2017-01-01', to = '2017-12-31')

head(stock_prices)

stock_prices2 = stock_prices[order(stock_prices$date),]



stock_returns_monthly = stock_prices %>% group_by(symbol) %>% 
  tq_transmute(select = adjusted, mutate_fun = periodReturn, period = 'monthly', col_rename = 'Ra')

head(stock_returns_monthly)

stock_returns_final = xts(stock_returns_monthly[,-2],order.by = as.POSIXct(stock_returns_monthly$date))

head(stock_returns_final)
weights_ans = c(0.25, 0.25, 0.25, 0.25)

portfolio_returns_monthly = stock_returns_monthly %>%
  tq_portfolio(assets_col = symbol, returns_col = Ra, weights = weights_ans,
               col_rename = 'Ra') %>% mutate(date = substring(date, 1,7)) %>%
  rename(Date = date)

head(portfolio_returns_monthly)

portfolio_returns_monthly_final = xts(portfolio_returns_monthly[,-1], order.by = as.POSIXct(portfolio_returns_monthly$Date))


final_portfolio = left_join(portfolio_returns_monthly, fund2)

mean(portfolio_returns_monthly$Ra)
beta(portfolio_returns_monthly)

TreynorRatio(portfolio_returns_monthly$Ra, All.dat$Market.Return, All.dat$Risk.Free)


factors = read.csv('Final_Exam_Factors.csv')
head(factors)

nvda_model = lm(NVDA ~ SMB + HML + QMJ + BAB + MOM + RF + MKT + MKT_RF, data = factors)
summary(nvda_model)

intc_model = lm(INTC ~ SMB + HML + QMJ + BAB + MOM + RF + MKT + MKT_RF, data = factors)
summary(intc_model)

data <- read.csv("KAG_wrangled_dataset.csv",stringsAsFactors = FALSE)   

head(data)

data %>%

dataQ1 = data %>%
  filter(Impressions > 10000)

getwd()

write.csv(dataQ1,'/Users/arno220512/Documents/School stuff/GA Tech/Fall 2021/Data Analysis for Business (MGT 6203)/R/final_data.csv', row.names = TRUE)


customertravel = read.csv('Customertravel_v3.csv')

head(customertravel)
attach(customertravel)
travel_model = glm(Churn ~ FrequentFlyer + AnnualIncomeClass + BookedHotel, data = customertravel, family = 'binomial')
summary(travel_model)


travel_model2 = glm(Churn ~ FrequentFlyer + AnnualIncomeClass + BookedHotel + Age + ServicesOpted, data = customertravel, family = 'binomial')
summary(travel_model2)

FrequentFlyer = c("Yes", "No", "Yes","No","Yes")
AnnualIncomeClass = c('Middle','High','Low','Middle','High')
BookedHotel = c("Yes", "Yes", "Yes","No","No")
Age = c(21,32,43,54,65)
ServicesOpted = c(0,0,6,5,1)
new_data <- data.frame(FrequentFlyer = FrequentFlyer, AnnualIncomeClass = AnnualIncomeClass, BookedHotel = BookedHotel, Age = Age, ServicesOpted = ServicesOpted)

new_data
##PREDICT PROBABILITY
predict(travel_model2, new_data, type="response") 

sample_data = read.csv('sample_data.csv')
sample_data
rowMeans(sample_data)

demand = read.csv('Store_Demand_Final.csv')
demand

demand$Date = mdy(demand$Date)
class(demand$Date)
demand

demand_ses = ses(train$total_demand, alpha = 0.25, h = 5)
demand_ses
test
demand_ses$model

accuracy(demand_ses, test$total_demand)
demand_data = as.data.frame(demand_data)
head(demand_data)

demand_data[,2]
accuracy(demand_ses, demand_data$V1)

demand_data 
RMSE(demand_data, )

library(forecast)

fit1 = meanf(demand_data, h = 5, lambda = NULL)

accuracy(fit1)

head(demand)
demand[96:100,]
train = demand[1:95,]
test = demand[96:100,]
test

train

