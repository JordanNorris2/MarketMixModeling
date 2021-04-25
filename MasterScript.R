#import data

data <- read.csv(file = file.choose(), header = T)

str(data)
View(data)

#packages
library(dplyr)
library(stats)
library(prophet) #seasonality
library(ggplot2)
library(tidyr)

#clean it

str(data)


#seasonality: We create a time series model. Then, we use monthly trend as dummy variables.

forcast <- forcast %>%
  select(ds,y) 

View(forcast)

forcast <- forcast %>%
  rename(
    ds = y,
    y = ds
  )


forcast$y <- as.Date(forcast$y, origin = lubridate::origin)



str(forcast)

output <- prophet(forcast, weekly.seasonality =TRUE)

future <- make_future_dataframe(output, periods = 52, freq = 'month')

final_model <- predict(output, future)
tail(final_model[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(output ,final_model) +
  xlab("date") +
  ylab("revenue") 

#with this, we see that between Dec - Feb, there is high revenue. Holiday season, therefore, we will put dummy variables for those


str(data)

data$DATE <- as.Date(data$DATE, origin = lubridate::origin)


data$month <- months(data$DATE, abbreviate = F)


data$stout <- if_else(data$month %in% c('December' , 'January' , 'February'), 1, 0)

data <- data %>%
  select(
    revenue,
    tv_S,
    ooh_S,
    print_S,
    facebook_S,
    search_S,
    stout
  )

str(data)
View(data)





#ADSTOCK: geometric vs delayed vs S hill

#includes hill function
adstockTransform <- function(x){
  stats::filter( 1/(1+exp(-2*x)), 0.25, method = "recursive")
}








#model: Create the linear model that adds the baseline, the adstock, the seasonality, and the variables.

#before seasonality
model.pred <- lm(revenue ~ tv_S + ooh_S + print_S + facebook_S + search_S, data = data)
summary(model.pred)

#after seasonality
seasonal.pred <- lm(revenue ~ stout + tv_S + ooh_S + print_S + facebook_S + search_S, data = data)
summary(seasonal.pred)

#after adstock + Seasonality

final.pred <- lm(revenue ~ stout + adstock.tv + ooh_S + print_S + adstock.facebook + adstock.search, data = data)
summary(final.pred)

plot(final.pred)

#we see an increase in R^2, which is the goodness of fit of about 13%.

#trying a log-log model 

loglinear.pred <- lm(log(revenue) ~ log(stout) + log(adstockTranform(tv_S)) + log(ooh_S) + log(print_S) + log(adstockTranform(facebook_S)) + log(adstockTransform(search_S)), data = data)
summary(loglinear.pred)
plot(loglinear.pred)

#R^2 goes up to 70% from 56% | A 14% increase. 1% increase in FB coefficient will result in a 4.3% increase in sales


#PLOTTING DATA
library(ggplot2)
 
scatterPlotPlusFit <- ggplot(data, aes(x = adstock.tv, y = revenue)) +
  geom_point() +
  geom_smooth()

