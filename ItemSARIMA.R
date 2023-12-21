# libraries
# install.packages('lubridate')
library(lubridate)

library(astsa)
library(forecast)

# read data to R variable
demand.data <- read.csv("../input/train.csv")

# use date format for dates
demand.data$date <- as.Date(demand.data$date, "%Y-%m-%d")

head(demand.data)

# read data to R variable
test.data <- read.csv("../input/test.csv")

# use date format for dates
test.data$date <- as.Date(test.data$date, "%Y-%m-%d")

head(test.data)

# Create lists
item <- list()
sub <- list()

for(i in 1:50) { # 50 items
  for(j in 1:10){ # 10 stores
    
    # create separate dataframes
    assign(paste("demand.data",j,i,sep="."),subset(demand.data, store == j & item == i )) -> data
    
    assign(paste("tsw",j,i,sep="."), ts(data["sales"], start = decimal_date(as.Date("2013-01-01")),frequency = 7)) -> tsw
    
    # Regression forecast 
    
    assign(paste("lm",j,i,sep="."),tslm(tsw ~ trend + season) ) -> lm
    assign(paste("lm.forecast",j,i,sep="."),forecast(lm,h=90) ) -> lm.forecast
    
    # ARIMA forecast on the residuals
    
    assign(paste("residuals",j,i,sep="."),auto.arima(lm$residuals)) -> residuals
    assign(paste("residual.forecast",j,i,sep="."), forecast(residuals,h=90)) -> residual.forecast
    
    # Forecast
    
    assign(paste("y",j,i,sep="."), data.frame(lm.forecast)$Point.Forecast ) -> y
    assign(paste("x",j,i,sep="."), data.frame(residual.forecast)$Point.Forecast ) -> x
    assign(paste("forecast.sales",j,i,sep="."), data.frame(round(x+y,0)) ) -> forecast.sales  
    
    
    item[[j]] <- forecast.sales   # add it to your list 
  }
  
  assign(paste("item",i,sep="."),do.call(rbind,item) ) -> ii
  
  sub[[i]] <- ii # add it to your list 
  
}

assign(paste("demand.sales"),do.call(rbind,sub)) 

# submission

names(demand.sales) <- "sales"

submission_tslm <- cbind(test.data,demand.sales)
submission_tslm <- submission_tslm[,c("id","sales")]

write.csv(submission_tslm, file = "submission_tslm.csv", row.names = F)

residuals.1.1 # Store 1 Item 1

residuals.10.2 # Store 10 Item 2