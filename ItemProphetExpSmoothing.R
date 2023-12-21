(library(data.table))

(library(tidyverse))

(library(gridExtra))

(library(forecast))
library(prophet) ### For Prophet Forecasting
(library(nnfor))   

train=fread("../input/train.csv")
sprintf("The train data set has %d rows and %d columns", nrow(train), ncol(train) )
str(train)

test  <- fread("../input/test.csv")
sprintf("The test data set has %d rows and %d columns", nrow(test), ncol(test) )
str(test)

print("the summary of train sales is:")
summary(train$sales)


# Extraction of Year and Month of Year :
train$Year=year(train$date)        #returns the year from date i.e. 2013, 2014 etc.
train$Month=as.yearmon(train$date) #this yearmon() function is coming from zoo package returns the month of an year i.e Jan 2013, Feb 2015 etc

gbp1<-wes_palette("GrandBudapest2")[1]

ggplot(train, aes(x=sales))+
  geom_histogram(fill="#a6d96a", alpha=.9)+
  labs(x=NULL, y=NULL, title = "Histogram of Sale Price")+
  # scale_x_continuous(breaks= seq(0,600000, by=100000))+
  theme_minimal() + theme(plot.title=element_text(vjust=3, size=15) )

MSP <- aggregate(sales ~date, train, mean)
# MSP <-na.omit(ddply(data, 'date', summarise, mean(Sale_Prices, na.rm=T)))

sl1 <-ggplot(MSP, aes(x=as.factor(date), y=sales))+
  geom_line(color=gbp1, aes(group=1), size=1.5)+
  geom_point(colour=gbp1, size = 3.5, alpha=0.5)+
  labs(title="The Growth of Sale Prices by date", x=NULL, y="Sale Price")+
  theme( plot.title=element_text(vjust=3, size=15) ) + theme_minimal()

MSP$rate = c(0, 100*diff(MSP$sales)/MSP[-nrow(MSP),]$sales)

sl2 <-ggplot(MSP, aes(x=as.factor(date), y=rate))+
  geom_line(color= "gray50", aes(group=1), size=1)+
  #geom_point(colour=gbp1, size = 3.5, alpha=0.5)+
  labs(title="Change rate of Sale Price", x="date", y="rate of change")+
  geom_hline(yintercept = 0, color = gbp1 )+
  theme(plot.title=element_text(size=15))+ theme_minimal()

grid.arrange(sl1,sl2)

MSP <- aggregate(sales ~Month, train, mean)
# MSP <-na.omit(ddply(data, 'date', summarise, mean(Sale_Prices, na.rm=T)))

sl1 <-ggplot(MSP, aes(x=as.factor(Month), y=sales))+
  geom_line(color=gbp1, aes(group=1), size=1.5)+
  geom_point(colour=gbp1, size = 3.5, alpha=0.5)+
  labs(title="The Growth of Sale Prices by Month of Year", x=NULL, y="Sale Price")+
  theme( plot.title=element_text(vjust=3, size=15) ) + theme_minimal()

MSP$rate = c(0, 100*diff(MSP$sales)/MSP[-nrow(MSP),]$sales)

sl2 <-ggplot(MSP, aes(x=as.factor(Month), y=rate))+
  geom_line(color= "gray50", aes(group=1), size=1)+
  #geom_point(colour=gbp1, size = 3.5, alpha=0.5)+
  labs(title="Change rate of Sale Price", x="Month", y="rate of change")+
  geom_hline(yintercept = 0, color = gbp1 )+
  theme(plot.title=element_text(size=15))+ theme_minimal()

grid.arrange(sl1,sl2)

MSP <- aggregate(sales ~Year, train, mean)
# MSP <-na.omit(ddply(data, 'date', summarise, mean(Sale_Prices, na.rm=T)))

sl1 <-ggplot(MSP, aes(x=as.factor(Year), y=sales))+
  geom_line(color=gbp1, aes(group=1), size=1.5)+
  geom_point(colour=gbp1, size = 3.5, alpha=0.5)+
  labs(title="The Growth of Sale Prices by Year", x=NULL, y="Sale Price")+
  theme( plot.title=element_text(vjust=3, size=15) ) + theme_minimal()

MSP$rate = c(0, 100*diff(MSP$sales)/MSP[-nrow(MSP),]$sales)

sl2 <-ggplot(MSP, aes(x=as.factor(Year), y=rate))+
  geom_line(color= "gray50", aes(group=1), size=1)+
  #geom_point(colour=gbp1, size = 3.5, alpha=0.5)+
  labs(title="Change rate of Sale Price", x="Year", y="rate of change")+
  geom_hline(yintercept = 0, color = gbp1 )+
  theme(plot.title=element_text(size=15))+ theme_minimal()

grid.arrange(sl1,sl2)

unique(train$store)
Year_state<-aggregate(sales ~store+Year, train,mean)
pal<-rep(brewer.pal(10, "BrBG"),5)

ggplot(Year_state, aes(group = store ))+
  geom_line(aes(x=Year,y=sales,color=store), alpha=0.5, show.legend=F)+
  labs(title="The Growth of Sales Price by Store from 2013 - 2017", x=NULL
  )+
  theme(panel.background=element_rect(fill = "Black"),
        plot.title=element_text(vjust=3, size=15),
        panel.grid.major=element_line(color = pal))

unique(train$item)
Year_state<-aggregate(sales ~item+Year, train,mean)
pal<-rep(brewer.pal(10, "BrBG"),5)

ggplot(Year_state, aes(group = item ))+
  geom_line(aes(x=Year,y=sales,color=item), alpha=0.5, show.legend=F)+
  labs(title="The Growth of Sales Price by Store from 2013 - 2017", x=NULL
  )+
  theme(panel.background=element_rect(fill = "Black"),
        plot.title=element_text(vjust=3, size=15),
        panel.grid.major=element_line(color = pal))

train_final_store1_item1=subset(train,train$store==1 & train$item==1)

stats=data.frame(y=log1p(train_final_store1_item1$sales)
                 ,ds=train_final_store1_item1$date)
stats=aggregate(stats$y,by=list(stats$ds),FUN=sum)
head(stats)
colnames(stats)<- c("ds","y")

model_prophet = prophet(stats)
summary(model_prophet)
future = make_future_dataframe(model_prophet, periods = 90)
forecast = predict(model_prophet, future)

add_changepoints_to_plot <- function(m, threshold = 0.01, cp_color = "red",
                                     cp_linetype = "dashed", trend = TRUE, ...) {
  layers <- list()
  if (trend) {
    trend_layer <- ggplot2::geom_line(
      ggplot2::aes_string("ds", "trend"), color = cp_color, ...)
    layers <- append(layers, trend_layer)
  }
  signif_changepoints <- m$changepoints[abs(m$params$delta) >= threshold]
  cp_layer <- ggplot2::geom_vline(
    xintercept = as.integer(signif_changepoints), color = cp_color,
    linetype = cp_linetype, ...)
  layers <- append(layers, cp_layer)
  return(layers)
}
plot(model_prophet, forecast)+ add_changepoints_to_plot(model_prophet)

prophet_plot_components(model_prophet, forecast)

Currently there are two types of neural network available, both feed-forward:
  
  a. multilayer perceptrons (use function mlp);

b. extreme learning machines (use function elm)

y<-ts(stats$y,frequency=365,start = 2013,end=2017)
head(y)

plot(y)

h <- 90   # We will predict for 90 days sales starting from 01-JAN-2018.
tt <- cbind(c(1:(length(y)+h),rep(0,2*h)))
# Observe that the deterministic trend ends with zeros
#print(tt)

# Fit a network with no differencing, no univariate lags, and fixed deterministic trend
fit1 <- mlp(y,difforder=0,lags=0,xreg=tt,xreg.lags=list(0),xreg.keep=TRUE)

print(fit1)
plot(fit1)

plot(forecast(fit1,h=h,xreg=tt))

print("The MSE for Store-1 & Item -1 is")
print(round(fit1$MSE,4))

tt2 <- tt[-(1:h),,drop=FALSE]
plot(forecast(fit1,h=h,xreg=tt2))