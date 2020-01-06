library("ggplot2")
library('forecast')
library('tseries')
library('MLmetrics')
library("plotrix")
library("forecast")
library('strucchange')

#1-2-3**************************************************************
setwd('C:/Users/Lollo/Documents/OMA/SCH/TP/')
data <- read.table('0219_td_centrale.csv',header=TRUE, sep = ";")

data$earning_yield = data$earnings/data$price


ggplot(data = data, aes(rates, earning_yield)) + geom_point() + geom_smooth(method='lm')
reg = lm(data$earning_yield ~ data$rates)

data$MCO = reg$coefficients[1] + reg$coefficients[2]*data$rates



  
#4**************************************************************
t.test(data$earning_yield, data$MCO)
shapiro.test(reg$residuals)
summary(reg)$r.squared



#5**************************************************************
plot(density(reg$residuals), xlab="Support", main="Residuals density")
qqnorm(reg$residuals);qqline(reg$residuals)
acf(reg$residuals, lag = length(reg$residuals)-1, main = "Residual ACF")
plot(data$MCO,reg$residuals,xlab="Prediction values", ylab="Residuals values", main="Residuals vs. fitted")
plot(reg$residuals,main="Résidus")
abline(h=0,col="red")



#6 - 7 **************************************************************
data$deflated<- rep(NaN,nrow(data))
#croissance de l'indice des prix à la consommation (CPI) sur sur 12 mois 
data$deflated[13:nrow(data)] <- 100*(data$cpi[13:nrow(data)] - data$cpi[1:(nrow(data)-12)])/data$cpi[1:(nrow(data)-12)]
data$real_rates[13:nrow(data)]<-data$rates[13:nrow(data)]-data$deflated[13:nrow(data)]

tmp_data = data[13:nrow(data),]
ggplot(data = tmp_data, aes(real_rates[13:nrow(data)], earning_yield[13:nrow(data)])) + geom_point() + geom_smooth(method='lm')

reg_real = lm(data$earning_yield[13:nrow(data)] ~ data$real_rates[13:nrow(data)])
data$MCO_real = reg_real$coefficients[1] + reg_real$coefficients[2]*data$rates

# meme procedure qu'auparavant**************************************************************
t.test(data$earning_yield, data$MCO_real)
fisher.test(data$earning_yield[0:10], data$MCO_real[0:10]) # why 10?
summary(reg_real)$r.squared

plot(density(reg_real$residuals), xlab="Support", main="Residuals density")
qqnorm(reg_real$residuals);qqline(reg_real$residuals)
acf(reg_real$residuals, lag = length(reg_real$residuals)-1, main = "Residual ACF")
plot(data$MCO_real[13:nrow(data)],reg_real$residuals, xlab="Prediction values", ylab="Residuals values", main="Residuals vs. fitted")
plot(reg_real$residuals,main="Résidus")
abline(h=0,col="red")



#8**************************************************************
acf(data$earning_yield, lag = length(data$earning_yield)-1, main = "ACF Earnings yield")
pacf(data$earning_yield, lag = length(data$earning_yield)-1, main ="PACF Earnings yield")



#9 **************************************************************
differentiation <- diff(data$earning_yield, differences = 1)
acf(differentiation, lag = length(differentiation)-1, main = "Differentiation - ACF Earnings yield")
pacf(differentiation, lag = length(differentiation)-1, main ="Differentiation - PACF Earnings yield")
autoa_1 = auto.arima(data$earning_yield)



#10**************************************************************
model <- arima(data$earning_yield, order=c(1,1,0), method="ML")
W = 15
prediction <- predict(model, W, se.fit=TRUE, interv="confidence")
up_ <- prediction$pred + 1.96*prediction$se
lo_ <- prediction$pred - 1.96*prediction$se
x_ax = 250:(250+W-1)

plot(1:W, prediction$pred,type="l",col="red",xlab="N. month", ylab="Predicted Earnings yield", main="Predicted Earnings yield")
plot(1:W, prediction$se,type="l",col="green",xlab="N. month", ylab="Predicted s.d.", main="Predicted s.d. of E.Y.")

par(new=TRUE)
plot(x_ax,up_,col="green")
par(new=TRUE)
plot(x_ax,lo_,col="green")

prediction_df <- data.frame(list(prediction$pred, up_,lo_))
matplot(prediction_df,main = "Confidence intervals (95%) on E.Y. predictions", xlab="N. month", ylab="Predicted Earnings yield")



#11**************************************************************
n_train = 245
n_test = nrow(data) - n_train
model <- arima(data$earning_yield[1:n_train], order=c(1,1,0), method="ML")
prediction <- predict(model, n_test, se.fit=TRUE)

RMSE(prediction$pred, data$earning_yield[(n_train+1):nrow(data)])
MAE(prediction$pred, data$earning_yield[(n_train+1):nrow(data)])



#11 bis************************************************************** 
r_real = (data$earning_yield[13:(nrow(data)-1)] - reg_real$coefficients[1])/reg_real$coefficients[2]
r_simple = (data$earning_yield[13:(nrow(data)-1)] - reg$coefficients[1])/reg$coefficients[2]

n_h = 236
pval = vector("numeric", n_h)
for (i_pval in 1:n_h){
  pval[i_pval] = dm.test(data$real_rates - r_real,data$real_rates -  r_simple, alternative = c("two.sided"), h = i_pval, power = 2)$p.value
}
plot(1:n_h,pval, main = "p-values as a function of h", xlab = "h", ylab ="p-value")
abline(h=0.05,col="red")



#12**************************************************************
n_per = floor(nrow(data)/12) - 1
betas = vector("numeric", n_per)
betas_downs = vector("numeric", n_per)
betas_ups = vector("numeric", n_per)

for (model_i in 2:n_per){
  w = (1+(model_i-1)*12):(model_i*12)
  tmp_mod = lm(data$earning_yield[w] ~ data$real_rates[w])
  conf_int = confint(tmp_mod,'data$real_rates[w]', level = 0.95)
  betas[model_i-1] = summary(tmp_mod)$coefficients[2,1] 
  betas_downs[model_i-1] = conf_int[1]
  betas_ups[model_i-1] = conf_int[2]
}
require(plotrix)
x_beta = 1:(n_per-1)
betas_ord = betas[order(betas[x_beta])]
betas_downs_ord = betas_downs[order(betas[x_beta])]
betas_ups_ord = betas_ups[order(betas[x_beta])]

df_beta_conf <- data.frame(x_beta,
                           betas[x_beta],
                           betas_downs[x_beta],
                           betas_ups[x_beta])

df_beta_conf_ord <- data.frame(x_beta,
                               betas_ord,
                               betas_downs_ord,
                               betas_ups_ord)
require(ggplot2)
pp = ggplot(df_beta_conf, aes(x = x_beta, y = betas[x_beta])) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = betas_ups[x_beta], ymin = betas_downs[x_beta]))
pp + xlab("N. month")


ggplot(df_beta_conf_ord, aes(x = x_beta, y = betas_ord)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = betas_ups_ord, ymin = betas_downs_ord))
  
# 13**************************************************************



## compute OLS-CUSUM fluctuation process
CUSUM <-efp(data$earning_yield ~ data$real_rates,type='OLS-CUSUM')
bound.CUSUM <- boundary(CUSUM, alpha=0.05)
plot(CUSUM)
## add the boundaries in another colour
bound <- boundary(temp.cus, alpha = 0.05)
lines(bound, col=4)
lines(-bound, col=4)