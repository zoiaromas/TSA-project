install.packages("urca")
library(urca)
library(foreign)
library(RJDemetra)
library(tidyverse)
library(tidyquant)
library(forecast)
library(tseries)
library(lmtest)
library(xts)
library(urca)
library(aTSA)
require(ggplot2)
require(zoo)
require(urca)
require(fpp2)
install.packages("dynlm")
library(dynlm)

niesezonowy$Date <- as.Date(niesezonowy$DATE)
sezonowy$Date <- as.Date(sezonowy$DATE)

niesezonowy$Value <- as.numeric(niesezonowy$POLVOILUSDM)
sezonowy$Value <- as.numeric(sezonowy$MRTSSM4423XUSN)

niesezonowy$Value_log <- log(niesezonowy$Value)
#próbka in-sample dla szeregu niesezonowego - 3 miesiące
ts_niesezonowy <-  ts(data=niesezonowy$Value, start=c(2000,1), end=c(2023, 9), frequency=12)
ts_niesezonowy.xts = as.xts(ts_niesezonowy)

ts_niesezonowy_c <-  ts(data=niesezonowy$Value, start=c(2000,1), end=c(2023, 12), frequency=12)
ts_niesezonowy.xts = as.xts(ts_niesezonowy_c)

ts_niesezonowy_log <-  ts(data=niesezonowy$Value_log, start=c(2000,1), end=c(2023, 9), frequency=12)
ts_niesezonowy_log.xts = as.xts(ts_niesezonowy_log)

#próbka out-of-sample dla szeregu sezonwego - 1 rok
ts_sezonowy <- ts(data=sezonowy$Value, start = c(2000, 1), end=c(2022, 12), frequency=12)
ts_sezonowy.xts = as.xts(ts_sezonowy)

#wykresy
plot(ts_niesezonowy, 
     type='l', 
     col='black',
     xlab='Data',
     ylab='Cena oliwy z oliwek [$ za tonę]',
     lwd=1)


plot(ts_sezonowy, 
     type='l', 
     col='black',
     xlab='Data',
     ylab='Sprzedaż detaliczna [mln $]',
     lwd=1)

##########################
###SZEREG NIESEZONOWY ###
##########################


#dekompozycja szeregu - NIE UZYWAM W PRACY
niesezonowy_da = decompose(ts_niesezonowy, 'additive')
plot(niesezonowy_da)

niesezonowy_dm = decompose(ts_niesezonowy, 'multiplicative')
plot(niesezonowy_dm)

forecast::ggsubseriesplot(ts_niesezonowy, ylab = "Sprzedaż detaliczna [mln $]", xlab = "Miesiąc")

#DF
df_test2 <- ur.df(ts_niesezonowy, type="none", lags=0)
summary(df_test2)
#DF z dryfem
df_test3 <- ur.df(ts_niesezonowy, type="drift", lags=0)
summary(df_test3)
#DF z dryfem i trendem
df_test4 <- ur.df(ts_niesezonowy, type="trend", lags=0)
summary(df_test4)
#ADF

source("funs/TESTDF.R")
x <- ur.df(na.omit(ts_niesezonowy), type = c('trend'), lags=0)
summary(x)

#róznicowanie szeregu
I1_ts_niesezonowy <- diff.xts(ts_niesezonowy)
I1_ts_niesezonowy_log <- diff.xts(ts_niesezonowy_log)
d.nonseas.is <- window(I1_ts_niesezonowy, end=c(2023,9))
nonseas.is <- window(ts_niesezonowy, end=c(2023,9))

testdf(variable = nonseas.is, ADF_type="c", ADF_max_order = 3,BG_max_order = 4)

testdf(variable = d.nonseas.is, ADF_type="nc", ADF_max_order = 2,BG_max_order = 4)


plot(I1_ts_niesezonowy_log, 
     type='l', 
     col='black',
     xlab='Data',
     ylab='Pierwsze różnice dla ceny oliwy z oliwek [$ za tonę]',
     lwd=1)


#STACJONARNOŚĆ - test ADF z trendem

df.test.dns <- ur.df(ts_niesezonowy, type = c("trend"), lags =2)
#test (summary) sprawdzany po ponizszej procedurze

resids.dns <- df.test.dns@testreg$residuals
bg1 <- bgtest(resids.dns ~ 1, order = 1)
bg1
bg2 <- bgtest(resids.dns ~ 1, order = 2)
bg2
bg3 <- bgtest(resids.dns ~ 1, order = 3)
bg3
bg4 <- bgtest(resids.dns ~ 1, order = 4)
bg4
bg5 <- bgtest(resids.dns ~ 1, order = 5)
bg5
bg6 <- bgtest(resids.dns ~ 1, order = 6)
bg6

BG_dns <- matrix(c(bg1$p.value, bg1$statistic, bg2$p.value, bg2$statistic, bg3$p.value, bg3$statistic, bg4$p.value, bg4$statistic, bg5$p.value, bg5$statistic, bg6$p.value, bg6$statistic), ncol=2, byrow=TRUE)
colnames(BG_dns) <- c('p-value','Statystyka Testu B-G')
rownames(BG_dns) <- c(1,2,3,4,5,6)
as.table(BG_dns)

summary(df.test.dns)

#szereg jest niestacjonarny ,autokororelacja wyeliminowana na poziomie opoznienia k=2

D_dns<-diff.xts(ts_niesezonowy)

plot(D_dns, type = "l", col="black", ylab="Pierwsze różnice ceny oliwy z oliwek", xlab="Data")

df.test.D_dns <- ur.df(na.omit(D_dns), type = c("trend"), lags = 1)

resids.D_dns <- df.test.D_dns@testreg$residuals
bg1 <- bgtest(resids.D_dns ~ 1, order = 1)
bg1
bg2 <- bgtest(resids.D_dns ~ 1, order = 2)
bg2
bg3 <- bgtest(resids.D_dns ~ 1, order = 3)
bg3
bg4 <- bgtest(resids.D_dns ~ 1, order = 4)
bg4
bg5 <- bgtest(resids.D_dns ~ 1, order = 5)
bg5
bg6 <- bgtest(resids.D_dns ~ 1, order = 6)
bg6

BG_dns <- matrix(c(bg1$p.value, bg1$statistic, bg2$p.value, bg2$statistic, bg3$p.value, bg3$statistic, bg4$p.value, bg4$statistic, bg5$p.value, bg5$statistic, bg6$p.value, bg6$statistic), ncol=2, byrow=TRUE)
colnames(BG_dns) <- c('p-value','Statystyka Testu B-G')
rownames(BG_dns) <- c(1,2,3,4,5,6)
as.table(BG_dns)

summary(df.test.D_dns)


kpss.test <- ur.kpss(I1_ts_niesezonowe, type=c('mu'))
summary(kpss.test)
#######################
#KPSS Unit Root Test #
#######################

#Test is of type: mu with 5 lags. 

#Value of test-statistic is: 0.3873 

#Critical value for a significance level of: 
# 10pct  5pct 2.5pct  1pct
#critical values 0.347 0.463  0.574 0.739

#WHITE-NOISE

#Test Ljung-Boxa
Box.test(D_dns, type = "Ljung-Box", lag = 24)

#Test Boxa-Pearsa
Box.test(D_dns, type = "Box-Pierce", lag = 24)

#ACF i PACF
ACF <- acf(D_dns,
           lag.max = 24,
           na.action = na.pass,
           plot=FALSE)

ACF$lag <- ACF$lag * 12

plot(ACF,
     xlim=c(1,24),
     ylim=c(-0.5,0.5))

PACF <- pacf(D_dns,
             na.action=na.pass,
             plot=FALSE)

PACF$lag <- PACF$lag * 12

plot(PACF,
     xlim=c(1,24),
     ylim=c(-0.5,0.5),
     ylab='PACF')
#ARIMA
d.nonseas.is <- window(D_dns, end=c(2023,9))
nonseas.is <- window(ts_niesezonowy, end=c(2023,9))

nobs <- length(nonseas.is)
#######################################
#ARIMA(2,1,2)
arima212 <- arima(nonseas.is,
                  order = c(2, 1, 2))

arima212
coeftest(arima212) #MA(2) i AR(2) nieistotne

#badanie reszt
ACF <- acf(resid(arima212),
           lag.max = 24,
           na.action = na.pass,
           plot=FALSE)

ACF$lag <- ACF$lag * 12

plot(ACF,
     xlim=c(1,24),
     ylim=c(-0.5,0.5))

PACF <- pacf(resid(arima212),
             na.action=na.pass,
             plot=FALSE)

PACF$lag <- PACF$lag * 12

plot(PACF,
     xlim=c(1,24),
     ylim=c(-0.5,0.5),
     ylab='PACF')

Box.test(resid(arima212), type = "Ljung-Box", lag = 24)
Box.test(resid(arima212), type = "Box-Pierce", lag = 24)
#########################################
#ARIMA(2,1,0)
arima210 <- arima(nonseas.is,
                  order = c(2, 1, 0),
                  xreg=1:nobs)

arima210
coeftest(arima210) #AR(1) i AR(2) istotne

#badanie reszt
ACF <- acf(resid(arima210),
           lag.max = 24,
           na.action = na.pass,
           plot=FALSE)

ACF$lag <- ACF$lag * 12

plot(ACF,
     xlim=c(1,24),
     ylim=c(-0.5,0.5))

PACF <- pacf(resid(arima210),
             na.action=na.pass,
             plot=FALSE)

PACF$lag <- PACF$lag * 12

plot(PACF,
     xlim=c(1,24),
     ylim=c(-0.5,0.5),
     ylab='PACF')

Box.test(resid(arima210), type = "Ljung-Box", lag = 24)
Box.test(resid(arima210), type = "Box-Pierce", lag = 24)
#######################################
#ARIMA(0,1,2)

arima012 <- arima(nonseas.is,
                  order = c(0, 1, 2))

arima012
coeftest(arima012) #MA(1) i MA(2) istotne

#badanie reszt
ACF <- acf(resid(arima012),
           lag.max = 24,
           na.action = na.pass,
           plot=FALSE)

ACF$lag <- ACF$lag * 12

plot(ACF,
     xlim=c(1,24),
     ylim=c(-0.5,0.5))

PACF <- pacf(resid(arima012),
             na.action=na.pass,
             plot=FALSE)

PACF$lag <- PACF$lag * 12

plot(PACF,
     xlim=c(1,24),
     ylim=c(-0.5,0.5),
     ylab='PACF')

Box.test(resid(arima012), type = "Ljung-Box", lag = 24)
Box.test(resid(arima012), type = "Box-Pierce", lag = 24)

#######################################
#ARIMA(1,1,1)

arima111 <- arima(nonseas.is,
                  order = c(1, 1, 1))

arima111
coeftest(arima111) #MA(1) i AR(1) istotne

#badanie reszt
ACF <- acf(resid(arima111),
           lag.max = 24,
           na.action = na.pass,
           plot=FALSE)

ACF$lag <- ACF$lag * 12

plot(ACF,
     xlim=c(1,24),
     ylim=c(-0.5,0.5))

PACF <- pacf(resid(arima111),
             na.action=na.pass,
             plot=FALSE)

PACF$lag <- PACF$lag * 12

plot(PACF,
     xlim=c(1,24),
     ylim=c(-0.5,0.5),
     ylab='PACF')

Box.test(resid(arima111), type = "Ljung-Box", lag = 24)
Box.test(resid(arima111), type = "Box-Pierce", lag = 24)

#######################################
#ARIMA(2,1,1)

arima211 <- arima(nonseas.is,
                  order = c(2, 1, 1),
                  xreg=1:nobs)

arima211
coeftest(arima211) #tylko MA(1) istotne

#######################################
#ARIMA(0,1,1)

arima011 <- arima(nonseas.is,
                  order = c(0, 1, 1))

arima011
coeftest(arima011) #MA(1) istotne

#badanie reszt
ACF <- acf(resid(arima011),
           lag.max = 24,
           na.action = na.pass,
           plot=FALSE)

ACF$lag <- ACF$lag * 12

plot(ACF,
     xlim=c(1,24),
     ylim=c(-0.5,0.5))

PACF <- pacf(resid(arima011),
             na.action=na.pass,
             plot=FALSE)

PACF$lag <- PACF$lag * 12

plot(PACF,
     xlim=c(1,24),
     ylim=c(-0.5,0.5),
     ylab='PACF')

Box.test(resid(arima011), type = "Ljung-Box", lag = 24)
Box.test(resid(arima011), type = "Box-Pierce", lag = 24)

#######################################
#ARIMA(1,1,0)

arima110 <- arima(nonseas.is,
                  order = c(1, 1, 0),
                  xreg=1:nobs)

arima110
coeftest(arima110) # AR(1) istotne


#######################################
#ARIMA(1,1,2)

arima112 <- arima(nonseas.is,
                  order = c(1, 1, 2),
                  xreg=1:nobs)

arima112
coeftest(arima112) #MA(1) i MA(2) istotne

#test
teststat1<- -2*(as.numeric(logLik(arima011))-as.numeric(logLik(arima012)))
teststat1
pchisq(teststat1, df = 1, lower.tail = FALSE)
qchisq(0.95, df = 1) #wartość krytyczna
pchisq(teststat1, df=1, lower.tail = FALSE) #odrzucamy H0, że prosty model wystarczy

AIC(arima111, arima011, arima012, arima110, arima210)
BIC(arima111, arima011, arima012, arima110, arima210)

#Możesz także spróbować ręcznie ustawić parametry i porównać wyniki
fit_manual <- arima(ts_niesezonowy, order=c(1,1,1))
summary(fit_manual)

arima.best.AIC <- auto.arima(ts_niesezonowy,
                                                d = 1,             #parameter d w modelu ARIMA
                                                max.p = 4,         #p = maksymalna wartosc
                                                max.q = 4,         #q = maksymalna wartosc
                                                max.order = 8,     #suma p+q
                                                start.p = 0,       #Wartosc startowa dla p
                                                start.q = 0,       #Wartosc startowa dla q
                                                ic = "aic",        #Wybor modelu na podstawie kryterium informcyjne
                                                stepwise = FALSE,  #jezeli FALSE rozwaza wszystkie modeli
                                                allowdrift = TRUE, #model zawiera stalą
                                                trace = TRUE)      #wyswietlenie rozwazonych modeli

coeftest(arima.best.AIC)

#prognozowanie ARIMA
nonseas.prog=ts_niesezonowy_c[286:288]

forecast012 <-  predict(arima012,n.ahead = 3) 

plot(as.numeric(ts_niesezonowy_c), xlim=c(260,290),xlab='Numer obserwacji', ylim=c(2000, 11000),
     ylab='Cena oliwy z oliwek [$ za tonę]', type='l')
abline(v = 286, lty = 2, col = "gray")
time_points <- (length(ts_niesezonowy) + 1):(length(ts_niesezonowy) + length(forecast012$pred))
lines(time_points, forecast012$pred, col = "red", lwd = 2)
lines(time_points, forecast012$pred + 2 * forecast012$se, col = "red", lty = 3)
lines(time_points, forecast012$pred - 2 * forecast012$se, col = "red", lty = 3)

forecast011 <-  predict(arima011,n.ahead = 3) 
plot(as.numeric(ts_niesezonowy_c), xlim=c(260,290),xlab='Numer obserwacji', ylim=c(2000, 11000),
     ylab='Cena oliwy z oliwek [$ za tonę]', type='l')
abline(v = 286, lty = 2, col = "gray")
time_points <- (length(ts_niesezonowy) + 1):(length(ts_niesezonowy) + length(forecast011$pred))
lines(time_points, forecast011$pred, col = "red", lwd = 2)
lines(time_points, forecast011$pred + 2 * forecast011$se, col = "red", lty = 3)
lines(time_points, forecast011$pred - 2 * forecast011$se, col = "red", lty = 3)

forecast111 <-  predict(arima111,n.ahead = 3) 
plot(as.numeric(ts_niesezonowy_c), xlim=c(260,290),xlab='Numer obserwacji', ylim=c(2000, 11000),
     ylab='Cena oliwy z oliwek [$ za tonę]', type='l')
abline(v = 286, lty = 2, col = "gray")
time_points <- (length(ts_niesezonowy) + 1):(length(ts_niesezonowy) + length(forecast111$pred))
lines(time_points, forecast111$pred, col = "red", lwd = 2)
lines(time_points, forecast111$pred + 2 * forecast111$se, col = "red", lty = 3)
lines(time_points, forecast111$pred - 2 * forecast111$se, col = "red", lty = 3)


#bledy ARIMA(0,1,2)
#Tworzenie ramki danych z rzeczywistymi danymi i prognozami bez dodawania NA
ts_niesezonowy_c
n_forecasts <- length(forecast012$pred)
wpi_forecast <- data.frame(
  actual = ts_niesezonowy_c,
  forecast = c(rep(NA, length(ts_niesezonowy_c) - n_forecasts), forecast012$pred)
)
#Możesz teraz wyświetlić ramkę danych, aby upewnić się, że wszystko jest zgodne
print(wpi_forecast)
#Usunięcie wierszy z NA
comparison <- na.omit(wpi_forecast)

#Wyświetlenie porównania
print(comparison)

#Obliczenie błędów prognozy
mae <- mean(abs(comparison$actual - comparison$forecast))
mse <- mean((comparison$actual - comparison$forecast)^2)
mape <- mean(abs((comparison$actual - comparison$forecast) / comparison$actual)) * 100
amape <- mean(abs((comparison$actual - comparison$forecast) / ((comparison$actual + comparison$forecast) / 2))) * 100

#Wyświetlenie wyników
cat("MAE:", mae, "\n")
cat("MSE:", mse, "\n")
cat("MAPE:", mape, "%\n")
cat("AMAPE:", amape, "%\n")

#bledy ARIMA(0,1,1)
#Tworzenie ramki danych z rzeczywistymi danymi i prognozami bez dodawania NA
ts_niesezonowy_c
n_forecasts <- length(forecast011$pred)
wpi_forecast <- data.frame(
  actual = ts_niesezonowy_c,
  forecast = c(rep(NA, length(ts_niesezonowy_c) - n_forecasts), forecast011$pred)
)
#Możesz teraz wyświetlić ramkę danych, aby upewnić się, że wszystko jest zgodne
print(wpi_forecast)
#Usunięcie wierszy z NA
comparison <- na.omit(wpi_forecast)

#Wyświetlenie porównania
print(comparison)

#Obliczenie błędów prognozy
mae <- mean(abs(comparison$actual - comparison$forecast))
mse <- mean((comparison$actual - comparison$forecast)^2)
mape <- mean(abs((comparison$actual - comparison$forecast) / comparison$actual)) * 100
amape <- mean(abs((comparison$actual - comparison$forecast) / ((comparison$actual + comparison$forecast) / 2))) * 100

#Wyświetlenie wyników
cat("MAE:", mae, "\n")
cat("MSE:", mse, "\n")
cat("MAPE:", mape, "%\n")
cat("AMAPE:", amape, "%\n")

#bledy ARIMA(1,1,1)
#Tworzenie ramki danych z rzeczywistymi danymi i prognozami bez dodawania NA
ts_niesezonowy_c
n_forecasts <- length(forecast111$pred)
wpi_forecast <- data.frame(
  actual = ts_niesezonowy_c,
  forecast = c(rep(NA, length(ts_niesezonowy_c) - n_forecasts), forecast111$pred)
)
#Możesz teraz wyświetlić ramkę danych, aby upewnić się, że wszystko jest zgodne
print(wpi_forecast)
#Usunięcie wierszy z NA
comparison <- na.omit(wpi_forecast)

#Wyświetlenie porównania
print(comparison)

#Obliczenie błędów prognozy
mae <- mean(abs(comparison$actual - comparison$forecast))
mse <- mean((comparison$actual - comparison$forecast)^2)
mape <- mean(abs((comparison$actual - comparison$forecast) / comparison$actual)) * 100
amape <- mean(abs((comparison$actual - comparison$forecast) / ((comparison$actual + comparison$forecast) / 2))) * 100

#Wyświetlenie wyników
cat("MAE:", mae, "\n")
cat("MSE:", mse, "\n")
cat("MAPE:", mape, "%\n")
cat("AMAPE:", amape, "%\n")


#Model liniowy Holta

holt <- HoltWinters(ts_niesezonowy,gamma=FALSE)

holt.pred <- predict(holt,n.ahead = 3,prediction.interval = TRUE)
plot(holt,xlab='Data',
     ylab='Cena oliwy z oliwek [$ za tonę]', ylim=c(1000,10000))
holt


plot(as.numeric(ts_niesezonowy_c), xlim = c(260, 290), xlab = 'Numer obserwacji', ylim = c(2000, 11000),
     ylab = 'Cena oliwy z oliwek [$ za tonę]', type = 'l')
abline(v = 286, lty = 2, col = "gray")

#Dodanie linii prognozy Holta
lines(286:288, holt.pred[,1], col = "blue")
lines(286:288, holt.pred[,2], col = "blue", lty = 2)
lines(286:288, holt.pred[,3], col = "blue", lty = 2)

#Dodanie linii prognozy ARIMA
time_points <- (length(ts_niesezonowy) + 1):(length(ts_niesezonowy) + length(forecast012$pred))
lines(time_points, forecast012$pred, col = "red", lwd = 2)
lines(time_points, forecast012$pred + 2 * forecast012$se, col = "red", lty = 3)
lines(time_points, forecast012$pred - 2 * forecast012$se, col = "red", lty = 3)

#SES
lines(286:288, ses_forecast$mean, col = "green")
lines(286:288, ses_forecast$lower[,2], col = "green", lty = 2)
lines(286:288, ses_forecast$upper[,2], col = "green", lty = 2)


#Dodanie legendy
legend("topleft", legend = c("ARIMA(0,1,2)", "SES", "Holt"), col = c("red", "green", "blue"), lty = 1, lwd = 2, cex = 0.8)


#SES
#Proste wygładzanie wykładnicze (SES)
ses_model <- ses(ts_niesezonowy, h = 3)

#Prognozy i przedziały ufności
ses_forecast <- predict(ses_model, h=3)

#Wykres wyników
plot(as.numeric(ts_niesezonowy_c), xlim = c(260, 290), xlab = 'Numer obserwacji', ylim = c(2000, 11000),
     ylab = 'Cena oliwy z oliwek [$ za tonę]', type = 'l')
abline(v = 286, lty = 2, col = "gray")

#Dodanie linii prognozy SES
lines(286:288, ses_forecast$mean, col = "green")
lines(286:288, ses_forecast$lower[,2], col = "green", lty = 2)
lines(286:288, ses_forecast$upper[,2], col = "green", lty = 2)

#bledy Holta
#Tworzenie ramki danych z rzeczywistymi danymi i prognozami bez dodawania NA
n_forecasts <- length(holt.pred[,1])
wpi_forecast <- data.frame(
  actual = ts_niesezonowy_c,
  forecast = c(rep(NA, length(ts_niesezonowy_c) - n_forecasts), holt.pred[,1])
)
#Możesz teraz wyświetlić ramkę danych, aby upewnić się, że wszystko jest zgodne
print(wpi_forecast)
#Usunięcie wierszy z NA
comparison <- na.omit(wpi_forecast)

#Wyświetlenie porównania
print(comparison)

#Obliczenie błędów prognozy
mae <- mean(abs(comparison$actual - comparison$forecast))
mse <- mean((comparison$actual - comparison$forecast)^2)
mape <- mean(abs((comparison$actual - comparison$forecast) / comparison$actual)) * 100
amape <- mean(abs((comparison$actual - comparison$forecast) / ((comparison$actual + comparison$forecast) / 2))) * 100

#Wyświetlenie wyników
cat("MAE:", mae, "\n")
cat("MSE:", mse, "\n")
cat("MAPE:", mape, "%\n")
cat("AMAPE:", amape, "%\n")
#bledy SES
#Tworzenie ramki danych z rzeczywistymi danymi i prognozami bez dodawania NA
n_forecasts <- length(ses_forecast$mean)
wpi_forecast <- data.frame(
  actual = ts_niesezonowy_c,
  forecast = c(rep(NA, length(ts_niesezonowy_c) - n_forecasts), ses_forecast$mean)
)
#Możesz teraz wyświetlić ramkę danych, aby upewnić się, że wszystko jest zgodne
print(wpi_forecast)
#Usunięcie wierszy z NA
comparison <- na.omit(wpi_forecast)

#Wyświetlenie porównania
print(comparison)

#Obliczenie błędów prognozy
mae <- mean(abs(comparison$actual - comparison$forecast))
mse <- mean((comparison$actual - comparison$forecast)^2)
mape <- mean(abs((comparison$actual - comparison$forecast) / comparison$actual)) * 100
amape <- mean(abs((comparison$actual - comparison$forecast) / ((comparison$actual + comparison$forecast) / 2))) * 100

#Wyświetlenie wyników
cat("MAE:", mae, "\n")
cat("MSE:", mse, "\n")
cat("MAPE:", mape, "%\n")
cat("AMAPE:", amape, "%\n")

##########################
###SZEREG SEZONOWY ###
##########################

sezonowy$Value <- sezonowy$Value/1000

ts_sezonowy <- ts(data=sezonowy$Value, start = c(2000, 1), end=c(2023, 12), frequency=12)
ts_sezonowy.xts = as.xts(ts_sezonowy)

ts_sezonowy_full = ts_sezonowy
ts_sezonowy_oos <- ts_sezonowy[277:288] #out of sample
ts_sezonowy_is <- ts_sezonowy[1:276] #in-sample

forecast::ggsubseriesplot(ts_sezonowy, ylab = "Sprzedaż detaliczna [mld $]", xlab = "Miesiąc")

plot(ts_sezonowy, 
     type='l', 
     col='black',
     xlab='Data',
     ylab='Sprzedaż detaliczna [mld $]',
     lwd=1)
#try with AIC etc
#information criteria
model_aic = auto.arima(ts_sezonowy_is,max.d=1,max.p=24,max.q=24,ic="aic",seasonal = FALSE,stepwise=FALSE, approximation=FALSE)
model_bic = auto.arima(ts_sezonowy_is,max.d=1,max.p=24,max.q=24,ic="bic",seasonal = FALSE,stepwise=FALSE, approximation=FALSE)

checkresiduals(model_aic)
checkresiduals(model_bic)

PACF <- pacf(dsez,
             na.action=na.pass,
             xlim=c(2,36),
             lag.max = 36,
             plot=TRUE)


#general to specific 


#różnice
dsez = diff.xts(ts_sezonowy_is, lag=1)
plot(dsez, type='l') #różnicowany regularnie

d12 = diff.xts(ts_sezonowy_is, lag=12) #różnicowany sezonowo
plot(d12, type='l')

d12dsez=diff.xts(dsez, lag=12) #różnicowany regularnie i sezonowo
plot(d12dsez, type='l')


par(mfrow = c(1, 2))
#ACF, PACF
ACF <- acf(lag12sez,
           lag.max = 36,
           xlim=c(2,36),
           na.action = na.pass,
           plot=TRUE)

ACF$lag <- ACF$lag * 12

plot(ACF,
   xlim=c(2,36),
   ylim=c(-0.9,0.9))

PACF <- pacf(lag12sez,
             na.action=na.pass,
             xlim=c(2,36),
             lag.max = 36,
             plot=TRUE)

#PACF$lag <- PACF$lag * 12

#plot(PACF,
#    xlim=c(1,36),
#    ylim=c(-0.9,0.9),
#    ylab='PACF')

library(tseries)
library(lmtest)

#Przeprowadzenie testu Dickeya-Fullera na różnicowanym szeregu czasowym
adf_test_result <- adf.test(ts_sezonowy_is)
print(adf_test_result)
model <- lm(ts_sezonowy_is ~ time(ts_sezonowy_is))

#Test Breuscha-Godfreya
bg_test_result <- bgtest(model, order = 4) #order = 4 dla miesięcznych danych (można dostosować)
print(bg_test_result)
#skorelowane reszty dla początkowej postaci szeregu

######ROZNICOWANIE REGULARNE
#Test Dickey-Haszy-Fullera

d12sez = diff.xts(ts_sezonowy_is, lag=1) #roznicowanie regularne (różnica pierwszego stopnia)
plot(d12sez, type='l')

adf_test_result <- adf.test(d12sez)
print(adf_test_result)
model <- lm(d12sez ~ time(d12sez))

######ROZNICOWANIE REGULARNE I SEZONOWE
lag12sez = diff.xts(d12sez, k=12) #Sezonowe różnicowanie na już zróżnicowanym szeregu (lag = 12)
model <- lm(lag12sez ~ time(lag12sez))

#Test Breuscha-Godfreya
bg_test_result <- bgtest(model, order = 4) #order = 4 dla miesięcznych danych (można dostosować)
print(bg_test_result)

#Przeprowadzenie rozszerzonego testu Dickeya-Fullera na różnicowanym szeregu czasowym
adf_test_result_urca <- ur.df(na.omit(lag12sez), type = "none", lags = 0)  #type = "none" dla braku trendu lub stałej, dostosuj lags
summary(adf_test_result_urca)
#skorelowane reszty dla pierwszych różnic

#Wyciągnięcie statystyki testu ADF i p-wartości
adf_stat <- summary(adf_test_result_urca)@teststat[1]
adf_p_value <- summary(adf_test_result_urca)@cval[1, "5pct"]

#AUTOSARIMA
sarima_model <- Arima(ts_sezonowy_is, order = c(1, 1, 3), seasonal = c(0, 1, 3))
#Podsumowanie modelu
summary(sarima_model)
#Diagnostyka reszt
checkresiduals(sarima_model)

#Test Breuscha-Godfreya
bg_test_results <- sapply(1:6, function(order) {
  bg_test <- bgtest(model, order = order)
  p_value <- bg_test$p.value
  return(p_value)
})

#Tworzenie wynikowej tabeli
results_table <- data.frame(
  adf_stat = adf_stat,
  p_adf = adf_p_value,
  `p_bg.p.value.1` = bg_test_results[1],
  `p_bg.p.value.2` = bg_test_results[2],
  `p_bg.p.value.3` = bg_test_results[3],
  `p_bg.p.value.4` = bg_test_results[4],
  `p_bg.p.value.5` = bg_test_results[5],
  `p_bg.p.value.6` = bg_test_results[6]
)

#Wyświetlenie tabeli wyników
print(results_table)

##########################
#logarytmowanie 

lds<-log(ts_sezonowy_is)
plot(lds,
     main = "lds",
     xlab = "data",
     ylab = "lds")

tsdisplay(ts_sezonowy_is, lag.max=36)
#ROZNICE REGULARNE
d_lds<-diff.xts(ts_sezonowy_is, lag = 1)
plot(d_lds, type = "l", col="blue", main = "Pierwsze różnice ds")

tsdisplay(d_lds, lag.max=36)
adf_test_result_urca <- ur.df(na.omit(d_lds), type = "none", lags = 0)  #type = "none" dla braku trendu lub stałej, dostosuj lags
summary(adf_test_result_urca)
#ROZNICE SEZONOWE I REGULARNE
D_lds<-diff.xts(d_lds, lag=12)

plot(D_lds, type = "l", col="blue", main = "Pierwsze różnice lds")

tsdisplay(D_lds, lag.max=36)

diflds <- diff.xts(ts_sezonowy_is, lag = 12)
laglds <- lag.xts(ts_sezonowy_is, k = 12)

plot(D_lds, type = "l")
adf_test_result_urca <- ur.df(na.omit(l), type = "none", lags = 0)  #type = "none" dla braku trendu lub stałej, dostosuj lags
summary(adf_test_result_urca)
######################
#test Dickey Haszy Fullera
air_sample.xts$d12lair <- diff.xts(air_sample.xts$lair, lag = 12)
air_sample.xts$lag12lair <- lag.xts(air_sample.xts$lair, k = 12)

plot(air_sample.xts$d12dlair)

model1=lm(d12lair~0+lag12lair, data=air_sample.xts)
summary(model1)
bg1 <- bgtest(model1, order = 1)
bg1
#############################
model1=lm(diflds~0+laglds)
summary(model1)
bg1 <- bgtest(model1, order = 1)
bg1
bg2 <- bgtest(model1, order = 2)
bg2
bg3 <- bgtest(model1, order = 3)
bg3
bg4 <- bgtest(model1, order = 4)
bg4

BG_dns <- matrix(c(bg1$p.value, bg1$statistic, bg2$p.value, bg2$statistic, bg3$p.value, bg3$statistic, bg4$p.value, bg4$statistic), ncol=2, byrow=TRUE)
colnames(BG_dns) <- c('p-value','Statystyka Testu B-G')
rownames(BG_dns) <- c(1,2,3,4)
as.table(BG_dns)

adf_test_result_urca <- ur.df(na.omit(lag4diflds), type = "none", lags = 0)  #type = "none" dla braku trendu lub stałej, dostosuj lags
summary(adf_test_result_urca)
#Test ADHF

#K=1
lagdiflds <- lag.xts(diflds, k = 1)

model2=lm(diflds~0+laglds+lagdiflds)
summary(model2)
bg1 <- bgtest(model2, order = 1)
bg1
bg2 <- bgtest(model2, order = 2)
bg2
bg3 <- bgtest(model2, order = 3)
bg3
bg4 <- bgtest(model2, order = 4)
bg4

BG_dns <- matrix(c(bg1$p.value, bg1$statistic, bg2$p.value, bg2$statistic, bg3$p.value, bg3$statistic, bg4$p.value, bg4$statistic), ncol=2, byrow=TRUE)
colnames(BG_dns) <- c('p-value','Statystyka Testu B-G')
rownames(BG_dns) <- c(1,2,3,4)
as.table(BG_dns)

options(scipen=4)
#K=2
lag2diflds <- lag.xts(diflds, k = 2)

model2=lm(diflds~0+laglds+lagdiflds+lag2diflds)
summary(model2)
bg1 <- bgtest(model2, order = 1)
bg1
bg2 <- bgtest(model2, order = 2)
bg2
bg3 <- bgtest(model2, order = 3)
bg3
bg4 <- bgtest(model2, order = 4)
bg4

BG_dns <- matrix(c(bg1$p.value, bg1$statistic, bg2$p.value, bg2$statistic, bg3$p.value, bg3$statistic, bg4$p.value, bg4$statistic), ncol=2, byrow=TRUE)
colnames(BG_dns) <- c('p-value','Statystyka Testu B-G')
rownames(BG_dns) <- c(1,2,3,4)
as.table(BG_dns)

#K=3
lag3diflds <- lag.xts(diflds, k = 3)
model3=lm(diflds~0+laglds+lagdiflds+lag2diflds+lag3diflds)
summary(model3)
bg1 <- bgtest(model3, order = 1)
bg1
bg2 <- bgtest(model3, order = 2)
bg2
bg3 <- bgtest(model3, order = 3)
bg3
bg4 <- bgtest(model3, order = 4)
bg4

BG_dns <- matrix(c(bg1$p.value, bg1$statistic, bg2$p.value, bg2$statistic, bg3$p.value, bg3$statistic, bg4$p.value, bg4$statistic), ncol=2, byrow=TRUE)
colnames(BG_dns) <- c('p-value','Statystyka Testu B-G')
rownames(BG_dns) <- c(1,2,3,4)
as.table(BG_dns)

#K=4
lag4diflds <- lag.xts(diflds, k = 4)
model4=lm(diflds~0+laglds+lagdiflds+lag2diflds+lag3diflds+lag4diflds)
summary(model4)
bg1 <- bgtest(model4, order = 1)
bg1
bg2 <- bgtest(model4, order = 2)
bg2
bg3 <- bgtest(model4, order = 3)
bg3
bg4 <- bgtest(model4, order = 4)
bg4
bg5 <- bgtest(model4, order = 5)
bg5

BG_dns <- matrix(c(bg1$p.value, bg1$statistic, bg2$p.value, bg2$statistic, bg3$p.value, bg3$statistic, bg4$p.value, bg4$statistic), ncol=2, byrow=TRUE)
colnames(BG_dns) <- c('p-value','Statystyka Testu B-G')
rownames(BG_dns) <- c(1,2,3,4)
as.table(BG_dns)


library(foreign)
library(xts)
library(urca)
library(lmtest)
install.packages("fBasics")
library(fBasics)
library(forecast)
library(tseries)
install.packages("fUnitRoots")
library(fUnitRoots)

source("funs/testdf.R")
#H0: zmienna d12lair jest zmienna niestacjonarna
testdf(variable = ts_sezonowy_is ,ADF_type="nc", ADF_max_order = 7, BG_max_order = 6)
#sezonowo
testdf(variable = laglds ,ADF_type="nc", ADF_max_order = 3, BG_max_order = 6)
#regularnie
testdf(variable = diflds ,ADF_type="nc", ADF_max_order = 3, BG_max_order = 6)
#sezonowo i regularnie
testdf(variable = D_lds ,ADF_type="nc", ADF_max_order = 3, BG_max_order = 6)

#KPSS

kpss.test <- ur.kpss(laglds, type = c("mu"))  #stała w równaniu testowym
summary(kpss.test)

#statystyka KPSS (0,9216) jest większa od 5% wartości krytycznej
#(0.463) za zatem odrzucamy H0 o stacjonarności

kpss.test.d <- ur.kpss(D_lds, type = c("mu"))
summary(kpss.test.d)
#statystyka KPSS (0,0148) jest mniejsza od 5% wartości krytycznej (0.463)
#a zatem nie możemy odrzucić H0 o stacjonarności pierwszych różnic.

#Instalacja i załadowanie pakietów
install.packages("forecast")
library(forecast)

#Generowanie przykładowych danych miesięcznych
set.seed(123)
data <- rnorm(120)  #10 lat miesięcznych danych
ts_sezonowy_is <- ts(data, frequency = 12)  #Tworzenie szeregu czasowego z sezonowością roczną

#Automatyczny dobór modelu SARIMA z uwzględnieniem sezonowości
auto_sarima_model <- auto.arima(ts_sezonowy_is, seasonal = TRUE, 
                                max.p = 5, max.q = 5, max.P = 2, max.Q = 2, 
                                d = NA, D = NA, 
                                stepwise = FALSE, approximation = FALSE)

#Wyświetlenie wyników modelu
summary(auto_sarima_model)

#Diagnostyka reszt
checkresiduals(auto_sarima_model)
#WHITE-NOISE #


par(mfrow = c(1, 2))
acf(D_lds,  lag.max = 36,
    lwd = 4,
    col = "red", na.action = na.pass)
pacf(D_lds, lag.max = 36,
     lwd = 4,
     col = "red", na.action = na.pass)
par(mfrow = c(1, 1))

library(tseries)
d12sez = diff(ts_sezonowy_is, lag=12)
plot(d12dsez, type='l')
testdf(d12dsez, 'nc', 3, 4)
kpss.test <- ur.kpss(d12, type=c('mu'))
summary(kpss.test)
#Test Ljung-Boxa
Box.test(D_lds, type = "Ljung-Box", lag = 36)

#Test Boxa-Pierce
Box.test(D_lds, type = "Box-Pierce", lag = 36)

arima.best.AIC <- auto.arima(ts_sezonowy_is,
                             d = 0,             #parameter d w modelu ARIMA
                             max.p = 4,         #p = maksymalna wartosc
                             max.q = 4,         #q = maksymalna wartosc
                             max.order = 8,     #suma p+q
                             start.p = 0,       #Wartosc startowa dla p
                             start.q = 0,       #Wartosc startowa dla q
                             ic = "aic",        #Wybor modelu na podstawie kryterium informcyjne
                             stepwise = FALSE,  #jezeli FALSE rozwaza wszystkie modeli
                             allowdrift = TRUE, #model zawiera stalą
                             trace = TRUE)      #wyswietlenie rozwazonych modeli


coeftest(arima.best.AIC)


####SARIMA estymacja czesci sezonowej
#ARIMA(0,1,0)(3,1,1)
arima010311 <- arima(ts_sezonowy_is,
                     #rzędy (p,d,q)
                     order = c(0, 1, 0),
                     #rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(3, 1, 1),
                                     #częstotliwość danych (12 dla danych miesięcznych)
                                     period = 12)       #dodatkowe regresory - stala
)
arima010311
#ARIMA(0,1,0)(2,1,1)
arima010211 <- arima(ts_sezonowy_is,
                     #rzędy (p,d,q)
                     order = c(0, 1, 0),
                     #rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(2, 1, 1),
                                     #częstotliwość danych (12 dla danych miesięcznych)
                                     period = 12)       #dodatkowe regresory - stala
)
arima010211
coeftest(arima010211) #MA(1) ist
#general to specific 
lgen = logLik(arima010311)
lspec =logLik(arima010211)
dfs <- length(coef(model_general)) - length(coef(model_specific))
teststat<--2*(as.numeric(lspec-lgen))
pchisq(teststat,df=dfs,lower.tail=FALSE)
#brak podstaw do odrzucenia H0
#brak podstaw do odrzucenia H0 #ARIMA(0,1,0)(2,1,1) LEPSZY

#ARIMA(0,1,0)(1,1,1)
arima010111 <- arima(ts_sezonowy_is,
                     #rzędy (p,d,q)
                     order = c(0, 1, 0),
                     #rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(1, 1, 1),
                                     #częstotliwość danych (12 dla danych miesięcznych)
                                     period = 12)       #dodatkowe regresory - stala
)
arima010111
coeftest(arima010111) #AR(1) nieist, MA(1) IST
#general to specific 
lgen = logLik(arima010211)
lspec =logLik(arima010111)
dfs <- length(coef(model_general)) - length(coef(model_specific))
teststat<--2*(as.numeric(lspec-lgen))
pchisq(teststat,df=dfs,lower.tail=FALSE)
#brak podstaw do odrzucenia H0 #ARIMA(0,1,0)(1,1,1) LEPSZY

#przetestuję jeszcze (1,1,0)
#ARIMA(0,1,0)(1,1,0)
arima010110 <- arima(ts_sezonowy_is,
                     #rzędy (p,d,q)
                     order = c(0, 1, 0),
                     #rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(1, 1, 0),
                                     #częstotliwość danych (12 dla danych miesięcznych)
                                     period = 12)       #dodatkowe regresory - stala
)
arima010110
coeftest(arima010110)
#general to specific 
lgen = logLik(arima010111)
lspec =logLik(arima010110)
dfs <- length(coef(model_general)) - length(coef(model_specific))
teststat<--2*(as.numeric(lspec-lgen))
pchisq(teststat,df=dfs,lower.tail=FALSE)
#odrzucamy H0 o tym, że proszty model jest lepszy (1,1,1)

#przetestuję jeszcze (0,1,1)
#ARIMA(0,1,0)(0,1,1)
arima010011 <- arima(ts_sezonowy_is,
                     #rzędy (p,d,q)
                     order = c(0, 1, 0),
                     #rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),
                                     #częstotliwość danych (12 dla danych miesięcznych)
                                     period = 12)       #dodatkowe regresory - stala
)
arima010011
coeftest(arima010011)
#general to specific 
lgen = logLik(arima010111)
lspec =logLik(arima010011)
dfs <- length(coef(model_general)) - length(coef(model_specific))
teststat<--2*(as.numeric(lspec-lgen))
pchisq(teststat,df=dfs,lower.tail=FALSE)
#brak podstaw do odrzucenia H0 -> (0,1,1)

#zostaje sezonowy komponent (0,1,1)

#przetestuję jeszcze (0,1,0)
#ARIMA(0,1,0)(0,1,0)
arima010010 <- arima(ts_sezonowy_is,
                     #rzędy (p,d,q)
                     order = c(0, 1, 0),
                     #rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 0),
                                     #częstotliwość danych (12 dla danych miesięcznych)
                                     period = 12)       #dodatkowe regresory - stala
)
arima010010

#general to specific 
lgen = logLik(arima010011)
lspec =logLik(arima010010)
dfs <- length(coef(model_general)) - length(coef(model_specific))
teststat<--2*(as.numeric(lspec-lgen))
pchisq(teststat,df=dfs,lower.tail=FALSE)
#ODRZUCAMY H0, zostaje (0,1,1)

AIC(arima010311, arima010211, arima010111, arima010011, arima010010)
#czesc sezonowa SARIMA(0,1,0)(0,1,1) - AIC
#wartosci BIC
BIC(arima010311, arima010211, arima010111, arima010011, arima010010) 

#SARIMA czesc regularna
#1,1,3
arima113011 <- arima(ts_sezonowy_is,
                     #rzędy (p,d,q)
                     order = c(1, 1, 3),
                     #rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),
                                     #częstotliwość danych (12 dla danych miesięcznych)
                                     period = 12)
)

arima113011
coeftest(arima113011) #tylko MA(2)

Box.test(resid(arima113011), type = "Ljung-Box", lag = 36)
Box.test(resid(arima113011), type = "Box-Pierce", lag = 36)
#0,1,3
arima013011 <- arima(ts_sezonowy_is,
                     #rzędy (p,d,q)
                     order = c(0, 1, 3),
                     #rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),
                                     #częstotliwość danych (12 dla danych miesięcznych)
                                     period = 12)
)

arima013011
coeftest(arima013011) #ma(2), MA(3)
Box.test(resid(arima013011), type = "Ljung-Box", lag = 36)
Box.test(resid(arima013011), type = "Box-Pierce", lag = 36)
#general to specific 
lgen = logLik(arima113011)
lspec =logLik(arima013011)
dfs <- length(coef(model_general)) - length(coef(model_specific))
teststat<--2*(as.numeric(lspec-lgen))
pchisq(teststat,df=dfs,lower.tail=FALSE)
#brak podtsaw do odrzucenia H0, mamy (0,1,3)

#0,1,2
arima012011 <- arima(ts_sezonowy_is,
                     #rzędy (p,d,q)
                     order = c(0, 1, 2),
                     #rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),
                                     #częstotliwość danych (12 dla danych miesięcznych)
                                     period = 12)
)

arima012011
coeftest(arima012011) #ma(2)

Box.test(resid(arima012011), type = "Ljung-Box", lag = 36)
Box.test(resid(arima012011), type = "Box-Pierce", lag = 36)
#general to specific 
lgen = logLik(arima013011)
lspec =logLik(arima012011)
dfs <- length(coef(model_general)) - length(coef(model_specific))
teststat<--2*(as.numeric(lspec-lgen))
pchisq(teststat,df=dfs,lower.tail=FALSE)
#ODRUCAMY H0 - prostszy nie jest lepszy, zostaje (0,1,3)

#Mamy więc (0,1,3)(0,1,1)12
par(mfrow = c(2, 1))
acf(resid(arima013011), lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36), lwd = 4, col = "red")
pacf(resid(arima013011), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))

Box.test(resid(arima013011), type = "Ljung-Box", lag = 36)
AIC(arima113011, arima013011, arima012011)
BIC(arima113011, arima013011, arima012011) 


#SARIMA PROGNOZA


forecast110 <- predict(arima013011, n.ahead = 12)

forecast110
str(forecast110)

plot(ts_sezonowy_full,
     xlim=c(260, 288),
     ylab = "Sprzedaż detaliczna [mld $]",
     xlab='Data')
#pocztek okresu prognozy
abline(v = 276, lty=2)
lines(forecast110$pred, col = "red", lwd = 2)
lines(forecast110$pred + 2 * forecast110$se, col = "red", lty = 3)
lines(forecast110$pred - 2 * forecast110$se, col = "red", lty = 3)

#bledy prognozy SARIMA(0,1,3)(0,1,1)
n_forecasts <- length(forecast110$pred)
wpi_forecast <- data.frame(
  actual = ts_sezonowy_full,
  forecast = c(rep(NA, length(ts_sezonowy_full) - n_forecasts), forecast110$pred)
)
#Możesz teraz wyświetlić ramkę danych, aby upewnić się, że wszystko jest zgodne
print(wpi_forecast)
#Usunięcie wierszy z NA
comparison <- na.omit(wpi_forecast)

#Wyświetlenie porównania
print(comparison)

#Obliczenie błędów prognozy
mae <- mean(abs(comparison$actual - comparison$forecast))
mse <- mean((comparison$actual - comparison$forecast)^2)
mape <- mean(abs((comparison$actual - comparison$forecast) / comparison$actual)) * 100
amape <- mean(abs((comparison$actual - comparison$forecast) / ((comparison$actual + comparison$forecast) / 2))) * 100

#Wyświetlenie wyników
cat("MAE:", mae, "\n")
cat("MSE:", mse, "\n")
cat("MAPE:", mape, "%\n")
cat("AMAPE:", amape, "%\n")
#ekstrapolacyjny
#addytywny HW
HWadd <- HoltWinters(ts(ts_sezonowy_is, frequency = 12, start = c(2000, 1)),seasonal = "additive")
HWadd.forecast <- predict(HWadd,n.ahead = 12,prediction.interval = TRUE)
plot(HWadd, lwd=2)
HWadd
ts(ts_sezonowy_is, frequency = 12, start = c(2000, 1))

plot(ts_sezonowy_full,
     xlim=c(2016, 2024.12),
     ylab = "Sprzedaż detaliczna [mld $]",
     xlab='Data')
lines(HWadd.forecast[, 1], col = "blue") #prognoza
lines(HWadd.forecast[, 2], col = "blue", lty = 2) #dolna granica przedziału ufności
lines(HWadd.forecast[, 3], col = "blue", lty = 2) #górna granica przedziału ufności
abline(v = c(2023, 01), lty=2)

#Tworzenie ramki danych z rzeczywistymi danymi i prognozami bez dodawania NA
n_forecasts <- length(HWadd.forecast[,1])
wpi_forecast <- data.frame(
  actual = ts_sezonowy_full,
  forecast = c(rep(NA, length(ts_sezonowy_full) - n_forecasts), HWadd.forecast[,1])
)
#Możesz teraz wyświetlić ramkę danych, aby upewnić się, że wszystko jest zgodne
print(wpi_forecast)
#Usunięcie wierszy z NA
comparison <- na.omit(wpi_forecast)

#Wyświetlenie porównania
print(comparison)

#Obliczenie błędów prognozy
mae <- mean(abs(comparison$actual - comparison$forecast))
mse <- mean((comparison$actual - comparison$forecast)^2)
mape <- mean(abs((comparison$actual - comparison$forecast) / comparison$actual)) * 100
amape <- mean(abs((comparison$actual - comparison$forecast) / ((comparison$actual + comparison$forecast) / 2))) * 100

#Wyświetlenie wyników
cat("MAE:", mae, "\n")
cat("MSE:", mse, "\n")
cat("MAPE:", mape, "%\n")
cat("AMAPE:", amape, "%\n")

#mltiplikatywny HW
HWadd1 <- HoltWinters(ts(ts_sezonowy_is, frequency = 12, start = c(2000, 1)),seasonal = "multiplicative")
HWadd1.forecast <- predict(HWadd1,n.ahead = 12,prediction.interval = TRUE)
plot(HWadd1, lwd=2)
HWadd1
ts(ts_sezonowy_is, frequency = 12, start = c(2000, 1))

plot(ts_sezonowy_full,
     xlim=c(2016, 2024.12),
     ylab = "Sprzedaż detaliczna [mld $]",
     xlab='Data')
lines(HWadd1.forecast[, 1], col = "green") #prognoza
lines(HWadd1.forecast[, 2], col = "green", lty = 2) #dolna granica przedziału ufności
lines(HWadd1.forecast[, 3], col = "green", lty = 2) #górna granica przedziału ufności
abline(v = c(2023, 01), lty=2)

#Tworzenie ramki danych z rzeczywistymi danymi i prognozami bez dodawania NA
n_forecasts <- length(HWadd1.forecast[,1])
wpi_forecast <- data.frame(
  actual = ts_sezonowy_full,
  forecast = c(rep(NA, length(ts_sezonowy_full) - n_forecasts), HWadd1.forecast[,1])
)
#Możesz teraz wyświetlić ramkę danych, aby upewnić się, że wszystko jest zgodne
print(wpi_forecast)
#Usunięcie wierszy z NA
comparison <- na.omit(wpi_forecast)

#Wyświetlenie porównania
print(comparison)

#Obliczenie błędów prognozy
mae <- mean(abs(comparison$actual - comparison$forecast))
mse <- mean((comparison$actual - comparison$forecast)^2)
mape <- mean(abs((comparison$actual - comparison$forecast) / comparison$actual)) * 100
amape <- mean(abs((comparison$actual - comparison$forecast) / ((comparison$actual + comparison$forecast) / 2))) * 100

#Wyświetlenie wyników
cat("MAE:", mae, "\n")
cat("MSE:", mse, "\n")
cat("MAPE:", mape, "%\n")
cat("AMAPE:", amape, "%\n")



arima212011graph <- arima(ts(ts_sezonowy_is, frequency = 12, start = c(2000, 1)),
                          #rzędy (p,d,q)
                          order = c(0, 1, 3),
                          #rzędy sezonowe (P,D,Q)
                          seasonal = list(order = c(0, 1, 1),
                                          #częstotliwość danych (12 dla danych miesięcznych)
                                          period = 12)
)
forecast212 <- predict(arima212011graph, n.ahead = 12)

plot(ts_sezonowy_full, xlim=c(2016, 2024.12),
     ylab = "Sprzedaż detaliczna [mld $]",
     xlab='Data')
abline(v = c(2023,01), lty = 2, col = "gray")
lines(forecast212$pred, col = "red", lwd = 2)
lines(forecast212$pred + 2 * forecast212$se, col = "red", lty = 3)
lines(forecast212$pred - 2 * forecast212$se, col = "red", lty = 3)
lines(HWadd.forecast[, 1], col = "blue")
lines(HWadd.forecast[, 2], col = "blue", lty = 2)
lines(HWadd.forecast[, 3], col = "blue", lty = 2)
legend("topleft", legend = c("SARIMA(0,1,3)(0,1,1)", "Holt-Winters"), col = c("red", "blue"), lty = 1, lwd = 2, cex = 0.8)


