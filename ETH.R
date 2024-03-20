rm(list=ls())
library(zoo)
library(tseries)
library(ggplot2)
library(moments)

filename <- "https://stooq.pl/q/d/l/?s=eth.v&i=d" 
dane <- read.csv(filename) 
dane <- dane[,c(1,5)] #wybieram kolumny z datą (1) i ceną zamknięcia (5)
plot(dane)

dane_cleaned <- subset(dane, Data >= as.Date("2019-03-20"))

ETH <- as.numeric(dane_cleaned$Zamkniecie)
data <- as.Date(dane_cleaned$Data, format="%Y-%m-%d")

ETH <- zoo(ETH, order.by = data)

#plot(ETH, type = "l", col = "blue", lwd = 2, main = "Wykres cen Ethereum", xlab = "Data", ylab = "Cena (USD)")
#grid()
#legend("topleft", legend = "ETH", col = "blue", lty = 1, lwd = 2, cex = 0.8, inset = 0.05)

ETH_logdiff <- diff(log(ETH))

#plot(ETH_logdiff, type = "l", xlab = "Data", ylab = "Dzienna stopa zwrotu", main = "Wykres dziennych stóp zwrotu Ethereum")

r  <- ETH_logdiff

par(mfrow=c(2,1), cex = 0.7, bty="l")
plot(window(ETH), main="Wykres cen Ethereum", xlab="Data", ylab="Cena (USD)")
plot(r, main="Dzienne logarytmiczne stopy zwrotu", xlab="", ylab="")

M1   = moment(r, order = 1, central = FALSE, na.rm = TRUE)
M2   = moment(r, order = 2, central = TRUE, na.rm = TRUE)
M3   = moment(r, order = 3, central = TRUE, na.rm = TRUE)
M4   = moment(r, order = 4, central = TRUE, na.rm = TRUE)


mu  = M1; mu               #średnia
sig = sqrt(M2); sig         #odchylenie std. (miara zmienności)
S0   = M3/(sig^3); S0      #skośność
K0   = M4/(sig^4); K0      #kurtoza

#par(mfrow=c(1,1))
#hist(r, breaks = 100, freq = FALSE, main = "Histogram logarytmicznych stóp zwrotu z Ethereum", xlab = "Wartość zmiennej r", ylab = "Gęstość rozkładu")

ggplot(data.frame(r), aes(x = r)) +
  theme_bw() +
  geom_density(colour = "darkblue", fill = "lightblue", size = 0.5) +
  stat_function(fun = function(x) dnorm(x, mean = mu, sd = sig), color = "darkred", size = 0.8, alpha = 0.7) +
  labs(x = "Wartości logarytmicznych stóp zwrotu", y = "Gęstość") 

par(mfrow=c(1,1))
BP <- boxplot(r*100, range = 3,
              main = "Dzienne logarytmiczne stopy zwrotu z ETH",
              xlab = "Stopa zwrotu (%)",
              ylab = "",
              col = "orange",
              border = "black",
              horizontal = TRUE,
              notch = FALSE
)
BP$out
liczba_odstajacych <- length(BP$out)
print(liczba_odstajacych)

