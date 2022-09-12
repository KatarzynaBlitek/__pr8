#KGH


dane <- read.csv("C:/Users/katar/OneDrive/Pulpit/IMRR3/kgh_d.csv", row.names=1)


# Obliczenie stop zwrotu (prostych)
dane$stopa_zwrotu <- NA
dane$stopa_zwrotu <- (dane$Zamkniecie-dane$Otwarcie)/dane$Otwarcie

## METODA HISTORYCZNA ------------------------------------------------------------------------------------------------
#--500 OBSERWACJI NA SUWAKU---------------------------------------------
library(dplyr)

t = 500
dane$met_historyczna_var99_500obs <- NA
dane$met_historyczna_es99_500obs <- NA
iloscWierszy <- nrow(dane)
max = iloscWierszy-t

for (i in c(1:max)){
  #VaR
  fragmentDanych <- dane[i:(t+i-1),]
  kw_99_H = quantile(fragmentDanych$stopa_zwrotu, 0.99)
  dane$met_historyczna_var99_500obs[t+i] <- kw_99_H
  
  #ES
  fragmentDanych2 <- fragmentDanych %>% filter(stopa_zwrotu > kw_99_H)
  es_99_H = mean(fragmentDanych2$stopa_zwrotu)
  dane$met_historyczna_es99_500obs[t+i] <- es_99_H
}

plot(dane$met_historyczna_var99_500obs, type = "l", ylab = "VaR 99%", main = "Jak zmienialo sie VaR w czasie - metoda historyczna")

es_99_H

#MONTE CARLO--------------------------------------------------------------------------------------------------------

t = 500
dane$monteCarlo_var99 <- NA
dane$monteCarlo_es99 <- NA

iloscWierszy <- nrow(dane)
max = iloscWierszy-t

for (i in c(1:max)){
  #VaR
  fragmentDanych <- dane[i:(t+i-1),]
  fragmentDanychh <- rnorm(t,mean = mean(fragmentDanych$stopa_zwrotu),sd=sd(fragmentDanych$stopa_zwrotu))
  kw_99_MC = quantile(fragmentDanychh, 0.99)
  dane$monteCarlo_var99[t+i] <- kw_99_MC
  
  #ES
  fragmentDanych2 <- fragmentDanych %>% filter(stopa_zwrotu > kw_99_MC)
  es_99_MC = mean(fragmentDanych2$stopa_zwrotu)
  dane$monteCarlo_es99[t+i] <- es_99_MC
}


plot(dane$monteCarlo_var99,type = "l", ylab = "VaR 99%", main = "Jak zmienialo sie VaR w czasie - metoda Monte Carlo")

es_99_MC


