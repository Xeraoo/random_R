library(tidyverse)
library(Rmisc)
library(boot)
library(nortest)
library(fBasics)
library(purrr)
library(ggpubr)
library(MASS)

dane_2019 <- dane_stacje_Krakow
dane_2019<-na.omit(dane_2019)
dane_2019 <-as.data.frame(dane_2019)
str(dane_2019)
#usunac NA z tych danych
sred <- function(d, i, x, y) {
  d2 <- d[i,]
  ((d2[,x]-d2[,y]))
}

my.boot <- boot(data = dane_2019, 
                 statistic = sred, 
                 R = 1000,
                 x="MpKrakAlKras", y="MpKrakBujaka")

my.boot
my.boot$t0
str(my.boot)
hist(my.boot)
mean(dane_2019$MpKrakAlKras)
mean(dane_2019$MpKrakBujaka)
hist(my.boot$t)
my.ci <- boot.ci(boot.out = my.boot,
                 conf = 0.95, type = c("basic", "perc"))
my.ci

ggqqplot(my.boot$t0)

#adibata -szczegolowo
#rozchodzenie dzwieku w tubie (chyba ze miana chemiczna czy cos taiego)
#rozchodzenie fal jakis chyba przyklad struna jak sie rozchodza fale
