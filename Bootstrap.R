install.packages("Rmisc")
install.packages("ggpubr")
install.packages("boot")
library(MASS)
library(nortest)
library(fBasics)
library(Rmisc)
library(tidyverse)
library(purrr)
library(ggpubr)
library(boot)

#Przyk³ad 1

Pima.te

dane <- Pima.te$age

summarySE(data =pima, measurevar = "age") #Podstawowy rozk³ad statystyk - sd- rozklad zmiany sredniej

str(pima)

summarySE(data =pima, measurevar = "age",
          groupvars = "type",
          conf.interval = 0.99) %>%
  mutate(dp = age - ci,
         gp = age + ci) %>%
  knitr::kable(, digits = 2)

colnames(pima)

pima1 <- pima %>%
  gather(key = "param",
         value = "obs", -type ) %>% as.tibble()

summarySE(data = pima1,
          measurevar = "obs",
          groupvars = c("param", "type")) %>%
  knitr::kable(digits = 2)

#BOOTsTRAP

n     <-  10
mu    <-  40.
sigma <- 5.
alpha <- 0.05

#Obliczenia 

se    <- sigma/sqrt(n)                       # b³¹d standardowy
ci_t  <- qt(p = 1-alpha/2, df = 10-1) * se   # przedzia³ ufnosci dla n < 30

#Podsumowanie statystyk - co jest co - skrypt w plikach 

c(n = n, mu = mu, sigma = sigma, se = se, ci_t = ci_t) %>%  
  knitr::kable(., digits = 3.,col.names = "Value")

c(lower = mu-ci_t, 
  mu= mu, 
  upper = mu + ci_t) %>% 
  knitr::kable(., digits = 3, col.names = "value")

set.seed(133)
dane <- round(rnorm(n = 10, mean = 40, sd = 5),2)
set.seed(133)
losowanie <- lapply(1:200, function(i) {sample(dane, replace = T)})

los_mean <- sapply(losowanie, mean)
los_mean

sd(los_mean)

ggplot() + geom_histogram(aes(x = los_mean), bins = 8) 

ggplot() + geom_density(aes(x = los_mean)) 

shapiro.test(los_mean)

ci <- round(quantile(los_mean, probs = c(0.025,0.975)),2)

c(lower = ci[1], mean = mean(dane), upper = ci[2]) %>% knitr::kable(., digits = 2)
ggplot(data = NULL) +
  geom_point(aes(x = 1, y = mean(dane))) + 
  geom_errorbar(aes(x =1, ymin = ci[1], ymax = ci[2]), width = 0.1) +
  xlim(0.0,2) + ylim(30,50)+theme_bw() + coord_fixed(0.3) + ylab("Wartoœæ")


#Piszemy funkcje przy zdefiniowanym obiekcie wejsciowym


mediana <- function(d, i) {
  median(d[i])
}

my.boot <- boot(data = dane, 
                statistic = mediana, 
                R = 1000)

my.boot

my.boot$t
my.boot$t0
my.boot$R
my.boot$seed

str(my.boot)

median(dane)

mean(my.boot$t) - my.boot$t0

sd(my.boot$t)
hist(my.boot)


#Estymacja przedzia³ów ufnosci na podstawie rozk³adów naszej zmiennej

my.ci <- boot.ci(boot.out = my.boot,
                 conf = 0.95, type = c("basic", "perc", "norm"))

my.ci
my.boot$t0

fun_r <- function(d, i, x, y, metoda) {
  d2 <- d[i,]
  cor(d2[,x], d2[,y], method = metoda)
}

set.seed(1786)
my.boot <- boot(data = pima, 
                statistic = fun_r,
                R = 1000, 
                x = "glu", y = "skin", metoda = "pearson") # argumenty funkcji fun_r

my.boot

ggqqplot(my.boot$t)
hist(my.boot$t)

my.ci <- boot.ci(boot.out = my.boot,
                 conf = 0.95, type = c("basic", "perc", "norm"))

c(lower = my.ci$basic[4], 
  wsp_kor = my.boot$t0, 
  upper = ci_r$basic[5]) %>% 
  knitr::kable(., digits = 2)



airquality %>% as_tibble()

airquality$Month%>% unique()

aqm <- airquality %>% 
  dplyr::select(Month, Day, Ozone) %>%
  na.omit() %>% 
  mutate(Ozone2 = Ozone + rnorm(116, 4, 2))

aqm <- as_data_frame(aqm)

fun_ci_r <- function(data, x, y, R, metoda){
  
  my.boot <- boot(data = data, 
                  statistic = fun_r,
                  R = R, 
                  x = x, y = y, metoda = metoda)
  
  ci_r <- boot.ci(boot.out = my.boot, conf = 0.95)
  
  out <- c(lower = ci_r$basic[2], 
           wsp_kor = my.boot$t0, 
           upper = ci_r$basic[3])
  
  return(out)  
}

View(aqm)

fun_ci_r(data  = aqm,
         x  = aqm$Day,
         y = aqm$Ozone,
         R = 1000, 
         metoda = "pearson")

fun_r <- function(d, i, x, y) {
  d2 <- d[i,]
  mean(d2[,x]- d2[,y])
}