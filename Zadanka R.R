#-------------- Zad 1-----------------
#Wektor typu numeric skadajacy sie z 20 elementow
wek1 <- c(1:20)
class(wek1)
typeof(wek1)

#Wektor typu numeric zawierajacy sie miedzy 20 a 63 skladajacy sie z 20 elementow
wek2 <- seq(20, 63, length.out = 20)
wek2
class(wek2)
#Wektor 4 unikalne wartosi tekstowe skadajacym sie z 20 elementów
wektor_elementy <- c("a", "b","c","d")
wek3 <- rep(wektor_elementy, each = 5)
wek3
typeof(wek3)
#Wektor wartosci logicznych, skladajacy sie z 8 wartosi T, 8 Wartosci F i dwoch NA
wek4 <- c("T","T","T","T","T","T","T","T",
          "F","F","F","F","F","F","F","F",
          "NA","NA")
wek4

#-------------ZADANIE 2---------------
getwd()
setwd("C:..........")
getwd()
dir()

dane <- read.table(file = "Z:/R/Kurs_R/Temat_2_prog//dane1_srednik.txt", # lokalizacja i nazwa pliku
                   header = TRUE, # czy zawiera nazwy kolumn
                   sep = ";", # separtor wartosci
                   dec=".") # separator miejsca dzisiętnego
dane 
View(dane)
#---------------ZADANIE 3-----------

#------------ZADANIE 6-------------
library(tidyverse)
install.packages("ggthemes")
library(ggthemes)

ggplot(data = dane, mapping =aes(x = waga, y=wzrost))

ggplot(data = dane, mapping = aes(x= waga, y = wzrost))+
  geom_point() + geom_smooth()

q <- ggplot(data = mpg, mapping = aes(x= displ, y = hwy)) + 
  geom_point(aes(size = cyl), color = "red")+
  geom_point(aes(color = class), shape = 23 ) +
  geom_smooth()+ 
  labs(tag = "1)",
       title = "Wykres GGPLOT2", subtitle = "Kolokwium",
       caption = "Przypis dolny" , x = "Waga", y = "Wzrost",
       size = "....")

q
q + theme_dark()
q + facet_grid(~fl)
q + facet_wrap(~fl)
q + theme(title = element_text(face = "bold",
                               color = "Green",
                               size = 12))
q + theme(plot.background = element_rect(fill = "turquoise",
                                         color = "blue",
                                         size = 3, 
                                         linetype = 5))

q + theme(axis.line = element_line(colour = "black", 
                                   size =2, 
                                   linetype = 6, lineend = 7))
q + theme(legend.position = "bottom", 
          legend.direction = "horizontal")
#------------------ Wykres pelna modyfikacja u Gory------------
#---------------Podstawowy histogram------------------
meteo %>% ggplot(aes(air_temp)) +
  geom_histogram()


