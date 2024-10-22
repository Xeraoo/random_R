load("AQM_krakow.Rdata")
library(tidyverse)

mpg

mpg %>% glimpse()

ggplot(data = mpg, mapping =aes(x = class, y=hwy))

range(mpg$displ)
range(mpg$hwy)

ggplot(data = mpg, mapping = aes(x= displ, y = hwy)) + geom_point()+
  geom_smooth()

mpg %>% 
  ggplot()+
  geom_point(aes(x=displ, y= hwy), color = "blue") +
  geom_smooth(aes(x=displ, y= cty), color = "red")+
  geom_smooth(aes(x=displ, y = hwy))+
  geom_smooth(aes(x=displ, y= cty, color= "yellow"))

              
mpg %>% 
  ggplot(aes(x= displ, y = hwy))+ 
  geom_point(aes(color = class, shape= class))+
  geom_smooth()

ggplot(data= mpg, mapping = aes(x=displ, y=hwy)) + geom_point()

ggplot(data=mpg, mapping = aes(x=cyl, y=cty))

mpg %>% 
  ggplot(aes(x= cyl, y = cty)) + 
  geom_point(aes(color = class, shape= class)) +
  geom_smooth()


mpg %>% 
  ggplot(aes(x= cyl, y = cty)) + 
  geom_point(aes(color = class, shape = class)) +
  geom_smooth()

mpg %>% 
  ggplot(aes(x= cyl, y = cty)) + 
  geom_point(aes(size = cyl), color = "yellow") +
  geom_smooth()

mpg %>% 
  ggplot(aes(x= cyl, y = cty)) + 
  geom_point(aes(color = class), shape = 23 ) +
  geom_smooth()

 p <-   mpg %>% 
  mutate(cyl = as.character(cyl)) %>% 
  ggplot(aes(x = displ, y = hwy)) + 
  geom_point(aes(size = cyl), color = "yellow") +
  geom_smooth() +
  labs(tag = "1)",
       title = "Mój Tytul Wykresu", subtitle = "podtytul wykresu",
       caption = "Przypis dolny" , x = "Pojemnosc samochodu", y = "Spalanie na autostradzie [1l/100km]",
       size = "Liczba cylindrow", color = "Liczba cylindrow", shape = "Rodzaj napedu") +
  xlim(4,7) + ylim(10,30) +
   theme(legend.positin = "top")

p + facet_wrap(~fl, scales = "free")       
p + facet_grid(~fl)   
p
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

p+theme_bw()
p+theme_void()

p

install.packages("ggthemes")
library(ggthemes)

p+theme_dark()
p+theme_excel()
p+theme_economist()

p + labs(title = "Tytul wykrsu") +
  theme(title = element_text(face = "bold",
                             color = "brown",
                             size = 12),
        plot.title =         )

meteo %>% ggplot(aes(air_temp)) +
  geom_histogram()
