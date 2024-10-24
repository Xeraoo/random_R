##Stary: SlCzestCzes_baczy Nowy: SlCzestoBacz Start: 01.01.1992

install.packages("glue")
install.packages("devtools")
library(devtools)
install.packages("doMC", repos="http://R-Forge.R-project.org")
install_github("skgrange/enlightenr")
install_github("skgrange/normalweatherr")

#########################

library(tidyverse)
library(worldmet)
library(leaflet)
library(corrplot)
library(Hmisc)
library(caret)
library(parallel)
library(doParallel)
library(lubridate)
library(ggplot2)
library(ggpmisc)

load(file = "projekt_2.RData")

czestochowa_PM10 <- PM10_1h %>%
  filter(kod=="SlCzestCzes_baczy" | kod=="SlCzestoBacz") %>% 
  select(date, obs) 

###################

gios_inv %>% filter(Kod.stacji == "SlCzestoBacz") -> czesto_lok

czesto_lok_info <- paste(paste(
  czesto_lok$Kod.stacji,
  paste("Miejscowo��:", czesto_lok$Miejscowosc),
  paste("Data uruchomienia:", czesto_lok$Data.uruchomienia),
  paste("Data zamkni�cia:", czesto_lok$Data.zamkniecia),
  paste("Typ stacji:", czesto_lok$Typ.stacji),
  paste("Typ obszaru:", czesto_lok$Typ.obszaru),
  paste("Wsp�rz�dne:", czesto_lok$Dlugosc,"E, ", czesto_lok$Szerokosc,"N"),
  sep = "<br/>"
))


leaflet() %>%
  addTiles() %>%
  addMarkers(data=czesto_lok,
             lng= ~ Dlugosc,
             lat= ~ Szerokosc,
             popup = czesto_lok_info)

###################

noaa_isd <- getMeta(end.year="current", lon=19.130111, lat=50.836389, returnMap=F)

###################


## Pobieranie danych meteorologicznych dla wybranych lat i stacji Katowice
czestochowa_met <- importNOAA(code="125600-99999", year=2000:2020)

## Musimy doda� godzin� w danych meteo, aby uwzgl�dni� r�ne strefy czasowe danych
czestochowa_met$date <- czestochowa_met$date + 3600

## selekcja wybranych zmiennych  

czestochowa_met <- czestochowa_met[c(3, 7:14)]

## ��czenie danych meteorologicznych z danymi jako�ci powietrza
dane <- inner_join(czestochowa_met, czestochowa_PM10, by = "date")

###############
# Sprawdzenie brak�w danych i poszczeg�lnych kolumn
summary(dane)

# Selekcja tylko kompletnych wierszy
dane_rf <- dane_rf[complete.cases(dane_rf),]

# Policzenie ilo�ci pomiar�w w poszczeg�lnych latach
dane_rf$years <- format(dane_rf$date, "%Y")

dane_rf %>%
  group_by(years) %>%
  summarise(liczba_pomiarow = n()) -> dane_rf_summary

ggplot(dane_rf_summary, aes(x=years, y=liczba_pomiarow, color=liczba_pomiarow)) +
  geom_point(size=3, show.legend = FALSE) +
  theme_minimal() +
  coord_flip()

###############

# Filtrujemy dane do zbioru treningowego i testowego
dane_rf %>%
  filter(years>2015 & years<2018) %>%
  select(-date, -years) -> dane_rf_train

dane_rf %>%
  filter(years==2018) %>%
  select(-years) -> dane_rf_test

################

# Zapisanie kopii obiektu, tak aby nie zmienia� nazw kolumn w oryginalnym zestawie danych
dane_rfc <- dane_rf_train

# Definiowanie indywidualnej palety kolor�w w zale�no�ci od wsp�czynnika korelacji (kodowanie hex)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

# Policzenie odpowiednich wsp�czynnik�w za pomoc� funkcji 'corr'
corr <- rcorr(as.matrix(dane_rfc))

# Zdefiniowanie wektora ze wsp�czynnikami korelacji
corr_r <- corr$r

# zdefiniowanie wektora z warto�ciami p-value
corr_p <- corr$P

# Wykre�lenie macierzy korelacji
corrplot(corr_r, method = "color", col = col(200),  
         type = "upper", order = "hclust", addCoef.col = "black", 
         diag = FALSE,
         tl.col = "black", 
         tl.srt = 45, 
         p.mat = corr_p, 
         sig.level = 0.05)

###############
# Ustawiamy metod� tzw. "kroswalidacji", czyli samo-testowania si� modelu w trakcie jego budowy.
cross.walid <- trainControl(method = "cv",
                            number = 5,
                            allowParallel = T,
                            returnResamp = 'all')

# Zadajemy, aby w modelu by�y obecne wszystkie zmienne opr�cz obja�nianej (PM10)
tunegrid <- expand.grid(.mtry = c(1:8))

# Uruchamiamy klaster obliczeniowy z wszystkich dost�pnych rdzeni - 1 (zwyczajowo zostawia si� jeden, aby system swobodnie dzia�a�)
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

# Budujemy (trenujemy) model lasu losowego
model_rf <- train(obs ~ ., 
                  data = dane_rf_train, 
                  method = "rf", 
                  replace = T,
                  ntree = 200,
                  metric = 'RMSE',
                  tuneGrid = tunegrid, 
                  trControl = cross.walid)

# Zatrzymujemy klaster
stopCluster(cluster)
registerDoSEQ() 


plot(model_rf)


dane1 <- dane %>%
  mutate(date_unix = as.numeric(as.POSIXct(dane$date),
                                week= week(date),
  ))

dane2 <- dane1 %>%
  mutate(week = week(date))

dane3 <- dane2 %>%
  mutate(weekday = weekday )

dane4 <- dane3 %>%
  mutate(hour = )

dane5 <- dane4 %>%
  mutate(month = )

dane6 <- dane5 %>%
  mutate(day_julian = )


dane_rf_test$mod <- predict(model_rf, dane_rf_test)

ggplot(data=dane_rf_test, aes(x=obs, y=mod))+
  geom_point(alpha=0.6, color="darkgreen") +
  geom_smooth(method="lm", formula = y~x-1) +
  geom_abline(intercept=0, col="grey", linetype="dashed", size=1) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~")), 
               label.x.npc = "right", label.y.npc = 0,
               formula = y~x-1, parse = TRUE, size = 5) +
  xlab("St�enie PM10 [�g/m3] (Obserwacja)") +
  ylab("St�enie PM10 [�g/m3] (Prognoza)") +
  ggtitle("Wykres rozrzutu prognozy od obserwacji dla st�e� PM10\nna stacji w Czestochowie w roku 2018") +
  theme_minimal()



test_data %>%
  gather(key = "type", value = "wynik", obs, mod) %>%
  timeVariation(pollutant = "wynik", group = "type", difference = T, type = 'typ') -> a


bind_rows(dane_rf_test %>%
            mutate(typ = "Model A"),
          dane_rf_test1 %>%
            mutate(typ = "Model B")
) -> test_data


library(openair)
modStats(mydata = test_data,
         mod = "mod",
         obs = "obs",
         type = c("typ", "season")) %>%
  arrange(season) %>%
  knitr::kable(digits = 2)



library(openair)
modStats(mydata = test_data,
         mod = "mod",
         obs = "obs",
         type = c("typ")) %>%
  arrange(season) %>%
  knitr::kable(digits = 2)



dane <- dane_rf %>%
  mutate(date_unix = as.numeric(date),
         week = week(date),
         week_day = wday(date, label = F),
         hour = hour(date),
         month = month(date),
         day_julina =yday(date))





test_data %>%
  gather(key = "type", value = "wynik", obs, mod) %>%
  timeVariation(pollutant = "wynik", group = "type", difference = T, type = 'typ') -> a


bind_rows(dane_rf_test %>%
            mutate(typ = "Model A"),
          dane_rf_test1 %>%
            mutate(typ = "Model B")
) -> test_data


library(openair)
modStats(mydata = test_data,
         mod = "mod",
         obs = "obs",
         type = c("typ", "season")) %>%
  arrange(season) %>%
  knitr::kable(digits = 2)



library(openair)
modStats(mydata = test_data,
         mod = "mod",
         obs = "obs",
         type = c("typ")) %>%
  arrange(season) %>%
  knitr::kable(digits = 2)



dane <- dane_rf %>%
  mutate(date_unix = as.numeric(date),
         week = week(date),
         week_day = wday(date, label = F),
         hour = hour(date),
         month = month(date),
         day_julina =yday(date))


