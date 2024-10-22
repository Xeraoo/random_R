# --------------------------------------------------------
# 2. R jako kalkulator
# --------------------------------------------------------
## 2.1 Podstawowe operacje arytmetyczne
# --------------------------------------------------------

#  |--------------------------------------|
#  |Operator |opis                        |
#  |:--------|:---------------------------|
#  |-        |Odejmowanie                 |
#  |+        |Dodawanie                   |
#  |*        |Mnożenie (iloczyn)          |
#  |/        |Dzielenie (iloraz)          |
#  |^ lub ** |Potęgowanie                 |
#  |%%       |Reszta z dzielenia          |
#  |%/%      |Część całkowita z dzielenia |

# Przykłądy 
2+2
2^2
4^2
1/10
(3+7)^(4-2)
8%%3
8%/%3

# --------------------------------------------------------
## 2.2. Funkcje arytmetyczne 
# --------------------------------------------------------

library(help = "base")

#  |Funkcja      |Opis funkcji                                                                                                |
#  |:------------|:-----------------------------------------------------------------------------------------------------------|
#  |round(x)     |Liczba całkowita najbliższa wartości x                                                                      |
#  |signif(x,k)  |Wartość x zaokrąglona do k miejsc znaczących                                                                |
#  |floor(x)     |Podłoga, czyli największa liczba całkowita nie większa od x                                                 |
#  |ceiling(x)   |Sufit, czyli najmniejsza liczba całkowita nie mniejsza od x                                                 |
#  |trunc(x)     |Wartość x po odcięciu części rzeczywistej, dla liczb dodatnich działa jak floor(), dla ujemnych jak ceiling |
#  |abs(x)       |Wartość bezwzględna z x                                                                                     |
#  |log(x)       |Logarytm naturalny z x                                                                                      |
#  |log(x, base) |Logarytm o podstawie base z x                                                                               |
#  |log10(x)     |Logarytm o podstawie 10 z x                                                                                 |
#  |log2(x)      |Logarytm o podstawie 2 z x                                                                                  |
#  |exp(x)       |Funkcja wykładnicza (eksponenta) z x                                                                        |
#  |expm1(x)     |Funkcja równoważna wyrażeniu exp(x)-1, ale wyznaczona z większą dokładnością dla x&#124;«1&#124;            |
#  |log1p(x)     |Funkcja równoważna wyrażeniu log(1+x), ale wyznaczona z większą dokładnością dla x&#124;«1&#124;            |
#  |sqrt(x)      |Pierwiastek kwadratowy z x, równoważne poleceniu x^0.5                                                      |


# W ramach ćwiczenia Wykonaj poniższe operacje.

round(2.5)         ; round(2.51)
signif(1.22562, 4) ; signif(1.22562, 2)
floor(23.45)       ; floor(23.75)     
ceiling(23.45)     ; ceiling(23.57)
trunc(23.25)       ; trunc(23.89)
abs(-54.2) ; abs(54.2)
log(10)    ; log(1)
log(10,10) ; log10(10)         
log(100)   ; exp(5)            
exp(0)     ; exp(1)
sqrt(4)    ; sqrt(36)


# reguła zaokrąglania

round(x = 2.2545) ; round(x = 2.2545, digits = 1) 
round(2.2545, 3) ; round(2.2545, 4)

16^(1/4)
round(1.23446789, 3) # działa dobrze ? 
abs(-23)
sqrt(100) ; log10(100)
log(1000, 10) ; log10(1000)

# --------------------------------------------------------
## 2.3. Funkcje trygonometryczne 
# --------------------------------------------------------


#  |Funkcja         |Opis funkcji                                                                                  |
#  |:---------------|:---------------------------------------------------------------------------------------------|
#  |cos(x)/sin(x)   |Wartość funkcji cosinus/sinus w punkcie x                                                     |
#  |tan(x)          |Wartość funkcji tangens w punkcie x                                                           |
#  |acos(x)/asin(x) |Wartość funkcji arcus cosinus/sinus w punkcie x                                               |
#  |atan(x)         |Wartość funkcji arcus tangens w punkcie x                                                     |
#  |atan2(y, x)     |Funkcja wyznaczająca kąt (w radianach) pomiędzy osią OX a wektorem o początku w punkcie (0,0) |  


# --------------------------------------------------------
## 2.4 Operatory logiczne i relacyjne
# --------------------------------------------------------

!FALSE # negacja
!TRUE # negacja
FALSE | FALSE # alternatywa
TRUE  | TRUE # alternatywa
TRUE  | FALSE # alternatywa
FALSE & FALSE # koniunkcja
TRUE  & TRUE # koniunkcja
TRUE  & FALSE # koniunkcja

# Wartość logiczna jest róWnież zwracana przez R, gdy stosujemy operatory logiczne.

1 > 3
3 >= 3
5 == 5
5 != 5
5 == 5 & 5 != 5 
5 == 5 | 5 != 5


# --------------------------------------------------------
# 3. Obiekty i typy danych
# --------------------------------------------------------

library(tidyverse)

#  |Funkcja       |Opis funkcji                                |
#  |:-------------|:-------------------------------------------|
#  |class()       |klasa, typ obiektu                          |
#  |typeof()      |typ obiektu                                 |
#  |length()      |długość obiektu                             |
#  |attributes()  |atrybuty obiektu                            |
#  |attr()        |dostęp do atrybutów obiektu                 |
#  |object.size() |wielkość obiektu w bajtach                  |
#  |mode()        |wewnętrzna reprezentacja obiektu            |
#  |str()         |szczegóły wewnętrznej reprezentacji obiektu |

# Postaraj się zapamiętać:** `str(), class(), lenght(), typeof()` - będziemy z nich często korzystać.

class("a") 
class(1L) ; typeof(1L)
class(1)
class(1.1) ; typeof(1.1)
class(FALSE)
class(NA)

# --------------------------------------------------------
## 3.1. Zmienna
# --------------------------------------------------------

a = 2      ## definiujemy zmienną a
b = 3      ## definiujemy zmienną b
a          ## wyświetla wartość zmiennej a
b          ## wyświetla wartość zmiennej b
a+b        ## zwraca wyniki dodawania zmiennych
d <- a+b   ## zapisuje winik pod zmiennej d 
d          ## wyświetla wartość zmiennej d


# Kilka bardziej złożony przykładów:

27+abs(-12.4)*(1/2)^exp(2)-100 * sqrt(0.3)
d <- 27+abs(-12.4)*(1/2)^exp(2)-100 * sqrt(0.3)
d 
z <-c/(a*2)+10^b
z

# Teraz, kilka przykładów z trochę innej beczki:


nazwa <- "nazwa"
logiczny <- "TRUE"
inny <- "5"
calko1 <- 5
calko2 <- 5L
rzecz <- 3.876
nic <- NA
pusty <- NULL

# wyświetlanie
(z <- 23)

# --------------------------------------------------------
## 3.2. Wektor
# --------------------------------------------------------

a <- c(1,2,3,4)
a

a <- 1:4
a

a <- c(1L, 2L: 3L, 4L) ; a  ; typeof(a)
a1 <- 1:4 ; a1 ; typeof(a1)
a2 <- 1.1:4.1 ; a2 ; typeof(a2)

a <- c(2.5e3, 1.1e2, NaN, Inf, -Inf) ; a ; class(a) ; typeof(a)


# Teraz utworzymy wektory wartości logicznych i znakowych

a  <- c("a","b","b", "c")  ; a  ; typeof(a)
a1 <- c(TRUE, FALSE, T, F) ; a1 ; typeof(a1)


# **UWAGA !!!** Zauważ, że nadpisaliśmy zmienną `a` i `a1`.

# kolejność typu wektora
a1 <- c(F, 1L, 2.2, "cztery") ; typeof(a1)
a2 <- c(F, 1L, 2.2) ; typeof(a2)
a3 <- c(F, 1L) ; typeof(a3)

a <- 1:4 ; a
typeof(a)
typeof(as.double(a)) # funkcja w funkcji

a <- 1.1:4.1 ; a
typeof(a)
typeof(as.integer(a)) # zwraca wynik funkcji zewnętrznej

a <- c(TRUE, FALSE, TRUE) ; a
typeof(a) 
typeof(as.character(a))

a <- c(1,0,1,1,1,0) ; a
typeof(a)
typeof(as.logical(a))

a <- c(1L,0L,1L,1L,1L,0L) # tworzymy wektor typu integer
a             # wyświetlamy obiekt na ekranie konsoli
as.logical(a) # konwersja 0 = FALSE, 1 = TRUE
typeof(a)     # sprawdzamy

# teraz poprawna składnia
a <- c(1L,0L,1L,1L,1L,0L) 
a <- as.logical(a) # konwersja z nadpisaniem 
typeof(a) # sprawdzamy 

# --------------------------------------------------------
### 3.2.1. Funkcje tworzenia wektorów
# --------------------------------------------------------

a <- c(10,1:3,10) ; a  
a <- c(4:1, 1:4) ; a
c(a, c(1,4,5), 8:5)
c(a, c(TRUE, FALSE))


# **Funkcja seq()** 

?seq()

example(seq()) ## ładuje do okna konsoli przyklady zastosowania


#Utworzymy teraz sekwencję liczb:
# * od 1 do 10 z krokiem 1, oraz  
# * od 1 do 2 z krokiem 0.1.

seq(from = 1, to = 10, by = 1) # integer
seq(from = 1, to = 2, by = 0.1) # real

seq(1,10,1) # zapis uproszczony, bez nazw argumentów
seq(10) # zapis uproszczony, z argumentami domyślnymi
seq(1,2,0.1)

# ilość elementów
sek1 <- seq(from = 34, to = 128, length.out = 5)
sek1
sek1 <- seq(34, 128, length.out = 4)
sek1

# minus 
sek2 <- seq(100,1,-12)
sek2

a <- 2 ; b <- 20    # tworzymy zmienne a i b
seq(a,b, len = b/a) # wykorzystujemy je do uruchomienia funkcji seq()


# **Funkcja rep()** 

rep(1:4, times = 2) # powielamy 2 razy wektor
rep(1:4,2) # j.w.
rep(seq(4),2) #j.w. z seq()
rep(seq(4), len = 3) # tylko 3 elementy wektora bez powielania
rep(1:4, each = 2) # nie to samo,

rep(1:3, each = 3) # każdy 3 razy
rep(1:3, each = 3, times = 2) # j.w, potem wektor 2 razy
rep(1:3, each = 3, times = 2, len = 9) # j.w. tylko zwraca 9 ostatnich elementów


rep(seq(3), each = 2, len = 4)  # len uciał 2 elementy
rep(seq(3), each = 2, len = 8)  # len dodał 2 elementy


rep(1:3, times = c(1,2,3))
rep(1:3, 1:3) # j.w. prościej
rep(1:3, 3:1) # odwrotnie

# --------------------------------------------------------
### 3.2.2. Wektory domyślne pakietu bas
# --------------------------------------------------------

letters # małe litery
LETTERS # duże litery
month.name # nazwy miesięcy
month.abb # skrócone j.w.
pi # 

format(ISOdate(2000, 1:12, 1), "%B")
format(ISOdate(2000, 1:12, 1), "%b")

czas <- ISOdate(year = 2000, month = 1:12, day = 1)  
czas

czas <- format(czas, "%B")  
czas

# --------------------------------------------------------
### 3.2.3. Indeksowanie wektorów
# --------------------------------------------------------


wek <-  c("a", "b", "c", "d") # tworzymy wektor
wek # wyświetlamy
str(wek) # wyświetlamy własnosći

length(wek) ; length(letters) ; length(month.name)


# oiperator [...]

LETTERS[1] # pierwszy element wektora
LETTERS[5] # piąty
LETTERS[26] # ostatni element wektora
LETTERS[length(LETTERS)] # j.w. ale za pomocą funkcji length


wek <- c(seq(2,28,2), seq(2,4,length.out = 6))
wek
length(wek) # Sprawdzamy długość wektora
wek[length(wek)] # wybieramy ostatni element wektora
wek[10] # wybieramy 10 element wektora

LETTERS[c(6,12,26)] 
LETTERS[c(6,12,length(LETTERS))]
a <- c(6,12,26)
LETTERS[a]

letters[seq(1,length(letters),2)]


co_drugi <- seq(1,length(letters),2)
letters[co_drugi]

LETTERS[1:10] # dziesięć pierwszych liter
LETTERS[c(1:5,21:26)] # pięć pierwszych i pięć ostatnich

LETTERS[26:1]
letters[seq(26,1,-1)]
seq(26,1,-1) # j.w. tylko z funkcją seq()

# usuwanie
letters[-(1:20)] # 6 ostatnich elementów


# indeksowanie operatorem relacji

wieksze_od_10 <- wek > 10


wieksze_od_10 ; wek # porównujemy wartości, patrz tabela.

knitr::kable(data.frame(wek, wieksze_od_10), caption = "Tabela X. Porównanie wekotorów wek i a")

wek[wieksze_od_10] # indeksowanie

wek[wek > 10]

wek[!wieksze_od_10]

wek2 <- rep(letters[1:5],3)
wek2

wek2[wek2 == "a" | wek2 == "c"] # wybierz a oraz c
wek2[wek2 != "b" | wek2 != "d"] # wybierz a oraz c


wek2[wek2 %in% c("a", "b")] # wybierz a oraz c


wek1 <- c(1:2, rep(NA, 4), seq(6,7,0.5))
wek1
is.na(wek1)


wek1[!is.na(wek1)] # liczby
wek1[is.na(wek1)] # NA

na.omit(wek1) -> bez_NA
str(bez_NA)

# --------------------------------------------------------
# 3.2.4. Operacje wykonywane na wektorach
# --------------------------------------------------------

a <- c(1:4) # Tworzymy dwa wektor a
b <- seq(2,8,2) # Tworzymy dwa wektor b

a ; a+2 # wyświelt, wykonaj dodawanie
a ; a-2 
a ; a*2 
a ; a/2
a ; a^2
a ; a%/%2 # cz. całkowita
a ; a%%2 # reszta z dzielenia


# **Operacje na równych wektorach**

a ; b ; a+b
a ; b ; a-b
a ; b ; a*b
a ; b ; a/b
a ; b ; a^b

# --------------------------------------------------------
# **Wektory o róznych długościach**
# --------------------------------------------------------

a <- seq(8)
b <- c(2,4)

a; b; b*a
a; b; b-a
a; b; b+a
a; b; b^a

# Analogiczną operację, można wykonać stosując funkcje `rep()` dla wektora b.

{r}
a <- seq(8)
b <- c(2,4)
b <- rep(b,4)

a; b; b*a
a; b; b-a

# --------------------------------------------------------
# **Operacje relacyjne**
# --------------------------------------------------------

x <- c(1,5,7,5) # tworzymy wektory
y <- c(4,2,9,5)

x; y; x > y # czy mniejszy ?
x; y; x < y # czy większy ?
x; y; x <= y # czy nie większy ?
x; y; x >= y # czy nie mniejszy ?
x; y; x == y # czy równy ?
x; y; x != y # czy różny ?

# --------------------------------------------------------
# **operacje logiczne**
# --------------------------------------------------------

a <- c(TRUE, FALSE, NA) # Tworzymy wektory wartości logicznych
b <- c(T, F, NA) # to samo, ale prosty zapis

a ; !a # ! negacja
b ; !b # ! negacja

T  |  b # | alternatywa
F  |  b # | alternatywa
NA |  b # | alternatywa

T  & a # & koniunkcja
F  & a # & koniunkcja
NA & a # & koniunkcja


a <- c(T, T, F, F, NA, NA)
b <- 1:6

a; b; a+b
a; b; a-b
a; b; a*b
a; b; a/b

as.integer(a)

# --------------------------------------------------------
# **Modyfikacje elementów wektora** 
# --------------------------------------------------------

# tworzymy wektor
x <- seq(1,5,0.8) 
y <- 1:length(a)  

# Podmieniamy 1-elemenet wektora
x[1] <- 1.2 ; x
# podmieniamy 6 element (ostatni)
x[6] <- 5.1 ; x
# j.w inny sposób
x[length(x)] <- 7 ;x
# podmieniamy 2-elementy, [2:3]
x[2:3] <- c(5,5) ; x
# podmieniamy 3 pierwsze elementy wektora x wekotem y
x[1:3] <- y[1:3]
x;y
x == y
# reguła zawijania, co podmieniamy za co ?
x
x[-(1:4)] <- 100
x


# --------------------------------------------------------
### 3.2.5. Funkcje działajace na elementach wektorach 
# --------------------------------------------------------


x <- seq(2,20,4) # tworzymy wektor

x ; sqrt(x)
x ; exp(x)# funkcja wykładnicza każdy element e^x, a e=2.71
x ; log(x, 10)

x ; round(log(x, 10),2)

x <- seq(-100,100,25)
x ; abs(x)

x ; round(log(x[x>0], 10) * (3/2), 2)


a <- sample(x = 1:4,   # wektor x 
            size = 20, # liczność obserwacji wynikowego wektora
            replace = T) # powtarzaj 
a

sort(a) # rosnący
sort(a, decreasing = F) # j.w.
sort(a, decreasing = T) # malejący

# --------------------------------------------------------
### 3.2.6. Funkcje wektorów
# --------------------------------------------------------

# tworzymy wektor
a <- sample(x = 1:10, size = 100, replace = T) # powtarzaj 
b <- sample(x = c(1:5,NA), size = 100, replace = T) # powtarzaj 


max(a)
max(b)

max(a) 
max(b, na.rm = T) 

max(b[!is.na(b)]) # z indeksowaniem

b[is.na(b)]<- 6 # podmiana NA na 3
max(b) # podmiana


b[b == 6]<- NA # powrót do NA

max(a); min(a); mean(a); median(a); sum(a) 
prod(a) # ilosczyn
sd(a) # odch.stand.
var(a) #wariancja

max(b, na.rm=T); min(b, na.rm=T); mean(b, na.rm=T); median(b, na.rm=T); sum(b, na.rm=T)

prod(b, na.rm=T)
sd(b, na.rm=T)
var(b, na.rm=T)


range(a) ; range(b)

unique(a)
unique(b, na.rm = T)

sort(unique(a))
sort(unique(b, na.rm = T))


a <- sample(seq(1,2,by = 0.1), size = 100, replace = T)
sort(unique(a))
sort(table(a))


max(table(a))

min(table(a))

sum(table(a)) ; length(a) ; sum(table(a)) == length(a)

hist(a, 
     main = "Mój pierwzy histogram",
     xlab = "Wartości wektora a",
     ylab = "częstość występowania wartości")


# * `summary()` - zwraca kwartyle, mediane oraz średnią
# * `quantile()` - zwraca określony kwantyl

summary(a)
summary(b) # działa nawet na brakach danych NA


boxplot(a, horizontal = T, main = "Mój drugi wykres")



quantile(a) # analogia summary - bez średniej
quantile(a, probs = 0.25)
quantile(a, probs = c(0.25, 0.5))
quantile(a, probs = seq(0,1,0.1))


# --------------------------------------------------------
### 3.2.7. Funkcje generowania wartości losowych
# --------------------------------------------------------

rk1 <- rnorm(n = 100) # podajemy tylko liczbe elementów wektora n
rk2 <- runif(n = 100, min =2, max = 5) # podajemy n, wartość min i max. 

hist(rk1)
hist(rk2)

## 3.3. Listy

al <- list(imie=c("Jan","Tomasz"), nazwisko="Kowalski", 
           wiek=25, czyWZwiazku=T)
al

# --------------------------------------------------------
# 3.3.1. Indeksowanie listy
# --------------------------------------------------------
al[1] # odwołanie do atrybutu listy
al[3]

al[[3]] * 2
al$wiek * 2

al[[1]][2]

### 3.3.2. Modyfikowanie list

al$wiek <- c(23,35,77,58)
al
al[[2]] <- c("Nowak","Kowalski", "Koper")
al

l1 <- list(nowa = c(1:10))
l2 <- list(koniec = c(20:29))
lista <- c(l1,l2) 
lista

# --------------------------------------------------------
# 3.3.2. Operacja na listach
# --------------------------------------------------------

nowa <- list(waga = sample(50:80, replace = T, size = 5),
             wiek = round(runif(5, 25, 77),0),
             wzrost = sample(150:190, size = 5))


mean(nowa) # nie działa

mean(nowa$waga)
mean(nowa[[2]]) # itd.


lapply(nowa, mean) -> a ; a
sapply(nowa, mean) -> b ; b


# Działą na funkcjach zwracających wektory
lapply(nowa, summary) -> a ; a # lista wektoróW
sapply(nowa, range) -> b ; b # ramka danych

# można odwrócić wartość liczby
lapply(nowa, "-") -> a ; a 

# można przeliczyć każdy element
lapply(nowa, sqrt) -> a ; a 
sapply(nowa, exp) -> b ; b 

# można stosować argumenty funkcji, ale bez nawiasów
sapply(nowa, quantile) -> b ; b 
sapply(nowa, quantile, prob = c(0.5,0.75)) -> b ; b # dodatkowy argument funkcji



nowa <- list(waga = sample(50:80, replace = T, size = 5),
             wiek = round(runif(5, 25, 77),0),
             wzrost = sample(150:190, size = 5),
             tekst = letters[1:5])

lapply(nowa, mean) -> b
b
sapply(nowa, mean) -> a
a

# --------------------------------------------------------
# 3.4. Macierze
# --------------------------------------------------------

matrix(data = 0, nrow = 2, ncol = 4) 

# lub forma skrócona
matrix(0,2,4)

matrix(1:6, nrow = 2, ncol = 3)

matrix(seq(3,8,1), nrow = 3, ncol = 2)

x <- matrix(c(1,2,3,1:3,3:5), 3, 3)
x
x <- matrix(c(1,2,3,1:3,3:5), nrow = 3, ncol = 3)
x

a <- c("A","B","C","D")
xx <- matrix(a,2,2)



# --------------------------------------------------------
# Utwórz macierz: matrix(1:6, nrow = 2, ncol = 3) -> xx 
# Następnie wykonaj następujące polecenia i 
# wyjaśnij w komentarzach co one zwracają.
# --------------------------------------------------------

length(xx)
is.vector(xx)
is.atomic(xx)
typeof(xx)
class(xx)
nrow(xx)
ncol(xx)


# --------------------------------------------------------
# 3.5. Ramki danych
# --------------------------------------------------------

ramka <- data.frame(id = c(100,101,102), 
                    wiek = c(25,21,22),
                    wzrost = seq(170,190,10),
                    chlopiec = c(TRUE,TRUE,FALSE))
ramka

View(ramka)
ramka$wiek

# --------------------------------------------------------
# 3.5.1. Przygotowanie danych
# --------------------------------------------------------

getwd()
dir()
load(file = "dane.RData")
dane

# --------------------------------------------------------
# 3.5.2. Własności obiektu data.frame
# --------------------------------------------------------

nrow(dane)
#liczbę kolumn sprawdzamy funkcją ncol(). 
ncol(dane)
# naraz
dim(dane)


colnames(dane)  # nazwy kolumn
names(dane)     # j.w
row.names(dane) # nazwy wierszy

summary(dane) 

str(dane)
head(dane) # 6 pierwszych
tail(dane) # 6 ostatnich

head(dane, 3) # 3 pierwsze
tail(dane, 3) # 3 ostatnie

View(dane)


# --------------------------------------------------------
#  3.5.3. Indeksowanie
# --------------------------------------------------------  

load("dane.RData")

# --------------------------------------------------------  
# Filtrowanie wierszy w ramce danych
# --------------------------------------------------------  

dane[1,] # pierwszy wiersz
dane[5:7,]
dane[c(2:3,7:9) , ]
indeksy <- c(2:3,7:9)
dane[indeksy,]

ciezkie <- dane$waga > 60 # operator relacji
ciezkie # wyniki logical

dane[ciezkie, ] # waga większa od 60

dane[ -seq(1,9,2), ]

-seq(1,9,2) 
-c(1,2,3)
-5:3 # uwaga
-5:-3

##UWAGA!!!## Nie można mieszać jednocześnie indeksów dodatnich i ujemnych.

dane[c(-4:-2, 8, 9),] # błąd

# --------------------------------------------------------  
# Selekcja kolumn w ramce danych## - Zacznijmy od prostego przykładu: 
# --------------------------------------------------------  

dane[1]

dane$imie

dane[12] # metoda 1
dane$Kraj # metoda 2
levels(dane$Kraj) # tylko unikalne
unique(dane$Kraj) # działa j.w. 

class(dane$imie)

dane[,2]

dane[,2, drop=FALSE]

colnames(dane) # nazwy
dane[, "praca"]

# równoważnie moglibyśmy napisać

dane[, c("imie", "nazwisko", "plec", "kraj")]
dane[ ,c(1,2,11:12)]
kol <- c(1,2,11:12)
dane[,kol]

kolumny <- c("imie", "nazwisko", "plec", "kraj")
dane[, kolumny] # j.w.

# --------------------------------------------------------  
# Wybieranie pod ramki danych##
# --------------------------------------------------------  

# wybór czterech wierszy i czterech kolumn lub 
dane[c(1:4), c(1:4)]
#wybór czterech wierszy i jednej kolumny lub 
dane[c(1:4), 2]
#Jeden wiersz i cztery kolumny może wyglądać taki.
dane[1, c(9:12)]

rownames(dane)

dane$imie <- as.character(dane$imie) # konwersja typu z nadpisaniem
rownames(dane) <- dane$imie # podmiana nazwy wierszy
dane[,3:6] # podgląd


dane[c("Zofia","Julia", "Robert", "Piotr"), 
     c("waga", "wiek", "wzrost")]

rownames(dane) <- NULL
dane[,3:6] # podgląd

colnames(dane) <- c("tu wpisać nowe nazwy kolumn")
# lub
colnames(dane)[1:3] <- c("zmienisz tylko pierwsze trzy nazwy")

# --------------------------------------------------------  
# Sortowanie przez indeksowanie
# --------------------------------------------------------  

dane[,"wzrost"]

# Wynikiem funkcji `order()` są indeksy kolejnych, wartości. 
# Najmniejsza wartość to 157, na pozycji 6, kolejna to 161 na pozycji 2, 
# kolejna to 164 na pozycji 1 i tak dalej.

order(dane[,"wzrost"])
# Równoważnie
order(dane$wzrost) ; dane$wzrost

kolejnosc <- order(dane[,"wzrost"])
dane[kolejnosc, c(1:2,7:9)]

dane[dane$plec == "K" & dane$waga < 70, ] # 

# --------------------------------------------------------  
# 3.5.4. Operacje arytmetyczne na data.frame()
# --------------------------------------------------------  

wskaznik <- dane$waga/mean(dane$waga)

dane$wskaznik <- dane$waga/mean(dane$waga)

# Funkcją aggregate()

# `aggregate()`
# `by()`
# `taplly()`

str(dane)

aggregate(dane$zarobki,       # kolumna uśrednia 
          list(kraj = dane$kraj), # grupowanie wg.
          FUN = mean)             # srednia

aggregate(dane$zarobki,list(plec = dane$plec), sum)


# Inna funkcja to `by()`

sredni <- by(dane$zarobki, dane$plec, FUN=mean) 
sredni

sredni <- as.list(sredni)
as.data.frame(sredni)

# I jeszcze funkcja `tapply()`

tapply(dane$zarobki, dane$plec, mean) # średnia w grupach


# --------------------------------------------------------  
# 3.5.5. Wybrane funkcje dla obiektu data.frame() 
# --------------------------------------------------------  

dane$typ <- c(seq(1,7,1), NA, NA) # wymierzony
dane$rodzaj <- c(TRUE, FALSE, FALSE) # reguła zawijania

dane

dane[,-nrow(dane):-(nrow(dane)-3)]
colnames(dane) ; ncol(dane) # sprawdzamy

a <- data.frame(las = runif(5, 0, 5),
                lit = sample(LETTERS[1:3], size = 5, replace = T))

b <- data.frame(las = runif(5, 0, 3),
                lit = sample(LETTERS[4:8], size = 5, replace = T))
a ; b

# Poleceniem `rbind()` 

rbind(a,b) -> nowy
nowy

rbind(a, c(10.1, "A"))

# Funkcją `cbind()`

cbind(a,b) # doda prefiks do nazwy kolumny

cbind(a, logiczna = c(T,T,F,F,F))

cbind(a, 
      logarytm = log(a$las),
      pierw = sqrt(b$las),
      nowa = seq(0,20, length.out = 5))

#  czytelniej 

logarytm <-  log(a$las)
pierw <-  sqrt(b$las) 
nowa  <-  seq(0,20, length.out = 5) 
logarytm ; pierw ; nowa
cbind(a, logarytm, pierw, nowa)


# --------------------------------------------------------  
# 4. Wczytywanie danych
# --------------------------------------------------------  
## 4.1. Operacje na plikach i katalogach
# --------------------------------------------------------  

getwd() ## zwraca informacje o aktualnym katalogu domyślnym
setwd("...") ## ustawia katalog domyślny 

dir()
list.files() # j.w.

pliki <- dir()
pliki

##Przykłady##

file.create("im3.txt")
file.create("im4.txt")
dir()
file.info("im1.txt")
file.info("im3.txt")
file.exists("im3.txt")
file.remove("im3.txt")
dir()
file.rename(from = "im4.txt", to = "im3.txt" )
dir()
file.remove("im3.txt")
file.append("im1.txt", "im2.txt")
## Teraz podglądnij w notatnik zawartość pliku im1.txt
file.copy(from = "im1.txt", to = "im3.txt", overwrite=FALSE)
dir.create("Twoje_Nazwisko", showWarnings=TRUE, recursive=FALSE)

# --------------------------------------------------------  
## 4.2. Wczytywanie danych
# --------------------------------------------------------  
### 4.2.1. Dane w plikach tekstowych

getwd()
setwd("...") # wchodzę do foldera dane
getwd()
dir()
# --------------------------------------------------------  

dane <- read.table(file = "Z:/R/Kurs_R/Wprowadzenie_2/dane1_srednik.txt", # lokalizacja i nazwa pliku
                   header = TRUE, # czy zawiera nazwy kolumn
                   sep = ";", # separtor wartosci
                   dec=".") # separator miejsca dzisiętnego

dane 

##Drugi przykład##

setwd("...") # To nie jest twoja ścieżka
getwd()

dane2 <- read.table("dane2_przecinek.txt", 
                     header = TRUE, 
                     sep = ",", dec=".")
dane2 

##Trzeci przykład:##

dane3 <- read.table("dane3_brudny.txt", # lokalizacja i nazwa pliku
                     header = TRUE, 
                     sep = ";",  
                     dec=".", skip = 2)
dane3 

##czwarty przykład:##

dane4 <- read.table("dane1_srednik.csv", # lokalizacja i nazwa pliku
                     header = TRUE, 
                     sep = ";",  
                     dec=".")

read.csv("dane1_srednik.csv", sep = ";") -> dane6


dane7 <- read.table(file = "http://biecek.pl/MOOC/dane/koty_ptaki.csv", 
  sep=";", dec=",", header=TRUE)

read.csv(file = "http://biecek.pl/MOOC/dane/koty_ptaki.csv", sep = ";") -> dane7


# --------------------------------------------------------  
# 4.2.2. Wczytywanie plików xlsx.
# --------------------------------------------------------  

install.packages("openxlsx")
library(openxlsx)

openxlsx::read.xlsx("dwa_arkusze.xlsx", sheet = 1) -> dane
dane

read.xlsx("dwa_arkusze.xlsx", sheet = 2) -> zwierz
zwierz

# --------------------------------------------------------  
# 4.2.3. pliki binarne
# --------------------------------------------------------  

load(url("http://biecek.pl/MOOC/dane/koty_ptaki.rda"))
load("dane.RData")

# --------------------------------------------------------  
## 4.3. Zapisywanie danych 
# --------------------------------------------------------  

getwd() # tu zapisujemy
write.table(dane, file = "write1.txt",  # txt
            sep = ";", dec = ".", 
            row.names = F, col.names = T)

write.table(dane, file = "write2.csv",  # csv
            sep = ",", dec = ".", 
            row.names = F, col.names = T)

write.csv(dane, file = "write3.csv", row.names = F)

write.xlsx(zwierz, file = "zwierz.xlsx")

save(dane, dane2, zwierz, koty_ptaki,
     file = "dane_all.RData") # można od razu zapisać kilka 

file.remove(c("dane_all.RData", "write1.txt", "write2.csv", "write3.csv", "zwierz.xlsx"))



# --------------------------------------------------------
# 5. Przetwarzanie tekstu
# --------------------------------------------------------
  
## 5.1 Wprowadzenie

"To jest napis" 
'To tez jest napis' 
"To jest napis 'a to jest napis wewnetrzny'" 

tolower(LETTERS)
toupper(letters)

letters[1:5] ; toString(letters[1:5])

cat(" co \t to \\ teraz\"\n\n bedzie?") 

paste("Napis", "napis doklejony", 12) 

# --------------------------------------------------------  
## 5.2. Konwersje napisów
# --------------------------------------------------------  

class(dane$nazwisko) # typ ?

dane$nazw_nap <- as.character(dane$nazwisko)
dane$nazw_nap


class(dane$nazw_nap)

length(dane$nazw_nap)
nchar(dane$nazw_nap)

as.numeric("2016") # konwersja na liczbę
as.factor(c("A", "B", "A", "A")) # konwersja na zmienną jakościową
as.Date("2016-01-01") # konwersja na datę

# --------------------------------------------------------  
## 5.3. Wyszukiwanie napisów
# --------------------------------------------------------  

which(dane$nazw_nap == "Kowalska")

which(dane$nazw_nap %in% c("Nowy", "Ptak"))

grep("a", dane$nazw_nap)

grep(dane$nazw_nap, pattern = "a")

dane[grep(dane$nazw_nap, pattern="a"), ]

grep("a", dane$nazw_nap, value = TRUE)

str(dane$nazwisko) # to jest factor
grep("a", as.character(dane$nazw_nap), value = TRUE) # z konwersją

dane$nazwisko <- as.character(dane$nazwisko) # konwersja
grep("a", dane$nazw_nap, value = TRUE) # z konwersją

grep("A", dane$nazwisko, ignore.case = TRUE)

grep("^[Pt]", dane$nazwisko, value = F)
grep("^[Pt]", dane$nazwisko, value = T)

# --------------------------------------------------------  
# 5.4. Fragmenty napisów
# --------------------------------------------------------  

daty <- c("2018-01-05", "2018-03-25", "2012-12-23")
substr(daty, 1, 4) # pierwsze 4
substr(daty, 6, 7) # miesiące
substr(daty, 9, 10) # dni

zdanie <- c( "W Szczebrzeszynie chrząszcz brzmi w trzcinie", "Ząb zupa zębowa dąb zupa dębowa")
(podzielony <- strsplit(zdanie, " ")) # tworzy liste

# --------------------------------------------------------  
# 5.5 Tworzenie nowych cech
# --------------------------------------------------------  

library(PogromcyDanych)

str(auta2012)

auta2012$Kolor_napis <- as.character(auta2012$Kolor)

sort(table(auta2012$Kolor_napis))

auta2012$czy_metallic <- grepl("metallic", auta2012$Kolor_napis)
table(auta2012$czy_metallic)

# --------------------------------------------------------  
# 5.6. Sklejanie napisów
# --------------------------------------------------------  

auta2012$MarkaModel   <- paste(auta2012$Marka, auta2012$Model, sep=": ")
statystykiMarkiModelu <- sort(table(auta2012$MarkaModel), decreasing = TRUE)
statystykiMarkiModelu[1:25] # to jest obiekt table

# --------------------------------------------------------  
# 5.7. Liczby, cechy jakościowe i napisy
# --------------------------------------------------------  

x <- c(2, 4, 5.5)
fx <- as.factor(x) ; fx

as.numeric(fx)

as.numeric(as.character(fx))

library(PogromcyDanych)
data(package = "PogromcyDanych")
auta2012 <-  auta2012


auta2012$autoalarm <- grepl("autoalarm", auta2012$Wyposazenie.dodatkowe)
table(auta2012$autoalarm) -> a
a ; (a[2]/sum(a))*100 # procent z autoalarm

auta2012$ABS <- grepl("ABS", auta2012$Wyposazenie.dodatkowe)
table(auta2012$ABS)
auta2012$ALU <- grepl("alufelgi", auta2012$Wyposazenie.dodatkowe)
table(auta2012$ALU)

sum(auta2012$ABS == TRUE & auta2012$ALU == TRUE) -> a # TRUE = 1
a ; paste(round((a/nrow(auta2012))*100,1), "%")

# --------------------------------------------------------  
# 6. Przetwarzanie daty
# --------------------------------------------------------  
# 6.1. Obiekty klasy date
# --------------------------------------------------------  

as.Date("2015-02-22")
as.Date("02/22/2015", format = "%m/%d/%Y")
as.Date("Czerwiec 2, 2015", format = "%B %d, %Y")

?strptime ## zasięgnijmy pomocy i zróbmy kilka przykładów 

Sys.time()
format(Sys.time(), "%a %b %d %X %Y %Z")
format(Sys.time(), "%H:%M:%OS3")


a <- as.Date("2014-03-21") ;a
format(a, "%a %b %d %X %Y %Z")
format(a, "%H:%M:%OS3")

dates <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92")
times <- c("23:03:20", "22:29:56", "01:03:30", "18:21:03", "16:56:26")
x <- paste(dates, times) ## tworzymy listę
strptime(x, "%m/%d/%y %H:%M:%S") ## tworzymy POSIXlt
a <- strptime(x, "%m/%d/%y %H:%M:%S")
a

# --------------------------------------------------------  
## 6.2. Czas ct() calender time
# --------------------------------------------------------  

(czas1 <- as.POSIXct("2015-02-13 12:56:26"))
(czas2 <- as.POSIXct("14022015 12:56:26", 
                     format = "%d%m%Y %H:%M:%S"))
# różnica czasów
czas2 - czas1
czas1 ; czas1 + 30 # + 30 sekund
czas1 ; czas1 + (24*60*60) # + 1 dzień
czas1 ; czas1 + 7*(24*3600) # + 7 dni

Sys.time()
Sys.time() - czas1 
Sys.timezone() ## zwraca strefę czasową

# --------------------------------------------------------  
## 6.3  Czas lt() local time
# --------------------------------------------------------  

(czas1 <- as.POSIXlt("2015-02-13 12:56:26"))
(czas2 <- as.POSIXlt("14022015 12:56:26", 
                     format = "%d%m%Y %H:%M:%S"))

czas1$sec
czas1$min
czas1$zone

czas2 - czas1
czas1 + 7*3600 # 7 godzin

# --------------------------------------------------------  
## 6.4. Pakiet lubridate
# --------------------------------------------------------  

# install.packages("lubridate")
library(lubridate)

now() # do sekundy
today() # do dnia

ymd_hms("2015-02-14 23:59:59")
(czas3 <- mdy_hm("02/14/15 08:32"))

wday(czas3, label = TRUE)

week(czas1)
day(czas1)
month(czas1)
year(czas1)

czas3 + hours(4)
czas3 + days(2) + months(4) + years(1)

# --------------------------------------------------------  
# 7. Typ czynnikowy - cechy jakościowe
# --------------------------------------------------------  

nz <- factor(c("sierzant", "kapitan", 
               "sierzant", "sierzant"))
nz

summary(nz)
table(nz)
str(dane)

# --------------------------------------------------------  
# 7.1. Wprowadzenie
# --------------------------------------------------------    
## 7.2. Cechy jakosciowe
# --------------------------------------------------------  

# Wczytaj zestaw danych `dane` z pliku `"dane.RData"`.
getwd() # gdzie jestem 
# setwd("tam gdzie plik dane.RData") # ustaw lokalizację 
load(file = "dane.RData")


osoba <- paste(dane$imie, 
               dane$nazwisko) 
osoba # nowa cecha

dane <- cbind(osoba,  # dodawanie kolumny
              dane[,3:ncol(dane)]) # selekcja 
dane[,1:4] # kulmny imie i nazwisko zastąpiono kolumną osoba

class(dane$osoba)
class(dane$praca)
class(dane$zarobki)

View(dane)
str(dane)

# --------------------------------------------------------  
library(tidyverse)
dplyr::glimpse(dane)

# `levels()` zwraca wektor napisów - poziomów zmiennej jakościowej.
# `table()` zwraca wektor liczebności każdego czynnika,

levels(dane$praca)
table(dane$kraj)
table(auta2012$Waluta)

summary(auta2012$Waluta)
table(auta2012$Waluta, useNA = "always")

# --------------------------------------------------------  
# 7.3. Procenty
# --------------------------------------------------------  

waluty <- table(auta2012$Waluta)
(frakcje <- prop.table(waluty))
(procenty <- 100*frakcje)
round(procenty, digits = 1)
zaokragloneProcenty <- round(procenty,1)
sort(zaokragloneProcenty)
sort(zaokragloneProcenty, decreasing = TRUE)
order(zaokragloneProcenty)
kolejnoscPosortowanych <- order(zaokragloneProcenty)
zaokragloneProcenty[kolejnoscPosortowanych]
zaokragloneProcenty[kolejnoscPosortowanych]
zaokragloneProcenty[rev(kolejnoscPosortowanych)]

# --------------------------------------------------------  
# 7.4. Graficzne statystyki opisowe
# --------------------------------------------------------

auta2012[as.numeric(as.character(auta2012$Rok.produkcji)) > 1995,] -> auta
rok <- table(auta$Rok.produkcji, useNA = "always" )
rok

barplot(rok) # ustawienia standardowe

barplot(as.table(rok),
        horiz = T, # poziomo
        las = 1) # # napisy poziomo

# --------------------------------------------------------  
# 7.5. character or factor
# --------------------------------------------------------  

cat(dane$praca)
cat(as.character(dane$praca))

czynnik <- as.factor(dane$kraj) ; str(czynnik)
napisy <- as.character(dane$kraj) ; str(napisy)

# --------------------------------------------------------  
# 7.6. Tablice częstości
# --------------------------------------------------------  

table(auta2012$Kraj.pochodzenia, auta2012$Waluta)

krajWaluta <- xtabs( ~ Kraj.pochodzenia + Waluta, auta2012)

krajWaluta # wyświetlamy

round(prop.table(krajWaluta, 1), 3)

xt <- xtabs(~Kraj.pochodzenia+Waluta, auta2012)
xt <- as.data.frame(xt)
xt[xt$Freq > 0, ]

# --------------------------------------------------------  
# 7.7. Przekształcanie zmiennych ilościowych w jakościowe
# --------------------------------------------------------

dane$kateg <- cut(dane$urlop, 
                  breaks = c(0,10,max(dane$urlop)))
table(dane$kateg)
barplot(table(dane$kateg))

sort(table(dane$kateg))

levels(auta2012$Skrzynia.biegow)
levels(auta2012$Rodzaj.paliwa)
tab <- xtabs(~Rodzaj.paliwa + Skrzynia.biegow, auta2012)

mosaicplot(tab, las=2, col=c("green", "blue", "red"))

krajWaluta <- xtabs( ~Waluta + Kraj.pochodzenia, auta2012)
mosaicplot(round(prop.table(krajWaluta, 1), 3), 
           col = c(2:(length(levels(auta2012$Kraj.pochodzenia)) + 1)
           ), las =2)

install.packages("openair")
library(openair)
View(mydata)


mydata$p.ws <- cut(mydata$ws, c(0, 2,4,6,8, 10, max(mydata$ws, na.rm = T)))
a <- round(prop.table(table(mydata$p.ws)),4)*100
barplot(a,  las = 2, col = c("red", "green","blue", "yellow", "grey", "tomato4"), 
        xlab = "przedziały prędkosci wiatru [m/s]", ylab =  "Udziały wiatrów [%]")

# --------------------------------------------------------  
#  7.8. Kolejność i nazwy czynników
# --------------------------------------------------------  

levels(auta2012$Rodzaj.paliwa)

boxplot(Rok.produkcji ~ Rodzaj.paliwa, auta2012, las=2)

levels(auta2012$Skrzynia.biegow)

auta2012$Skrzynia.biegow <- factor(auta2012$Skrzynia.biegow, 
                                   levels=c("manualna","automatyczna", ""))
levels(auta2012$Skrzynia.biegow)

auta2012$sort <- reorder(auta2012$Rodzaj.paliwa, # jakościowa
                         auta2012$Rok.produkcji, # ilościowa
                         mean)
boxplot(Rok.produkcji ~ sort, auta2012, las=2)
boxplot(Rok.produkcji ~ sort, auta2012, 
        las=2, outline = F) # bez odstających obs.

