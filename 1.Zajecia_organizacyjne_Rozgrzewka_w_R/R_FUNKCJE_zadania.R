


## ---- Tworzenie funkcji ----

# Definiowanie funkcji jest możliwe przy pomocy następującej składni
#
# nazwa_funkcji = function(argument1, argument2, ...) {
#   ... treść funkcji...
# }
#



# Przykład 1. 
# Stwórzmy funkcję o nazwie fahrenheit_to_celsius, 
# która będzie konwertować temperaturę
# (zgodnie z ogólnodostępnym wzorem):


fahrenheit_to_celsius = function(temp_F) {
  temp_C = (temp_F - 32) * 5 / 9
  return(temp_C)
}

# Krok 1. Tworzymy funkcję fehrenheit_to_celsius - 
# nazwa funkcji może być dowolna. 

# Krok 2. Funkcję definiujemy słowem 'function'.

# Krok 3. Lista argumentów funkcji podana jest w nawiasie zwykłym.
# Argumentem tej funkcji jest temperatura wyrażona w stopniach Fahrenheita.
# Argumenty funkcji możemy nazywać dowolnie.

# Krok 4. Składnia całej funkcji zawarta jest w nawiasach klamrowych {}. 
# Wewnątrz funkcji następuje przeliczenie wartości na stopnie Celsjusza.
# Polecenie return zwraca wartość funkcji.

# Krok 5. Uruchamiamy kod, aby zapisać funkcję.  


# Wywołajmy daną funkcję dla 212 stopni Fahrenheita:
fahrenheit_to_celsius(212)

# 212 stopni Fahrenheita to temperatura wrzenia wody, czyli 100 stopni Celsjusza



# Przykład 2. 
# W podobnie łatwy sposób można stworzyć funkcję 
# konwertującą stopnie Celsjusza na Kelvina.

celsius_to_kelvin = function(temp_C) {
  temp_K = temp_C + 273.15
  return(temp_K)
}

# Wywołajmy daną funkcję dla 0 stopni Celsjusza
celsius_to_kelvin(0)

# temperatura zamarzania wody to 273,15 stopni Kelvina.



# Używając tych dwóch utworzonych funkcji możemy utworzyć kolejną funkcję
# fahrenheit_to_kelvin, która będzie konwertować stopnie Fahrenheita na stopnie Kelvina.

fahrenheit_to_kelvin = function(temp_F) {
  temp_C = fahrenheit_to_celsius(temp_F)
  temp_K = celsius_to_kelvin(temp_C)
  return(temp_K)
}

# Wywołajmy tę funkcję dla 32 stopni Fahrenheita
fahrenheit_to_kelvin(32)



## ---- Zagnieżdżanie funkcji ----

# Zamiast łączenia funkcji w osobną funkcję,
# można też zagnieżdżać funkcje w sobie.
# Zatem stopnie Fahrenheita na stopnie Kelvina można przeliczyć tak:
celsius_to_kelvin(fahrenheit_to_celsius(32))



## ---- Argumenty funkcji ----


# Funkcja może zawierać więcej niż jeden argument
bmi = function(waga, wzrost) {
  bmi = waga / (wzrost ^ 2)
  return(bmi)
}

# Wywołajmy daną funkcję dla dwóch argumentów (waga i wzrost)
bmi(75, 1.81)



# Argument może mieć z góry przypisaną wartość
netto = function(brutto, podatek = 0.18) {
  netto = brutto * (1-podatek)
  return(netto)
}

# Wywołajmy funkcję
netto(10000)



## ---- Wyrażenia warunkowe ----

# Wyrażenia warunkowe to narzędzia do sterowania przepływem operacji,
# są podstawą każdego języka programowania.



## Instrukcja warunkowa IF ----

# W języku R składnia wygląda następująco:

x = 137
if (x > 100) {
  print("Więcej niż 100")
} 

# Wyrażenie if oczekuje, że warunek jest wektorem logicznym o długości jeden, 
# tj. takim który przyjmuje wartość TRUE lub FALSE. 

# Istnieje szereg sposobów uzyskania wartości TRUE lub FALSE w R, 
# jednym z nich jest zastosowanie porównania wartości.
# Test logiczny:
x > 100

# Jeżeli test logiczny zwróci wartość TRUE, 
# wtedy program wykona instrukcję zawartą w nawiasie.
# Jeśli test logiczny zwróci wartość FALSE, to nic się nie zadzieje.



## Instrukcja warunkowa IF ELSE ----

# Zamykamy pierwszą klamrę i po słowie else w kolejnym nawiasie klamerkowym 
# wpisujemy działanie, które ma zostać wykonane w przypadku, 
# jeśli nasz warunek logiczny nie zostanie spełniony.

x = 37
if (x > 100) {
  print("Więcej niż 100")
} else {
  print("Mniej niż 100")
}

# # Jeżeli test logiczny zwróci wartość TRUE, 
# wtedy program wykona instrukcję zawartą w pierwszej klamrze.
# Jeśli test logiczny zwróci wartość FALSE, 
# program wykona instrukcję zawartą po 'else'.



## Instrukcja warunkowa IF ELSE IF ----

# W tej instrukcji można zawrzeć kilka testów logicznych 
# za pomocą konstrukcji else if

x = 100
if (x > 100) {
  print("Więcej niż 100")
} else if (x == 100) {
  print("Dokładnie 100")
} else {
  print("Mniej niż 100")
}

# *Zwróć uwagę, że operator porównania to '==' a operator przypisania to '='



## ---- Operatory logiczne ----

# Operatory logiczne są kluczowe w instrukcjach warunkowych (if, else if, else) 
# gdyż decydują o przepływie sterowania programu.

# W języku R istnieją dwa rodzaje operatorów logicznych:
# podwójne (&& i ||) oraz pojedyncze (& i |)


# Podwójne (&& i ||)
# - porównują jeden/pierwszy element wektora i zwracają pojedynczą wartość logiczną,
# - stosowane są najczęściej w instrukcjach warunkowych 
# (interesuje nas wynik dla pojedynczego warunku logicznego)


# Pojedyncze (& i |)
# - są używane, gdy chcemy przeprowadzić operacje logiczne na całych wektorach

# Przykład: porównując dwa wektory logiczne za pomocą &
c(TRUE, FALSE, TRUE) & c(FALSE, FALSE, TRUE)
# otrzymamy wektor wyników c(FALSE, FALSE, TRUE)



# W PRAKTYCE to podwójne operatory (&& i ||) są stosowane w instrukcjach warunkowych!



# Przeanalizujmy przykład z wykorzystaniem operatora &&
x = 18
if (x <= 0) {
  print("mniejsze lub równe 0")
} else if (x > 0 && x <= 10) {
  print("między 1 a 10")
} else if (x > 10 && x <= 20) {
  print("między 11 a 20")
} else {
  print("więcej niż 20")
}


# Przeanalizujmy przykład z wykorzystaniem operatora ||
zwierze = "okoń"
if (zwierze == "kot" || zwierze == "pies") {
  print("Ssak")
} else if (zwierze == "okoń" || zwierze == "karp") {
  print("Ryba")
} else if (zwierze == "sokół" || zwierze == "orzeł") {
  print("Ptak")
} else {
  print("Zwierzę nieznane")
}

# Nasze zwierzę to ryba.




## ---- Połączenie funkcji i wyrażeń warunkowych ----

# Teraz wykorzystajmy funkcję oraz wyrażenia warunkowe 
# do stworzenia bardzo prostego algorytmu interpretującego wynik BMI

bmi2 = function(waga, wzrost) {
  bmi = waga / (wzrost ^ 2)
  if (bmi < 18.5) {
    tekst = c("Niedowaga", bmi)
  } else if (bmi >= 18.5 && bmi < 25) {
    tekst = c("Optimum", bmi)
  } else if (bmi >= 25 && bmi < 30) {
    tekst = c("Nadwaga", bmi)
  } else if (bmi >= 30) {
    tekst = c("Otyłość", bmi)
  }
  return(tekst)
}

# Wywołajmy funkcję
bmi2(74, 1.81)




###### ---- Zadania FUNKCJE I WYRAŻENIA WARUNKOWE ----

# 1. Stwórz funkcję o nazwie kostka, która będzie symulować n rzutów kostką.
# Wskazówka: Użyj funkcji sample() do losowania liczby oczek od 1 do 6.

# 2. Stwórz funkcję, która będzie tworzyć wektor o zadanej długości.
# Funkcja ma zwracać wektor liczb całkowitych od 1 do n:
#  długość wektora wynosi n, a wartości w wektorze to sekwencja liczb od 1 do n.

# 3. Stwórz funkcję o nazwie pole_kola, która oblicza pole powierzchni koła dla danego promienia.

# 4. Stwórz funkcję, która oblicza długość przeciwprostokątnej w trójkącie prostokątnym.

# 5. Stwórz funkcję będącą najprostszą wersją kalkulatora 
# (dodawanie, odejmowanie, mnożenie albo dzielenie dwóch liczb).

# 6. Stwórz funkcję o nazwie przyznaj_nagrode()
# która symuluje rzut sześcienną kostką do gry i przyznaje nagrodę w zależności od wyniku rzutu. 
# Funkcja powinna działać według następujących zasad:
# - Jeśli wyrzucona liczba oczek to 6, funkcja powinna zwrócić komunikat: "Super bonus!"
# - Jeśli wyrzucona liczba oczek to 4 lub 5, funkcja powinna zwrócić komunikat: "Nagroda standardowa"
# - Jeśli wyrzucona liczba oczek to 1, 2 lub 3, funkcja powinna zwrócić komunikat: "Brak nagrody..."

# 7. Stwórz funkcję obliczającą podatek w zależności od dochodu. 
# Przyjmij następujące założenia:
# a) Jeżeli podatnik rozlicza się liniowo, wtedy niezależnie od kwoty płaci 19% podatku.
# b) Jeżeli podatnik rozlicza się na zasadach ogólnych, wtedy:
# - poniżej kwoty 85528zł płaci 18% podatku minus kwota zmniejszająca, czyli 556zł;
# - powyżej kwoty 85528zł płaci 14839zł + 32% nadwyżki powyżej 85528zł.








