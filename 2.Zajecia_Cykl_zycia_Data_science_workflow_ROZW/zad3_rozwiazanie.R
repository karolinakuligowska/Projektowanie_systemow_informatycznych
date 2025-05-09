


# Zgodnie z etapami cyklu życia systemu informatycznego

# 1. Planowanie i 2 analiza wymagań, 3 projektowanie systemu
# Określenie zakresu analizy tekstu oraz metod przetwarzania danych.
# Przygotowanie metod analizy tekstu i wizualizacji wyników.

# 4. Implementacja i kodowanie

# Wczytaj dane tekstowe
# Wczytaj plik tekstowy z lokalnego dysku
text <- readLines(file.choose(), encoding="UTF-8")

# text

library(qdap)
frequent_terms <- freq_terms(text)
frequent_terms
frequent_terms <- freq_terms(text, stopwords = Top200Words)
plot(frequent_terms)

# 5. Testowanie i weryfikacja
# Sprawdzenie wyników analizy i poprawności danych.

# 6. Wdrożenie i wizualizacja wyników
# Tworzenie chmury słów ----

# Tworzenie chmury słów za pomocą pakietu wordcloud
# install.packages("wordcloud")
library(wordcloud)

# Utwórz chmurę słów
wordcloud(frequent_terms$WORD, frequent_terms$FREQ)

# Opcje chmury słów
# ?wordcloud

# Zmiana wartości min.freq i max.words w celu wyświetlenia mniejszej/większej liczby słów.
# min.freq: słowa o częstości poniżej tej wartości nie będą wyświetlane
# max.words: maksymalna liczba słów do wyświetlenia

# Ograniczenie liczby słów w chmurze poprzez określenie minimalnej częstości
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4)

# Ograniczenie liczby słów w chmurze poprzez określenie maksymalnej liczby słów
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, max.words = 5)

# Optymalizacja i dostosowanie wyników
# Dodanie koloru do chmury słów dla lepszej wizualizacji

wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(8,"Dark2"))

wordcloud(frequent_terms$WORD, frequent_terms$FREQ, max.words = 5, colors = brewer.pal(8,"Accent"))

# ?brewer.pal
# brewer.pal.info

# Dodanie różnych palet kolorystycznych
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(9,"Blues"))
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(9,"Reds"))
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(9,"Greens"))





