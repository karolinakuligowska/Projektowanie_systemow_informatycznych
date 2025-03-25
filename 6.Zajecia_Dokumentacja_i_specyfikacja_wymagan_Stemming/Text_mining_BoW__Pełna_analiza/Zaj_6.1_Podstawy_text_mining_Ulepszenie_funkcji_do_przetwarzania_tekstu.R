


# Wymagane pakiety ----
library(tm)
library(tidytext)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)


# Porównaj zestawy stop words:

# z pakietu tm
tm_stopwords <- stopwords("en")
tm_stopwords


# z pakietu tidytext
tidy_stopwords <- stop_words
tidy_stopwords



# 0. Funkcja do przetwarzania tekstu ----
process_text <- function(file_path) {
  # Wczytanie tekstu z pliku
  text <- tolower(readLines(file_path, encoding = "UTF-8"))
  # Usunięcie znaków interpunkcyjnych i cyfr
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  # Podział tekstu na słowa
  words <- unlist(strsplit(text, "\\s+"))
  # Usunięcie pustych elementów
  words <- words[words != ""]
  
  # Usunięcie stopwords z pakietu tidytext
  tidy_stopwords <- stop_words$word
  words <- words[!(words %in% tidy_stopwords)]
  
  # Usunięcie stopwords z pakietu tm
  tm_stopwords <- stopwords("en")
  words <- words[!(words %in% tm_stopwords)]
  
  return(words)
}



# 0. Funkcja do obliczania częstości występowania słów ----
word_frequency <- function(words) {
  freq <- table(words)
  freq_df <- data.frame(word = names(freq), freq = as.numeric(freq))
  freq_df <- freq_df[order(-freq_df$freq), ]
  return(freq_df)
}



# 0. Funkcja do tworzenia chmury słów ----
plot_wordcloud <- function(freq_df, title, color_palette = "Dark2") {
  wordcloud(words = freq_df$word, freq = freq_df$freq, min.freq = 16,
            colors = brewer.pal(8, color_palette))
  title(title)
}





# 0.a) Jeden plik txt ----


# Wczytanie i przetworzenie tekstu
# file_path <- "sciezka/do/pliku.txt"  <= Uzupełnij nazwę pliku i ustaw Working Directory!
file_path <- "Biden2021.txt"
words <- process_text(file_path)


# Obliczenie częstości występowania słów
freq_df <- word_frequency(words)


# Tworzenie chmury słów
plot_wordcloud(freq_df, "Chmura słów", "Dark2")


# Wyświetlenie 10 najczęściej występujących słów
print(head(freq_df, 10))



# 0.b) Potrzeba dodatkowych stop słów do usunięcia ----
custom_stopwords <- c("—", "–", "it", "and", "$")


# Usunięcie dodatkowych stop słów z przetworzonego tekstu 
# za pomocą indeksowania logicznego
words <- words[!words %in% custom_stopwords]
head(words, 10)

# Obliczenie częstości występowania słów
freq_df <- word_frequency(words)

# Tworzenie chmury słów
plot_wordcloud(freq_df, "Chmura słów", "Dark2")

# Wyświetlenie 10 najczęściej występujących słów
print(head(freq_df, 10))



# Nadal widać słowa z apostrofem
# np. it's, we're oraz let's

# Dane tekstowe mogą zawierać różne rodzaje apostrofów:
# 
# klasyczny: ' (U+0027)
# 
# typograficzny prawy: ’ (U+2019)
# 
# typograficzny lewy: ‘ (U+2018)

# Sprawdzenie:
stringi::stri_escape_unicode(words)

stringi::stri_escape_unicode(tm_stopwords)

stringi::stri_escape_unicode(stop_words$word)



# 0.c) Ulepszenie funkcji do przetwarzania tekstu z apostrofami ----
process_text <- function(file_path) {
  # Wczytanie tekstu z pliku
  text <- tolower(readLines(file_path, encoding = "UTF-8"))
  
  # Zamiana wszystkich rodzajów apostrofów na klasyczny '
  text <- gsub("[\u2019\u2018\u0060\u00B4]", "'", text)
  
  
  # Usunięcie cyfr (interpunkcja za chwilę)
  text <- removeNumbers(text)
  # Podział tekstu na słowa
  words <- unlist(strsplit(text, "\\s+"))
  # Usunięcie pustych elementów
  words <- words[words != ""]
  
  
  # Usunięcie słów zawierających apostrof (czyli skróty typu I'm, I've, don't)
  words <- words[!str_detect(words, "'")]
  
  
  
  # Usunięcie interpunkcji PRZED porównaniem do stopwords
  words <- str_replace_all(words, "[[:punct:]]", "")
  words <- words[words != ""]
  
  
  # Usunięcie spacji z początka i końca słów
  words <- str_trim(words)
  
  
  # Przygotowanie i usuniecie stopwords (jednolite apostrofy)
  
    # stopwords z pakietu tidytext i jednolite apostrofy
  tidy_stopwords <- tolower(stop_words$word)
  tidy_stopwords <- gsub("[\u2019\u2018\u0060\u00B4]", "'", tidy_stopwords)
  words <- words[!(words %in% tidy_stopwords)]
  
  # stopwords z pakietu tm i jednolite apostrofy
  tm_stopwords <- tolower(stopwords("en"))
  tm_stopwords <- gsub("[\u2019\u2018\u0060\u00B4]", "'", tm_stopwords)
  words <- words[!(words %in% tm_stopwords)]
  
  
  return(words)
}




# 1. Jeden plik txt z ulepszoną funkcją do przetwarzania tekstu ----


# Wczytanie i przetworzenie tekstu
words <- process_text(file_path)

# Ponownie potrzeba dodatkowych stop słów do usunięcia
custom_stopwords <- c("$")


# Usunięcie dodatkowych stop słów z przetworzonego tekstu 
# za pomocą indeksowania logicznego
words <- words[!words %in% custom_stopwords]
head(words, 10)

# Obliczenie częstości występowania słów
freq_df <- word_frequency(words)

# Tworzenie chmury słów
plot_wordcloud(freq_df, "Chmura słów", "Dark2")

# Wyświetlenie 10 najczęściej występujących słów
print(head(freq_df, 10))





# 2. Więcej plików txt z ulepszoną funkcją do przetwarzania tekstu ----


# Lista plików do wczytania
file_paths <- c("Biden2021.txt", "Biden2024.txt", "Trump2025.txt")  # Uzupełnij nazwy plików i ustaw Working Directory!



# Ponownie potrzeba dodatkowych stop słów do usunięcia
custom_stopwords <- c("$")


# Przetwarzanie każdego pliku osobno
for (file_path in file_paths) {
  # Wczytanie i przetworzenie tekstu
  words <- process_text(file_path)
  
  # Usunięcie dodatkowych stop słów z przetworzonego tekstu
  # za pomocą indeksowania logicznego
  words <- words[!words %in% custom_stopwords]
  
  # Obliczenie częstości występowania słów
  freq_df <- word_frequency(words)
  
  # Tworzenie chmury słów
  plot_wordcloud(freq_df, paste("Chmura słów -", file_path), "Dark2")
  
  # Wyświetlenie 10 najczęściej występujących słów
  cat("Najczęściej występujące słowa w pliku", file_path, ":\n")
  print(head(freq_df, 10))
  cat("\n")
}










