


# Instalacja i ładowanie wymaganych pakietów ----
# install.packages(c("tm", "wordcloud", "RColorBrewer", "ggplot2"))
library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)


# Stworzenie funkcji do przetwarzania tekstu ----
process_text <- function(file_path) {
  # Wczytanie tekstu z pliku
  text <- tolower(readLines(file_path, encoding = "UTF-8"))
  # Usunięcie znaków interpunkcyjnych i cyfr
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  # Usunięcie stop słów angielskich
  text <- removeWords(text, stopwords("en"))
  # Podział tekstu na słowa
  words <- unlist(strsplit(text, "\\s+"))
    # Usunięcie pustych elementów
  words <- words[words != ""]
  return(words)
  
}


# Stworzenie funkcji do obliczania częstości występowania słów ----
word_frequency <- function(words) {
  freq <- table(words)
  freq_df <- data.frame(word = names(freq), freq = as.numeric(freq))
  freq_df <- freq_df[order(-freq_df$freq), ]
  return(freq_df)
}

# Stworzenie funkcji do tworzenia chmury słów ----
plot_wordcloud <- function(freq_df, title, color_palette = "Dark2") {
  wordcloud(words = freq_df$word, freq = freq_df$freq, min.freq = 16,
            colors = brewer.pal(8, color_palette))
  title(title)
}


# Przykładowe użycie funkcji ----


# Jeden plik txt ----


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



# Potrzeba dodatkowych stop słów do usunięcia ----
custom_stopwords <- c("—", "–", "’s", "’re")


# Usunięcie dodatkowych stop słów z przetworzonego tekstu 
# za pomocą indeksowania logicznego
words <- words[!words %in% custom_stopwords]

# Obliczenie częstości występowania słów
freq_df <- word_frequency(words)

# Tworzenie chmury słów
plot_wordcloud(freq_df, "Chmura słów", "Dark2")

# Wyświetlenie 10 najczęściej występujących słów
print(head(freq_df, 10))




# Dwa pliki txt równocześnie ----


# Lista plików do wczytania
file_paths <- c("Biden2021.txt", "Biden2024.txt")  # Uzupełnij nazwy plików i ustaw Working Directory!


# Przetwarzanie każdego pliku osobno
for (file_path in file_paths) {
  # Wczytanie i przetworzenie tekstu
  words <- process_text(file_path)
  
  # Obliczenie częstości występowania słów
  freq_df <- word_frequency(words)
  
  # Tworzenie chmury słów
  plot_wordcloud(freq_df, paste("Chmura słów -", file_path), "Dark2")
  
  # Wyświetlenie 10 najczęściej występujących słów
  cat("Najczęściej występujące słowa w pliku", file_path, ":\n")
  print(head(freq_df, 10))
  cat("\n")
}




# Potrzeba dodatkowych stop słów do usunięcia ----
custom_stopwords <- c("—", "–", "’s", "’re")

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









