


# Wymagane pakiety ----
library(tm)
library(tidytext)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(SnowballC)




# 0. Funkcja do przetwarzania tekstu z apostrofami i stemmingiem ----
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
  
  
  # Stemming i stem completion
  stemmed_doc <- stemDocument(words)
  completed_doc <- stemCompletion(stemmed_doc, dictionary=words, type="prevalent")
  
  # Usunięcie pustych elementów
  completed_doc <- completed_doc[completed_doc != ""]
  
  # Zwróć wynik końcowy (po stemCompletion)
  return(completed_doc)
  
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





# 1. Jeden plik txt z ulepszoną funkcją i stemmingiem ----


# Wczytanie i przetworzenie tekstu
# file_path <- "sciezka/do/pliku.txt"  <= Uzupełnij nazwę pliku i ustaw Working Directory!
file_path <- "Biden2021.txt"
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





# 2. Więcej plików txt z ulepszoną funkcją i stemmingiem ----


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









