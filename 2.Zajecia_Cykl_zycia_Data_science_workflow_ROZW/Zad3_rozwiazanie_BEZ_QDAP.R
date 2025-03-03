


# Wymagane pakiety
# install.packages(c("tm", "tidytext", "dplyr", "ggplot2", "wordcloud", "RColorBrewer"))
library(tm)
library(tidytext)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

# Wczytaj dane tekstowe
# Wybierz plik tekstowy z dysku
text <- readLines(file.choose(), encoding="UTF-8")


# Przetwarzanie tekstu ----

# Konwersja na ramkę danych
text_df <- data.frame(line = 1:length(text), text = text, stringsAsFactors = FALSE)

# Tokenizacja tekstu (rozbicie na pojedyncze słowa)
tidy_text <- text_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")  # Usunięcie stop słów

# Analiza częstości słów ----

# Obliczenie częstości występowania słów
frequent_terms <- tidy_text %>%
  count(word, sort = TRUE)


# Wizualizacja częstości słów ----

# Wykres słupkowy 10 najczęściej występujących słów
ggplot(frequent_terms %>% top_n(10), aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Słowo", y = "Częstość") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Najczęściej występujące słowa")

# Tworzenie chmury słów ----
# Chmura słów z domyślnymi parametrami
wordcloud(words = frequent_terms$word, freq = frequent_terms$n)

# Opcje chmury słów ----
# Ograniczenie liczby słów w chmurze poprzez określenie minimalnej częstości
wordcloud(words = frequent_terms$word, freq = frequent_terms$n, min.freq = 4)

# Ograniczenie liczby słów w chmurze poprzez określenie maksymalnej liczby słów
wordcloud(words = frequent_terms$word, freq = frequent_terms$n, max.words = 5)

# Dodanie koloru do chmury słów
wordcloud(words = frequent_terms$word, freq = frequent_terms$n, min.freq = 4, colors = brewer.pal(8, "Dark2"))

# Dodanie różnych palet kolorystycznych
wordcloud(words = frequent_terms$word, freq = frequent_terms$n, min.freq = 4, colors = brewer.pal(9, "Blues"))
wordcloud(words = frequent_terms$word, freq = frequent_terms$n, min.freq = 4, colors = brewer.pal(9, "Reds"))
wordcloud(words = frequent_terms$word, freq = frequent_terms$n, min.freq = 4, colors = brewer.pal(9, "Greens"))

