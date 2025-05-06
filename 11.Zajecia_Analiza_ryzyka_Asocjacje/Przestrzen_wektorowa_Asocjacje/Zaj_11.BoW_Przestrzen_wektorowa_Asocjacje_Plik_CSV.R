


#' ---
#' title: "Asocjacje"
#' author: " "
#' date:   " "
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: readable      # Wygląd (bootstrap, cerulean, darkly, journal, lumen, paper, readable, sandstone, simplex, spacelab, united, yeti)
#'     highlight: kate      # Kolorowanie składni (haddock, kate, espresso, breezedark)
#'     toc: true            # Spis treści
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: show    
#'     number_sections: false # Numeruje nagłówki (lepsza nawigacja)
#' ---


knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)





#' # Wymagane pakiety
# Wymagane pakiety ----
library(tm)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(ggthemes)



#' # Dane tekstowe
# Dane tekstowe ----

# Ustaw Working Directory!
# Załaduj dokumenty z folderu
# docs <- DirSource("textfolder2")
# W razie potrzeby dostosuj ścieżkę
# np.: docs <- DirSource("C:/User/Documents/textfolder2")


# Utwórz korpus dokumentów tekstowych

# Gdy tekst znajduje się w jednym pliku csv:
data <- read.csv("LOT_reviews.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
corpus <- VCorpus(VectorSource(data$Review_Text))


# Korpus
# inspect(corpus)


# Korpus - zawartość przykładowego elementu
corpus[[1]]
corpus[[1]][[1]]
corpus[[1]][2]



#' # 1. Przetwarzanie i oczyszczanie tekstu
# 1. Przetwarzanie i oczyszczanie tekstu ----
# (Text Preprocessing and Text Cleaning)


# Normalizacja i usunięcie zbędnych znaków ----

# Zapewnienie kodowania w całym korpusie
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))


# Funkcja do zamiany znaków na spację
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))


# Usuń zbędne znaki lub pozostałości url, html itp.

# symbol @
corpus <- tm_map(corpus, toSpace, "@")

# symbol @ ze słowem (zazw. nazwa użytkownika)
corpus <- tm_map(corpus, toSpace, "@\\w+")

# linia pionowa
corpus <- tm_map(corpus, toSpace, "\\|")

# tabulatory
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")

# CAŁY adres URL:
corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")

# http i https
corpus <- tm_map(corpus, toSpace, "http\\w*")

# tylko ukośnik odwrotny (np. po http)
corpus <- tm_map(corpus, toSpace, "/")

# pozostałość po re-tweecie
corpus <- tm_map(corpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")

# inne pozostałości
corpus <- tm_map(corpus, toSpace, "www")
corpus <- tm_map(corpus, toSpace, "~")
corpus <- tm_map(corpus, toSpace, "â€“")


# Sprawdzenie
corpus[[1]][[1]]

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)


# Sprawdzenie
corpus[[1]][[1]]

# usunięcie ewt. zbędnych nazw własnych
corpus <- tm_map(corpus, removeWords, c("flight", "lot"))

corpus <- tm_map(corpus, stripWhitespace)

# Sprawdzenie
corpus[[1]][[1]]



# Decyzja dotycząca korpusu ----
# do dalszej analizy użyj:
#
# - corpus (oryginalny, bez stemmingu)
#




#' # Tokenizacja
# Tokenizacja ----



# Macierz częstości TDM ----

tdm <- TermDocumentMatrix(corpus)
tdm_m <- as.matrix(tdm)



#' # 2. Zliczanie częstości słów
# 2. Zliczanie częstości słów ----
# (Word Frequency Count)


# Zlicz same częstości słów w macierzach
v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)
head(tdm_df, 10)



#' # 3. Eksploracyjna analiza danych
# 3. Eksploracyjna analiza danych ----
# (Exploratory Data Analysis, EDA)


# Chmura słów (globalna)
wordcloud(words = tdm_df$word, freq = tdm_df$freq, min.freq = 7, 
          colors = brewer.pal(8, "Dark2"))


# Wyświetl top 10
print(head(tdm_df, 10))



#' # 4. Inżynieria cech w modelu Bag of Words:
#' # Reprezentacja słów i dokumentów w przestrzeni wektorowej
# 4. Inżynieria cech w modelu Bag of Words: ----
# Reprezentacja słów i dokumentów w przestrzeni wektorowej ----
# (Feature Engineering in vector-space BoW model)

# - podejście surowych częstości słów
# (częstość słowa = liczba wystąpień w dokumencie)
# (Raw Word Counts)



#' # Asocjacje - znajdowanie współwystępujących słów
# Asocjacje - znajdowanie współwystępujących słów ----



# Funkcja findAssoc() w pakiecie tm służy do:
# - znajdowania słów najbardziej skorelowanych z danym terminem w macierzy TDM/DTM
# - wykorzystuje korelację Pearsona między wektorami słów
# - jej działanie nie opiera się na algorytmach machine learning


# Samodzielnie wytypuj słowa (terminy), 
# które chcesz zbadać pod kątem asocjacji


findAssocs(tdm,"service",0.5)
findAssocs(tdm,"experience",0.5)
findAssocs(tdm,"passengers",0.5)
findAssocs(tdm,"staff",0.5)
findAssocs(tdm,"crews",0.5)
findAssocs(tdm,"prices",0.5)



#' # Wizualizacja asocjacji
# Wizualizacja asocjacji ----


# Wytypowane słowo i próg asocjacji
target_word <- "crews"
cor_limit <- 0.4


# Oblicz asocjacje dla tego słowa
associations <- findAssocs(tdm, target_word, corlimit = cor_limit)
assoc_vector <- associations[[target_word]]
assoc_sorted <- sort(assoc_vector, decreasing = TRUE)


# Ramka danych
assoc_df <- data.frame(
  word = factor(names(assoc_sorted), levels = names(assoc_sorted)[order(assoc_sorted)]),
  score = assoc_sorted
)


# Wykres lizakowy (lollipop chart)
# stosowany w raportach biznesowych i dashboardach:
ggplot(assoc_df, aes(x = score, y = reorder(word, score))) +
  geom_segment(aes(x = 0, xend = score, y = word, yend = word), color = "#a6bddb", size = 1.2) +
  geom_point(color = "#0570b0", size = 4) +
  geom_text(aes(label = round(score, 2)), hjust = -0.3, size = 3.5, color = "black") +
  scale_x_continuous(limits = c(0, max(assoc_df$score) + 0.1), expand = expansion(mult = c(0, 0.2))) +
  theme_minimal(base_size = 12) +
  labs(
    title = paste0("Asocjacje z terminem: '", target_word, "'"),
    subtitle = paste0("Próg r ≥ ", cor_limit),
    x = "Współczynnik korelacji Pearsona",
    y = "Słowo"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )



# Wykres lizakowy z natężeniem
# na podstawie wartości korelacji score:
ggplot(assoc_df, aes(x = score, y = reorder(word, score), color = score)) +
  geom_segment(aes(x = 0, xend = score, y = word, yend = word), size = 1.2) +
  geom_point(size = 4) +
  geom_text(aes(label = round(score, 2)), hjust = -0.3, size = 3.5, color = "black") +
  scale_color_gradient(low = "#a6bddb", high = "#08306b") +
  scale_x_continuous(
    limits = c(0, max(assoc_df$score) + 0.1),
    expand = expansion(mult = c(0, 0.2))
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = paste0("Asocjacje z terminem: '", target_word, "'"),
    subtitle = paste0("Próg r ≥ ", cor_limit),
    x = "Współczynnik korelacji Pearsona",
    y = "Słowo",
    color = "Natężenie\nskojarzenia"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "right"
  )




