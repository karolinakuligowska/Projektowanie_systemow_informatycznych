



# Analiza sentymentu

library(tm)
library(tidyverse)
library(tidytext)
library(textdata)
library(ggplot2)
library(ggthemes)



# Jeden cały plik ----

# Wczytanie danych tekstowych
# Odczytanie lokalnego pliku .txt
text <- readLines(file.choose(), encoding="UTF-8")


docs <- VCorpus(VectorSource(text))
tdm <- TermDocumentMatrix(docs)
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing=TRUE)


tokeny <- data.frame(Review = names(v), freq = v, stringsAsFactors = F)
tokeny_data <- as_tibble(tokeny)




#-----------------------------Przed analizą sentymentu ----

# Tokenizacja tekstu przy użyciu pakietu tidytext
# library(tidytext)

# Użycie unnest_tokens()
tidy_tokeny <- tokeny_data %>%
  unnest_tokens(word, Review) # Tworzenie (word) z (Review) 

# unnest_tokens() wykonuje czyszczenie
# Usuwa interpunkcję i białe znaki, zamienia tekst na małe litery itp.
# tidy_tokeny
head(tidy_tokeny, 10)


# Najczęściej występujące słowa to: the, it, and, to...
# Musimy wykonać dodatkowe czyszczenie
# z użyciem listy stopwords

# Użycie anti_join()
# 
# Wiersz ramki danych po lewej stronie zostaje zachowany,
# jeśli wartość w dopasowanej kolumnie NIE jest 
# obecna w ramce danych po prawej stronie


# Użycie unnest_tokens() z usuwaniem stopwords
tidy_tokeny2 <- tokeny_data %>%
  unnest_tokens(word, Review) %>%
  anti_join(stop_words)

# Liczba słów została zmniejszona
# tidy_tokeny2
head(tidy_tokeny2, 10)

# Sprawdzenie najczęściej występujących słów
# które rzeczywiście odzwierciedlają treść



# Analiza sentymentu ----



# Analiza sentymentu przy użyciu słownika Loughran ----

# Użycie inner_join()
tidy_tokeny %>%
  inner_join(get_sentiments("loughran"), relationship = "many-to-many")
# Liczba słów drastycznie się zmniejszyła,
# ponieważ inner_join zachował tylko te słowa,
# które występowały w słowniku


# Zliczanie sentymentu
sentiment_review <- tidy_tokeny %>%
  inner_join(get_sentiments("loughran"), relationship = "many-to-many")

sentiment_review %>%
  count(sentiment)

# Zliczanie, które słowa są najczęstsze
# dla danego sentymentu
sentiment_review %>%
  count(word, sentiment) %>%
  arrange(desc(n))


# Filtrowanie analizy sentymentu
# i pozostawienie tylko słów
# o sentymencie pozytywnym lub negatywnym

sentiment_review2 <- sentiment_review %>%
  filter(sentiment %in% c("positive", "negative"))


word_counts <- sentiment_review2 %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(
    word2 = fct_reorder(word, n)
  )

# Wizualizacja sentymentu
ggplot(word_counts, aes(x=word2, y=n, fill=sentiment)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free") +
  coord_flip() +
  labs(x = "Słowa", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Liczba słów wg sentymentu (Loughran)") +
  scale_fill_manual(values = c("firebrick", "darkolivegreen4"))



# Analiza sentymentu przy użyciu słownika NRC ----


# Zliczanie sentymentu
sentiment_review_nrc <- tidy_tokeny %>%
  inner_join(get_sentiments("nrc"), relationship = "many-to-many")

sentiment_review_nrc %>%
  count(sentiment)

# Zliczanie, które słowa są najczęstsze
# dla danego sentymentu
sentiment_review_nrc %>%
  count(word, sentiment) %>%
  arrange(desc(n))


# Filtrowanie analizy sentymentu
# i pozostawienie tylko słów
# o sentymencie pozytywnym lub negatywnym

sentiment_review_nrc2 <- sentiment_review_nrc %>%
  filter(sentiment %in% c("positive", "negative"))


word_counts_nrc2 <- sentiment_review_nrc2 %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(
    word2 = fct_reorder(word, n)
  )

# Wizualizacja sentymentu
ggplot(word_counts_nrc2, aes(x=word2, y=n, fill=sentiment)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free") +
  coord_flip() +
  labs(x = "Słowa", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Liczba słów wg sentymentu (NRC)")



# Analiza sentymentu przy użyciu słownika Bing ----


# Zliczanie sentymentu
sentiment_review_bing <- tidy_tokeny %>%
  inner_join(get_sentiments("bing"))

sentiment_review_bing %>%
  count(sentiment)

# Zliczanie, które słowa są najczęstsze
# dla danego sentymentu
sentiment_review_bing %>%
  count(word, sentiment) %>%
  arrange(desc(n))


# Filtrowanie analizy sentymentu
# i pozostawienie tylko słów
# o sentymencie pozytywnym lub negatywnym

sentiment_review_bing2 <- sentiment_review_bing %>%
  filter(sentiment %in% c("positive", "negative"))


word_counts_bing2 <- sentiment_review_bing2 %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(
    word2 = fct_reorder(word, n)
  )

# Wizualizacja sentymentu
ggplot(word_counts_bing2, aes(x=word2, y=n, fill=sentiment)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free") +
  coord_flip() +
  labs(x = "Słowa", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Liczba słów wg sentymentu (Bing)") +
  scale_fill_manual(values = c("dodgerblue4", "goldenrod1"))



# Analiza sentymentu przy użyciu słownika Afinn ----


# Zliczanie sentymentu
sentiment_review_afinn <- tidy_tokeny %>%
  inner_join(get_sentiments("afinn"))

sentiment_review_afinn %>%
  count(value)

# Zliczanie, które słowa są najczęstsze
# dla danego sentymentu
sentiment_review_afinn %>%
  count(word, value) %>%
  arrange(desc(n))


# Silnie pozytywne lub silnie negatywne słowa:
# filtrowanie analizy sentymentu
# i pozostawienie tylko słów o wartości w zakresie od -5 do 5

sentiment_review_afinn3 <- sentiment_review_afinn %>%
  filter(value %in% c("3", "-3" , "4", "-4", "5", "-5"))


word_counts_afinn3 <- sentiment_review_afinn3 %>%
  count(word, value) %>%
  group_by(value) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(
    word2 = fct_reorder(word, n)
  )

# Wizualizacja sentymentu
ggplot(word_counts_afinn3, aes(x=word2, y=n, fill=value)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~value, scales="free") +
  coord_flip() +
  labs(x = "Słowa", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Liczba słów wg sentymentu (AFINN)")











