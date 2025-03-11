



# Słowniki sentymentu

library(tm)
library(tidyverse)
library(tidytext)
library(textdata)
library(ggplot2)
library(ggthemes)

# Pobierz słowniki (leksykony sentymentu) 
# w uporządkowanym formacie, gdzie każdemu słowu odpowiada jeden wiersz,
# - jest to forma, którą można połączyć z zestawem danych 
# zawierającym jedno słowo na wiersz.
# https://juliasilge.github.io/tidytext/reference/get_sentiments.html
#
# get_sentiments(lexicon = c("bing", "afinn", "nrc", "loughran"))



# Słownik Bing ----
# https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
get_sentiments("bing")

# Podsumowujemy słownik Bing, licząc wystąpienia słów
get_sentiments("bing") %>%
  count(sentiment)
# W słowniku Bing znajduje się ponad 4 tysiące negatywnych
# oraz ponad 2 tysiące pozytywnych terminów



# Słownik Afinn ----
# https://darenr.github.io/afinn/
get_sentiments("afinn")

# Podsumowujemy słownik Afinn, sprawdzając minimalną i maksymalną wartość
get_sentiments("afinn") %>%
  summarize(
    min = min(value),
    max = max(value)
  )
# Wartości sentymentu mieszczą się w przedziale od -5 do 5



# Słownik NRC ----
# https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
get_sentiments("nrc")

# Zliczmy liczbę słów powiązanych z każdym sentymentem w słowniku NRC
get_sentiments("nrc") %>% 
  count(sentiment) %>% 
  # Sortujemy liczebność sentymentów w kolejności malejącej
  arrange(desc(n))

# Pobieramy słownik NRC, liczymy sentymenty i sortujemy je według liczebności
sentiment_counts <- get_sentiments("nrc") %>% 
  count(sentiment) %>% 
  mutate(sentiment2 = fct_reorder(sentiment, n))

# Wizualizacja liczby wystąpień sentymentów 
# używając nowej kolumny typu factor o nazwie sentiment2
ggplot(sentiment_counts, aes(x=sentiment2, y=n)) +
  geom_col(fill="goldenrod1") +
  coord_flip() +
  # Wstawiamy tytuł, nazwę osi X jako "Sentyment" i osi Y jako "Liczba"
  labs(x = "Sentyment", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Kategorie sentymentu w NRC")



# Słownik Loughran ----
# dostępny w pakiecie "lexicon"
# https://emilhvitfeldt.github.io/textdata/reference/lexicon_loughran.html
get_sentiments("loughran")

# Podsumowujemy słownik Loughran w następujący sposób:
sentiment_counts <- get_sentiments("loughran") %>%
  count(sentiment) %>%
  mutate(sentiment2 = fct_reorder(sentiment, n))

ggplot(sentiment_counts, aes(x=sentiment2, y=n)) + 
  geom_col(fill="darkorchid3") +
  coord_flip() +
  labs(x = "Sentyment", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Kategorie sentymentu w Loughran")




