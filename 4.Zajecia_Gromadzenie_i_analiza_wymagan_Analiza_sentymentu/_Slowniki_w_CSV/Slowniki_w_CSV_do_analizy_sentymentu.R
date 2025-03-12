



# Słowniki sentymentu

library(tm)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(ggthemes)

# Pobierz słowniki (leksykony sentymentu) 
# w uporządkowanym formacie, gdzie każdemu słowu odpowiada jeden wiersz,
# - jest to forma, którą można połączyć z zestawem danych 
# zawierającym jedno słowo na wiersz.
# https://juliasilge.github.io/tidytext/reference/get_sentiments.html
#


# Wczytaj słowniki z plików csv ----
afinn <- read.csv("afinn.csv", stringsAsFactors = FALSE)
bing <- read.csv("bing.csv", stringsAsFactors = FALSE)
loughran <- read.csv("loughran.csv", stringsAsFactors = FALSE)
nrc <- read.csv("nrc.csv", stringsAsFactors = FALSE)



# Słownik Bing ----
# https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
head(bing)


# Podsumowujemy słownik Bing, licząc wystąpienia słów
bing %>%
  count(sentiment)
# W słowniku Bing znajduje się ponad 4 tysiące negatywnych
# oraz ponad 2 tysiące pozytywnych terminów



# Słownik Afinn ----
# https://darenr.github.io/afinn/
head(afinn)

# Podsumowujemy słownik Afinn, sprawdzając minimalną i maksymalną wartość
afinn %>%
  summarize(
    min = min(value),
    max = max(value)
  )
# Wartości sentymentu mieszczą się w przedziale od -5 do 5



# Słownik NRC ----
# https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
head(nrc)

# Zliczmy liczbę słów powiązanych z każdym sentymentem w słowniku NRC
nrc %>% 
  count(sentiment) %>% 
  # Sortujemy liczebność sentymentów w kolejności malejącej
  arrange(desc(n))

# Pobieramy słownik NRC, liczymy sentymenty i sortujemy je według liczebności
sentiment_counts <- nrc %>% 
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
# https://emilhvitfeldt.github.io/textdata/reference/lexicon_loughran.html
head(loughran)

# Podsumowujemy słownik Loughran w następujący sposób:
sentiment_counts <- loughran %>%
  count(sentiment) %>%
  mutate(sentiment2 = fct_reorder(sentiment, n))

ggplot(sentiment_counts, aes(x=sentiment2, y=n)) + 
  geom_col(fill="darkorchid3") +
  coord_flip() +
  labs(x = "Sentyment", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Kategorie sentymentu w Loughran")




