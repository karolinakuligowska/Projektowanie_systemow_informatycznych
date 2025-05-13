


# Wymagane pakiety
library(tm)



# 1. Dane wejściowe ----
docs <- c("Jupiter ist the largest planet",
          "Mars ist the fourth planet from the Sun")



# Utworzenie korpusu tekstowego
corpus <- VCorpus(VectorSource(docs))



# Stworzenie macierzy częstości Term-Document Matrix
tdm <- TermDocumentMatrix(corpus)



# Konwersja do macierzy
tdm_m <- as.matrix(tdm)
tdm_m


colnames(tdm_m) <- c("A", "B")



# 2. Obliczenie TF ----


# Obliczenie TF dla słów w A i B
TF_A = round(tdm_m[, 1] / sum(tdm_m[,1]), 3)
TF_B = round(tdm_m[, 2] / sum(tdm_m[,2]), 3)

tdm_m <- cbind(tdm_m, TF_A, TF_B)


tdm_m



# 3. Obliczenie IDF ----


# Obliczenie IDF dla słów w całym korpusie
N <- length(docs)  # liczba dokumentów N
N


# Liczba dokumentów zawierających każde słowo
w <- rowSums(tdm_m[, 1:2] > 0)
w


N/w


IDF <- round(log(N/w), 3)


# Dodanie IDF do macierzy
tdm_m <- cbind(tdm_m, IDF)


tdm_m



# 4. Obliczenie TF-IDF ----


TFIDF_A <- round((tdm_m[, "TF_A"] * tdm_m[, "IDF"]), 3)
TFIDF_B <- round((tdm_m[, "TF_B"] * tdm_m[, "IDF"]), 3)


# Dodanie TFIDF do macierzy
tdm_m <- cbind(tdm_m, TFIDF_A, TFIDF_B)

tdm_m



# 5. Macierz częstości z wagami TF-IDF ----


tdm_tfidf <- tdm_m[ , 6:7]
tdm_tfidf



# Porównaj z implementacją TF-IDF 
# w funkcji TermDocumentMatrix()


# Macierz częstości TDM z TF-IDF ----


tdm_tfidf2 <- TermDocumentMatrix(corpus,
                                control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))


tdm_tfidf2 <- as.matrix(tdm_tfidf2)
tdm_tfidf2



# Uwaga!
# Wyniki tdm_tfidf oraz tdm_tfidf2 różnią się, gdyż:
#
# implementacja TF-IDF w funkcji TermDocumentMatrix()
#
#
# 1) dla TF przyjmuje zwykłe częstości
#
# Obliczenie TF dla słów w A i B
tf_a = tdm_m[, 1]
tf_b = tdm_m[, 2] 

#
# 2) dla IDF stosuje logarytm o podstawie 2
#
idf <- round(log2(N/w), 3)

# Wówczas wynik byłby:
tfidf_a = tf_a * idf
tfidf_b = tf_b * idf


tdm_tfidf <- cbind(tdm_tfidf, tfidf_a, tfidf_b)

# Teraz porównaj
tdm_tfidf

# Z implementacją TF-IDF w funkcji TermDocumentMatrix()
tdm_tfidf2














