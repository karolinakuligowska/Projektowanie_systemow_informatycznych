


#' ---
#' title: "Klasyfikacja"
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
library(RColorBrewer)
library(dplyr)
library(e1071)  # pakiet do SVM
library(SnowballC)



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



#' # Stemming
# Stemming ----

# zachowaj kopię korpusu 
# do użycia jako dictionary w uzupełnianiu rdzeni
corpus_copy <- corpus

# wykonaj stemming w korpusie
corpus_stemmed <- tm_map(corpus, stemDocument)


# Sprawdzenie
corpus[[1]][[1]]
# Sprawdzenie
corpus_stemmed[[1]][[1]]



# Uzupełnienie rdzeni słów po stemmingu ----

# funkcja pomocnicza: wykonuje stemCompletion linia po linii
complete_stems <- content_transformer(function(x, dict) {
  x <- unlist(strsplit(x, " "))                  # podziel na słowa
  x <- stemCompletion(x, dictionary = corpus_copy, type="longest") # uzupełnij rdzenie
  paste(x, collapse = " ")                       # połącz z powrotem w tekst
})

# wykonaj stemCompletion do każdego dokumentu w korpusie
corpus_completed <- tm_map(corpus_stemmed, complete_stems, dict = corpus_copy)

# usuń NA
corpus_completed <- tm_map(corpus_completed, toSpace, "NA")
corpus_completed <- tm_map(corpus_completed, stripWhitespace)


# Sprawdzenie
corpus_completed[[1]][[1]]




#' # Decyzja dotycząca korpusu
# Decyzja dotycząca korpusu ----
# Należy w tym momencie rozważyć, 
# który obiekt użyć do dalszej analizy:
#
# - corpus (oryginalny, bez stemmingu)
# - corpus_stemmed (po stemmingu)
# - corpus_completed (uzupełnione rdzenie)




#' # Tokenizacja
# Tokenizacja ----



#' # A. Macierz częstości TDM ----
# A. Macierz częstości TDM ----

tdm <- TermDocumentMatrix(corpus_completed)

tdm
# inspect(tdm)


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



#' # B. Macierz częstości TDM z TF-IDF ----
# B. Macierz częstości TDM z TF-IDF ----

tdm_tfidf <- TermDocumentMatrix(corpus_completed,
                                control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))

tdm_tfidf
# inspect(tdm_tfidf)


tdm_tfidf_m <- as.matrix(tdm_tfidf)


#' # 2. Zliczanie częstości słów
# 2. Zliczanie częstości słów ----
# (Word Frequency Count)


# Zlicz same częstości słów w macierzach
v_tfidf <- sort(rowSums(tdm_tfidf_m), decreasing = TRUE)
tdm_tfidf_df <- data.frame(word = names(v_tfidf), freq = v_tfidf)
head(tdm_tfidf_df, 10)



#' # 3. Eksploracyjna analiza danych
# 3. Eksploracyjna analiza danych ----
# (Exploratory Data Analysis, EDA)


# Chmura słów (globalna)
wordcloud(words = tdm_tfidf_df$word, freq = tdm_tfidf_df$freq, min.freq = 7, 
          colors = brewer.pal(8, "Dark2"))


# Wyświetl top 10
print(head(tdm_tfidf_df, 10))



#' # 4. Inżynieria cech w modelu Bag of Words:
#' # Reprezentacja słów i dokumentów w przestrzeni wektorowej
# 4. Inżynieria cech w modelu Bag of Words: ----
# Reprezentacja słów i dokumentów w przestrzeni wektorowej ----
# (Feature Engineering in vector-space BoW model)

# - podejście TF-IDF
# (waga słowa = znormalizowana częstość słowa w dokumencie 
# oraz rzadkość słowa w korpusie dokumentów)
# (Term Frequency-Inverse Document Frequency)




# Użyj utworzonej wcześniej macierzy:
# B. Macierz częstości TDM z TF-IDF
tdm_tfidf

inspect(tdm_tfidf)


tdm_tfidf_m[1:3, 1:3]



#' # UCZENIE MASZYNOWE NADZOROWANE
# UCZENIE MASZYNOWE NADZOROWANE ----
# (Supervised Machine Learning)





#' # Klasyfikacja SVM (Support Vector Machines)
# Klasyfikacja SVM (Support Vector Machines) ----



# Utworzenie ramki danych z macierzy TDM 
# (transpozycja: dokumenty jako wiersze)
dtm_df <- as.data.frame(t(tdm_tfidf_m))
dtm_df$Recommended <- factor(data$Recommended, levels = c("no", "yes"))



# Sprawdzenie wymiarów
dim(dtm_df)

# Sprawdzenie kilku ostatnich kolumn zmiennych
dtm_df[1:3, 1393:1398]



#' # a) Podział na zbiór treningowy/testowy: LOSOWY
# a) Podział na zbiór treningowy/testowy: LOSOWY ----


# Wielkość próby uczącej jest jednym z kluczowych czynników 
# wpływających na wynik modelu



# Podział na zbiór treningowy (uczący) i testowy (80/20)
set.seed(123)
train_indices <- sample(1:nrow(dtm_df), 0.8 * nrow(dtm_df))
trainData <- dtm_df[train_indices, ]
testData  <- dtm_df[-train_indices, ]




#' # Model klasyfikacji, podział LOSOWY
# Model klasyfikacji, podział LOSOWY ----



# Klasyfikator SVM (kernel liniowy)
svm_model <- svm(Recommended ~ ., data = trainData, kernel = "linear", probability = TRUE)



#' # Ocena modelu klasyfikacji, podział LOSOWY
# Ocena modelu klasyfikacji, podział LOSOWY ----



# Predykcja klas na zbiorze testowym
predictions <- predict(svm_model, newdata = testData)



# Macierz pomyłek (confusion matrix)
confusion_matrix <- table(Predicted = predictions, Actual = testData$Recommended)
print(confusion_matrix)




# Wyciąganie TP, TN, FP, FN z confusion_matrix
# "yes" jako pozytywna klasa
TP <- confusion_matrix["yes", "yes"]
TN <- confusion_matrix["no", "no"]
FP <- confusion_matrix["yes", "no"]
FN <- confusion_matrix["no", "yes"]


cat("\nTrue Positives (TP):", TP,
    "\nTrue Negatives (TN):", TN,
    "\nFalse Positives (FP):", FP,
    "\nFalse Negatives (FN):", FN, "\n")



# Obliczenia metryk
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
specificity <- TN / (TN + FP)
accuracy <- (TP + TN) / sum(confusion_matrix)
f1_score <- 2 * (precision * recall) / (precision + recall)


cat("\nAccuracy:", round(accuracy, 2),
    "\nPrecision (dla 'yes'):", round(precision, 2),
    "\nRecall (dla 'yes'):", round(recall, 2),
    "\nSpecificity (dla 'yes'):", round(specificity, 2),
    "\nF1 Score:", round(f1_score, 2), "\n")




#' # Wizualizacja metryk, podział LOSOWY
# Wizualizacja metryk, podział LOSOWY ----



# Przygotowanie danych do wykresu
metrics_df <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "Specificity", "F1 Score"),
  Value = c(accuracy, precision, recall, specificity, f1_score)
)


ggplot(metrics_df, aes(x = Metric, y = Value, fill = Metric)) +
  geom_col(width = 0.5, color = "black") +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 5) +
  ylim(0, 1) +
  labs(title = "Metryka", y = "Wartość", x = "") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")




# Przygotowanie do wizualizacji macierzy pomyłek
confusion_df <- as.data.frame(as.table(confusion_matrix))

# Tworzenie etykiet dla pól macierzy
confusion_df$Label <- c("True Negative (TN)", "False Positive (FP)", 
                        "False Negative (FN)", "True Positive (TP)")


# Wiersze to Predicted (prognozowane), kolumny to Actual (rzeczywiste)
ggplot(confusion_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste(Label, "\n", Freq)), size = 5) +
  scale_fill_gradient(low = "white", high = "steelblue", name= "Count") +
  labs(title = "Confusion Matrix", fill = "Count") +
  theme_minimal(base_size = 14)




# Oznaczenie poprawności klasyfikacji (Correct / Incorrect)
confusion_df$Correctness <- ifelse(confusion_df$Label %in% c("True Positive (TP)", "True Negative (TN)"),
                                   "Correct", "Incorrect")


# Przypisanie kolorów do typów błędów
confusion_df$FillColor <- case_when(
  confusion_df$Label == "True Positive (TP)" ~ "forestgreen",
  confusion_df$Label == "True Negative (TN)" ~ "forestgreen",
  confusion_df$Label == "False Positive (FP)" ~ "orange",
  confusion_df$Label == "False Negative (FN)" ~ "red"
)


# Wiersze to Predicted (prognozowane), kolumny to Actual (rzeczywiste)
# z przypisanymi kolorami i etykietami
ggplot(confusion_df, aes(x = Actual, y = Predicted, fill = FillColor)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste(Label, "\n", Freq)), size = 5, fontface = "bold") +
  scale_fill_identity(guide = "legend",
                      breaks = c("forestgreen", "orange", "red"),
                      labels = c("TP / TN (Correct)",
                                 "False Positive (Incorrect)",
                                 "False Negative (Incorrect)"),
                      name = "Wynik klasyfikacji") +
  labs(title = "Confusion Matrix") +
  theme_minimal(base_size = 14)






#' # b) Podział na zbiór treningowy/testowy: STRATYFIKOWANY
# b) Podział na zbiór treningowy/testowy: STRATYFIKOWANY ----


# Wielkość próby uczącej jest jednym z kluczowych czynników 
# wpływających na wynik modelu

# Równie ważne dla jakości modelu jest zapewnienie stratyfikacji,
# czyli utrzymanie takich samych proporcji klas w zbiorze treningowym i testowym.


# Podział na klasy
yes_class <- dtm_df[dtm_df$Recommended == "yes", ]
no_class  <- dtm_df[dtm_df$Recommended == "no",  ]


# Podział na zbiór treningowy (uczący) i testowy (80/20)
# Stratyfikowane próbkowanie 80% z każdej klasy
set.seed(123)
yes_train_indices <- sample(1:nrow(yes_class), size = floor(0.8 * nrow(yes_class)))
no_train_indices  <- sample(1:nrow(no_class),  size = floor(0.8 * nrow(no_class)))


# Budowa zbioru treningowego (uczącego) i testowego utrzymując proporcje klas
trainData <- rbind(yes_class[yes_train_indices, ], no_class[no_train_indices, ])
testData  <- rbind(yes_class[-yes_train_indices, ], no_class[-no_train_indices, ])




#' # Model klasyfikacji, podział STRATYFIKOWANY
# Model klasyfikacji, podział STRATYFIKOWANY ----



# Klasyfikator SVM (kernel liniowy)
svm_model <- svm(Recommended ~ ., data = trainData, kernel = "linear", probability = TRUE)



#' # Ocena modelu klasyfikacji, podział STRATYFIKOWANY
# Ocena modelu klasyfikacji, podział STRATYFIKOWANY ----




# Predykcja klas na zbiorze testowym
predictions <- predict(svm_model, newdata = testData)



# Macierz pomyłek (confusion matrix)
confusion_matrix <- table(Predicted = predictions, Actual = testData$Recommended)
print(confusion_matrix)




# Wyciąganie TP, TN, FP, FN z confusion_matrix
# "yes" jako pozytywna klasa
TP <- confusion_matrix["yes", "yes"]
TN <- confusion_matrix["no", "no"]
FP <- confusion_matrix["yes", "no"]
FN <- confusion_matrix["no", "yes"]


cat("\nTrue Positives (TP):", TP,
    "\nTrue Negatives (TN):", TN,
    "\nFalse Positives (FP):", FP,
    "\nFalse Negatives (FN):", FN, "\n")



# Obliczenie metryk
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
specificity <- TN / (TN + FP)
accuracy <- (TP + TN) / sum(confusion_matrix)
f1_score <- 2 * (precision * recall) / (precision + recall)


cat("\nAccuracy:", round(accuracy, 2),
    "\nPrecision (dla 'yes'):", round(precision, 2),
    "\nRecall (dla 'yes'):", round(recall, 2),
    "\nSpecificity (dla 'yes'):", round(specificity, 2),
    "\nF1 Score:", round(f1_score, 2), "\n")




#' # Wizualizacja metryk, podział STRATYFIKOWANY
# Wizualizacja metryk, podział STRATYFIKOWANY ----


# Przygotowanie danych do wykresu
metrics_df <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "Specificity", "F1 Score"),
  Value = c(accuracy, precision, recall, specificity, f1_score)
)


ggplot(metrics_df, aes(x = Metric, y = Value, fill = Metric)) +
  geom_col(width = 0.5, color = "black") +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 5) +
  ylim(0, 1) +
  labs(title = "Metryka", y = "Wartość", x = "") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")




# Przygotowanie do wizualizacji macierzy pomyłek
confusion_df <- as.data.frame(as.table(confusion_matrix))

# Tworzenie etykiet dla pól macierzy
confusion_df$Label <- c("True Negative (TN)", "False Positive (FP)", 
                        "False Negative (FN)", "True Positive (TP)")


# Wiersze to Predicted (prognozowane), kolumny to Actual (rzeczywiste)
ggplot(confusion_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste(Label, "\n", Freq)), size = 5) +
  scale_fill_gradient(low = "white", high = "steelblue", name= "Count") +
  labs(title = "Confusion Matrix", fill = "Count") +
  theme_minimal(base_size = 14)




# Oznaczenie poprawności klasyfikacji (Correct / Incorrect)
confusion_df$Correctness <- ifelse(confusion_df$Label %in% c("True Positive (TP)", "True Negative (TN)"),
                                   "Correct", "Incorrect")


# Przypisanie kolorów do typów błędów
confusion_df$FillColor <- case_when(
  confusion_df$Label == "True Positive (TP)" ~ "forestgreen",
  confusion_df$Label == "True Negative (TN)" ~ "forestgreen",
  confusion_df$Label == "False Positive (FP)" ~ "orange",
  confusion_df$Label == "False Negative (FN)" ~ "red"
)


# Wiersze to Predicted (prognozowane), kolumny to Actual (rzeczywiste)
# z przypisanymi kolorami i etykietami
ggplot(confusion_df, aes(x = Actual, y = Predicted, fill = FillColor)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste(Label, "\n", Freq)), size = 5, fontface = "bold") +
  scale_fill_identity(guide = "legend",
                      breaks = c("forestgreen", "orange", "red"),
                      labels = c("TP / TN (Correct)",
                                 "False Positive (Incorrect)",
                                 "False Negative (Incorrect)"),
                      name = "Wynik klasyfikacji") +
  labs(title = "Confusion Matrix") +
  theme_minimal(base_size = 14)










