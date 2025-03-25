


# Strona internetowa https://snowballstem.org


library(tm)
library(SnowballC)



# 1. Demo: wektor tekstowy ----
text <- c("programer", "program", "programing", "programation",
          "helpful", "help", "helping", "helps", "helps", "helps", "helps", "helps",
          "familiar", "familiarized", "familiarize", "familiarity",
          "families", "family", "family", "family",
          "organic", "organical", "organically", "organ", "organ", "organ",
          "organizer", "organized", "organization", "organization",
          "organizational")


sort(table(text), decreasing = TRUE)


# Szybka wizualizacja (opcjonalna)
par(mar = c(3, 8, 2, 2)) # marginesy: bottom, left, top, right

barplot(
  sort(table(text)), 
  border = "gray", 
  col = "skyblue",
  horiz = TRUE,
  las = 1,
  space = 0.2,     # odstępy między słupkami
  width = 0.3,     # szerokość słupka
  cex.names = 0.8,  # wielkość etykiet słupków
  main = "Częstość wszystkich słów w tekście",
  xlim=c(0,max(table(text)))
)



# 2. Wykonaj stemming ----
stemmed_doc <- stemDocument(text)



# 3. Wynik stemmingu ----
stemmed_doc
sort(table(stemmed_doc), decreasing = TRUE)


# Szybka wizualizacja (opcjonalna)
par(mar = c(3, 8, 2, 2)) # marginesy: bottom, left, top, right

barplot(
  sort(table(stemmed_doc)), 
  border = "gray", 
  col = "orchid",
  horiz = TRUE,
  las = 1,
  space = 0.2,     # odstępy między słupkami
  width = 0.3,     # szerokość słupka
  cex.names = 0.8,  # wielkość etykiet słupków
  main = "Częstość wszystkich rdzeni słów po stemmingu",
  xlim=c(0,max(table(stemmed_doc))+1)
)



# Porównanie
#
# wszystkie słowa w oryginalnym tekście
sort(table(text), decreasing = TRUE)
# wszystkie rdzenie słów po stemmingu
sort(table(stemmed_doc), decreasing = TRUE)




# 4. Uzupełnienie rdzeni słów po stemmingu ----


# Typy uzupełnienia:
#
# prevalent – (domyślny) wybiera najczęściej występujące słowo jako uzupełnienie,
#             dla identycznych częstości wybiera kolejność alfabetyczną; 
#
# first – wybiera pierwsze znalezione słowo jako uzupełnienie
#
# longest – wybiera najdłuższe słowo (pod względem liczby znaków)
#
# shortest – wybiera najkrótsze słowo (pod względem liczby znaków)
#
# random – wybiera losowe słowo jako uzupełnienie



# Przykłady wszystkich typów uzupełnienia


# type = prevalent ----
completed_doc <- stemCompletion(stemmed_doc, dictionary=text, type="prevalent")
completed_doc

# porównanie
table(stemmed_doc)
table(completed_doc)



# type = first ----
completed_doc_first <- stemCompletion(stemmed_doc, dictionary=text, type="first")
completed_doc_first

# porównanie
table(completed_doc)
table(completed_doc_first)



# type = longest ----
completed_doc_longest <- stemCompletion(stemmed_doc, dictionary=text, type="longest")
completed_doc_longest

# porównanie
table(completed_doc)
table(completed_doc_first)
table(completed_doc_longest)



# type = shortest ----
completed_doc_shortest <- stemCompletion(stemmed_doc, dictionary=text, type="shortest")
completed_doc_shortest

# porównanie
table(completed_doc)
table(completed_doc_first)
table(completed_doc_longest)
table(completed_doc_shortest)



# type = random ----
completed_doc_random <- stemCompletion(stemmed_doc, dictionary=text, type="random")
completed_doc_random

# porównanie wszysktich typów uzupełnienia
table(completed_doc)
table(completed_doc_first)
table(completed_doc_longest)
table(completed_doc_shortest)
table(completed_doc_random)



# Porównanie wyniku stemmingu z uzupełnieniem rdzeni type=prevalent

# Szybka wizualizacja (opcjonalna)
par(mar = c(3, 8, 2, 2)) # marginesy: bottom, left, top, right


barplot(
  sort(table(stemmed_doc)), 
  border = "gray", 
  col = "orchid2",
  horiz = TRUE,
  las = 1,
  space = 0.2,     # odstępy między słupkami
  width = 0.3,     # szerokość słupka
  cex.names = 0.8,  # wielkość etykiet słupków
  main = "Częstość wszystkich rdzeni słów po stemmingu",
  xlim=c(0,max(table(stemmed_doc))+1)
)


barplot(
  sort(table(completed_doc)), 
  border = "gray", 
  col = "#69b3a2",
  horiz = TRUE,
  las = 1,
  space = 0.2,     # odstępy między słupkami
  width = 0.3,     # szerokość słupka
  cex.names = 0.8,  # wielkość etykiet słupków
  main = "Częstość wszystkich uzupełnionych słów",
  xlim=c(0,max(table(completed_doc))+1)
)


barplot(
  sort(table(text)), 
  border = "gray", 
  col = "skyblue1",
  horiz = TRUE,
  las = 1,
  space = 0.2,     # odstępy między słupkami
  width = 0.3,     # szerokość słupka
  cex.names = 0.8,  # wielkość etykiet słupków
  main = "Częstość wszystkich słów w tekście",
  xlim=c(0,max(table(text)))
)






