#' ---
#' title: "TF-IDF"
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
# Załaduj wszystkie pliki .txt z bieżącego folderu
docs <- DirSource(getwd(), pattern = "*.txt", encoding = "UTF-8")

# Utwórz korpus dokumentów tekstowych
corpus <- VCorpus(docs)

# Sprawdź nazwy plików
cat("Załadowane pliki:\n")
print(names(corpus))


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
corpus <- tm_map(corpus, removeWords, c("nikki","kellermans", "maine", "amy", "bear", "captain", "dude", "billy", "johnny", "robbie", "max", "doc", "baby", "lisa", "vivian", "moe", "neil", "penny", "tito", "jake", "marjorie", "kellerman", "sheldrake", "pressman", "schumacher", "sylvia", "sidney", "cleopatra", "kennedy", "taylor", "elizabeth", "dee", "sandra", "bernstein", "burns", "arthur", "murray", "kramer", "maria", "janet", "francis", "frances", "houseman", "suarez", "rodriguez", "gould", "alfredo", "romeo", "brucie", "nicky", "james", "sarah", "hayden", "ian", "titanic", "atlantic", "passengers", "boat", "boats", "coast", "shit", "bitch", "fuck", "fucked", "fucking", "gonna", "got", "lord", "daddy", "know", "like", "okay", "yeah", "just", "can", "come", "get", "think", "sorry", "want", "will", "right", "good", "one", "going", "now", "well", "back", "need", "really", "something", "anything", "everything", "nothing", "someone", "anyone", "everybody", "thing", "things", "way", "came", "better", "minutes", "supposed", "behind", "acting", "tvirus", "frame", "wave", "tonight", "kaplan", "kelly", "mambo"))

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



#' # A. Macierz częstości TDM ----
# A. Macierz częstości TDM ----

tdm <- TermDocumentMatrix(corpus)
tdm_m <- as.matrix(tdm)



#' # 2. Zliczanie częstości słów
# 2. Zliczanie częstości słów ----
# (Word Frequency Count)


# Zlicz same częstości słów w macierzach
v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)
head(tdm_df, 20)



#' # 3. Eksploracyjna analiza danych
# 3. Eksploracyjna analiza danych ----
# (Exploratory Data Analysis, EDA)


# Chmura słów (globalna)
wordcloud(words = tdm_df$word, freq = tdm_df$freq, min.freq = 7, 
          colors = brewer.pal(8, "Dark2"))


# Wyświetl top 10
print(head(tdm_df, 20))



#' # B. Macierz częstości TDM z TF-IDF ----
# B. Macierz częstości TDM z TF-IDF ----

tdm_tfidf <- TermDocumentMatrix(corpus,
                                control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))

tdm_tfidf
# inspect(tdm_tfidf)


tdm_tfidf_m <- as.matrix(tdm_tfidf)


#' # 2. Analiza TF-IDF dla gatunków
# 2. Analiza TF-IDF dla gatunków ----

# Sprawdzenie nazw dokumentów
colnames(tdm_tfidf_m)

# Top 20 słów dla dokumentu 1
genre1 <- sort(tdm_tfidf_m[,1], decreasing = TRUE)

genre1_df <- data.frame(
  word = names(genre1),
  tfidf = genre1
)

cat("\n")
cat("====================================\n")
cat("TOP 20 słów dla:", colnames(tdm_tfidf_m)[1], "\n")
cat("====================================\n")

print(head(genre1_df, 20))

wordcloud(
  words = genre1_df$word,
  freq = genre1_df$tfidf,
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)

#' # TF-IDF - Gatunek 1

top_genre1 <- head(genre1_df, 15)

ggplot(top_genre1,
       aes(x = reorder(word, tfidf),
           y = tfidf)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste("Top 15 słów TF-IDF:", colnames(tdm_tfidf_m)[1]),
    x = "Słowo",
    y = "TF-IDF"
  ) +
  theme_minimal()

# Top 20 słów dla dokumentu 2
genre2 <- sort(tdm_tfidf_m[,2], decreasing = TRUE)

genre2_df <- data.frame(
  word = names(genre2),
  tfidf = genre2
)

cat("\n")
cat("====================================\n")
cat("TOP 20 słów dla:", colnames(tdm_tfidf_m)[2], "\n")
cat("====================================\n")

print(head(genre2_df, 20))

#' # Chmura słów - Gatunek 2

wordcloud(
  words = genre2_df$word,
  freq = genre2_df$tfidf,
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)

#' # TF-IDF - Gatunek 2

top_genre2 <- head(genre2_df, 15)

ggplot(top_genre2,
       aes(x = reorder(word, tfidf),
           y = tfidf)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste("Top 15 słów TF-IDF:", colnames(tdm_tfidf_m)[2]),
    x = "Słowo",
    y = "TF-IDF"
  ) +
  theme_minimal()
