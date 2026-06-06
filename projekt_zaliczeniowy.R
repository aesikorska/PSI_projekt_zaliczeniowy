#' ---
#' title: "Projekt zaliczeniowy"
#' author: "Aleksandra Sikorska, Izabela Sprysak"
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

#' # 1. Wczytanie potrzebnych pakietów
# 1. Wczytanie potrzebnych pakietów ----

library(tm)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(wordcloud)

#' # 2. Wczytanie danych tekstowych
# 2. Wczytanie danych tekstowych ----

# Wczytanie danych z pliku reviews.csv
data <- read.csv("reviews.csv", sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")

#' > Dane to opinie pracowników. Podzielone są na "best thing", "worst thing"
#' > i "entire review". Podział ten jest zachowany w dalszej analizie.
#' > Chmury słów stworzone są dla każdej kategorii osobno.
#' > Topic modelling przeprowadzony jest na "best thing" i "worst thing".
#' > Natomiast asocjacje przeprowadzone są na "entire review".
#' 

corpus_best_thing <- VCorpus(VectorSource(data$best_thing))
corpus_worst_thing <- VCorpus(VectorSource(data$worst_thing))
corpus_entire_review <- VCorpus(VectorSource(data$entire_review))

#' # 3. Przetwarzanie i oczyszczanie tekstu
# 3. Przetwarzanie i oczyszczanie tekstu ----

# Funkcja czyszcząca tekst

clean_corpus <- function(corpus) {
  
  # Kodowanie w całym korpusie
  corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))
  # Funkcja do zamiany znaków na spację
  toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))
  # Usunięcie \n
  corpus <- tm_map(corpus, toSpace, "\n")
  # Zmiana liter na małe
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Usunięcie cyfr
  corpus <- tm_map(corpus, removeNumbers)
  # Usunięcie angielskich stopwords
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  # Usunięcie interpunkcji
  corpus <- tm_map(corpus, removePunctuation)
  # Usunięcie "dont" i "cant", które pozostają w innym przypadku
  corpus <- tm_map(corpus, removeWords, c("dont", "cant", "isnt"))
  # Usunięcie nadmiarowych spacji
  corpus <- tm_map(corpus, stripWhitespace)
}

# Czyszczenie korpusów
corpus_best_thing <- clean_corpus(corpus_best_thing)
corpus_worst_thing <- clean_corpus(corpus_worst_thing)
corpus_entire_review <- clean_corpus(corpus_entire_review)


corpus_best_thing[[1]][[1]]

#' # 4. Stemming
# 4. Stemming ----

# Zachowanie kopii korpusu 
# do użycia jako dictionary w uzupełnianiu rdzeni
corpus_best_thing_copy <- corpus_best_thing
corpus_worst_thing_copy <- corpus_worst_thing
corpus_entire_review_copy <- corpus_entire_review

# Funkcja pomocnicza: wykonuje stemCompletion linia po linii
complete_stems <- content_transformer(function(x, dict) {
  x <- unlist(strsplit(x, " "))                  # dzieli na słowa
  x <- stemCompletion(x, dictionary = dict, type="prevalent") # uzupełnia rdzenie
  paste(x, collapse = " ")                       # łączy z powrotem w tekst
})

# Funkcja do zamiany znaków na spację
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))

# Funkcja wykonująca stemming i stemCompletion
stemming <- function(corpus) {
  # Utworzenie kopii korpusu
  corpus_copy <- corpus
  # Wykonanie stemmingu
  corpus_stemmed <- tm_map(corpus, stemDocument)
  
  # Wykonanie stemCompletion do każdego dokumentu w korpusie
  corpus_completed <- tm_map(corpus_stemmed, complete_stems, dict = corpus_copy)
  
  # usunięcie NA
  corpus_completed <- tm_map(corpus_completed, toSpace, "NA")
  corpus_completed <- tm_map(corpus_completed, stripWhitespace)
  
  return(corpus_completed)
}


completed_corpus_best_thing <- stemming(corpus_best_thing)
completed_corpus_worst_thing <- stemming(corpus_worst_thing)
completed_corpus_entire_review <- stemming(corpus_entire_review)

#' # 5. Tokenizacja
# 5. Tokenizacja ----

# Macierz częstości TDM ----

# Funkcja tworząca macierz częstości TDM + stemCompletion

create_tdm <- function(corpus) {
  tdm <- TermDocumentMatrix(corpus)
  return(tdm)
}

create_tdm_m <- function(corpus) {
  tdm <- TermDocumentMatrix(corpus)
  tdm_m <- as.matrix(tdm)
  return(tdm_m)
}


# Utworzenie macierzy częstości TDM dla wszystkich korpusów

tdm_m_best_thing <- create_tdm_m(completed_corpus_best_thing)
tdm_m_worst_thing <- create_tdm_m(completed_corpus_worst_thing)
tdm_m_entire_review <- create_tdm_m(completed_corpus_entire_review)

tdm_best_thing <- create_tdm(completed_corpus_best_thing)
tdm_worst_thing <- create_tdm(completed_corpus_worst_thing)
tdm_entire_review <- create_tdm(completed_corpus_entire_review)
  

#' # 6. Chmura słów
# 6. Chmura słów ----

# Funkcja obliczająca częstość występowania słów

word_frequency <- function(tdm_m) {
  appearance_count <- sort(rowSums(tdm_m), decreasing = TRUE)
  tdm_data_frame <- data.frame(word = names(appearance_count), frequency = appearance_count)
  return(tdm_data_frame)
}

# Utworzenie ramek danych z częstością słów
tdm_best_thing_df <- word_frequency(tdm_m_best_thing)
tdm_worst_thing_df <- word_frequency(tdm_m_worst_thing)
tdm_entire_review_df <- word_frequency(tdm_m_entire_review)

head(tdm_best_thing_df, 10)
head(tdm_worst_thing_df, 10)
head(tdm_entire_review_df, 10)

# Funkcja generująca chmurę słów

chmura_slow <- function(tdm_df) {
  wordcloud(
    words = tdm_df$word,
    freq = tdm_df$frequency,
    min.freq = 7,
    colors = brewer.pal(8, "Dark2")
  )
}

#' # 6.1. Chmura słów - entire review
# 6.1. Chmura słów - entire review ----

chmura_slow(tdm_entire_review_df)

#' > W swoich opiniach pracownicy zwracali uwagę przede wszystkim na klientów ("customers"),
#' > zespół ("team") i sposób zarządzania ("management"). 
#' > Sugeruje to, że są to aspekty, na które należy zwrócić największą uwagę.
#'
#' # 6.2. Chmura słów - best thing
# 6.2. Chmura słów - best thing ----

chmura_slow(tdm_best_thing_df)

#' > W pozytywnych aspektach pracownicy używają często słów "team", "customers" i "people", 
#' > co wskazuje na to, że zespół i klienci byli pozytywnie postrzegani. 
#' > Pojawienie się słowa "free" może mieć związek z darmową kawą dla pracowników, 
#' > można to dalej sprawdzić z wykorzystaniem asocjacji.
#' > Może to też częściowo wyjaśniać pojawienie się słowa "coffee", ale niekoniecznie.
#'
#' # 6.3. Chmura słów - worst thing
# 6.3. Chmura słów - worst thing ----

chmura_slow(tdm_worst_thing_df)

#' > Pojawienie się słów "hours" i "time" może sugerować, że jednym z problemów były godziny pracy
#' > lub jak sugeruje dodatkowo występowanie "paid" i "unpaid" - brak wynagrodzenia za wszystkie
#' > przepracowane godziny. Podobnie jak w pozytywnych apektach, też pojawiają się
#' > słowa "customers", "people" i "team", a także dodatkowo "management". Sugeruje to że część
#' > pracowników postrzega negatywnie swój zespół, a w szczególności zarządzanie w firmie.
#' > Pracownicy używali też słowa "breaks", co może sugerować problemy z przerwami w pracy.
#'
#' # 7. Topic modelling
# 7. Topic modelling ----

# Funkcja, która na podstawie korpusu wizualizuje słowa o największej informatywności
# przy użyciu metody ukrytej alokacji Dirichleta (LDA) dla wyznaczonej liczby teamtów

top_terms_by_topic_LDA <- function(corpus, # korpus tekstu
                                   plot = TRUE, # domyślnie rysuje wykres
                                   k = number_of_topics) # liczba tematów
{
  # Utworzenie macierzy DTM
  DTM <- DocumentTermMatrix(corpus)
  
  # Pobranie indeksu każdej unikalnej wartości
  unique_indexes <- unique(DTM$i)
  # Pobranie z DTM podzbioru unikalnych wartości
  DTM <- DTM[unique_indexes, ]
  
  # Wykonanie LDA
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 132709052026))
  # Pobranie słów/tematów w uporządkowanym formacie tidy
  topics <- tidy(lda, matrix = "beta") 
  
  # Pobranie dziesięciu najczęstszych słów dla każdego tematu
  top_terms <- topics  %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) # Uporządkowanie słów w malejącej kolejności informatywności
  
  
  
  # Rysowanie wykresu (domyślnie plot = TRUE)
  if(plot == T){
    # Dziesięć najczęstszych słów dla każdego tematu
    top_terms %>%
      mutate(term = reorder(term, beta)) %>% # Sortowanie słów według wartości beta 
      ggplot(aes(term, beta, fill = factor(topic))) + # Rysowanie beta według tematu
      geom_col(show.legend = FALSE) + # Wykres kolumnowy
      facet_wrap(~ topic, scales = "free") + # Każdy temat na osobnym wykresie
      labs(x = "Terminy", y = "β (ważność słowa w temacie)") +
      coord_flip() +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1")
  }
  else{ 
    # Jeśli użytkownik nie chce wykresu zwraca listę posortowanych słów
    return(top_terms)
  }
  
}

#' # 7.1. Modelowanie tematów dla best_thing
# 7.1. Modelowanie tematów dla best_thing ----

number_of_topics = 2
top_terms_by_topic_LDA(tdm_best_thing_df$word)

number_of_topics = 3
top_terms_by_topic_LDA(tdm_best_thing_df$word)

number_of_topics = 4
top_terms_by_topic_LDA(tdm_best_thing_df$word)

number_of_topics = 5
top_terms_by_topic_LDA(tdm_best_thing_df$word)

number_of_topics = 6
top_terms_by_topic_LDA(tdm_best_thing_df$word)

#' > Najlepszy zdaje się podział na trzy tematy. Można zauważyć, że pierwszy temat 
#' > odnosi się do ogólnych warunków pracy i jej tempa. 
#' > Drugi temat ma związek z atmosferą i kontaktem z ludźmi, w tym klientami.
#' > Trzeci temat odnosi się do godzin pracy i dodatkowych korzyści.
#'
#' # 7.2. Modelowanie tematów dla worst_thing
# 7.2. Modelowanie tematów dla worst_thing ----

number_of_topics = 2
top_terms_by_topic_LDA(tdm_worst_thing_df$word)

number_of_topics = 3
top_terms_by_topic_LDA(tdm_worst_thing_df$word)

number_of_topics = 4
top_terms_by_topic_LDA(tdm_worst_thing_df$word)

number_of_topics = 5
top_terms_by_topic_LDA(tdm_worst_thing_df$word)

number_of_topics = 6
top_terms_by_topic_LDA(tdm_worst_thing_df$word)

#' > Podział na trzy tematy zdaje się najlepszy. 
#' > Pierwszy temat odnosi się do potencjalnych problemów z przerwami na lunch
#' > i problemów z wolnym ("holidays").
#' > Drugi temat może dotyczyć problemów z menedżerami, naruszeniami i stresującym
#' > charakterem pracy.
#' > Trzeci temat może mieć związek z czasem i napięciami w pracy ("shout").
#' 

#' # 8. Asocjacje
# 8. Asocjacje ----

# Funkcja tworząca wykres asocjacji

wykres_asocjacji <- function(target_word, cor_limit) {
  # Oblicz asocjacje dla tego słowa
  associations <- findAssocs(tdm_entire_review, target_word, corlimit = cor_limit)
  assoc_vector <- associations[[target_word]]
  assoc_sorted <- sort(assoc_vector, decreasing = TRUE)
  
  
  # Ramka danych
  assoc_df <- data.frame(
    word = factor(names(assoc_sorted), levels = names(assoc_sorted)[order(assoc_sorted)]),
    score = assoc_sorted
  )
  
  # Wykres lizakowy z natężeniem
  # na podstawie wartości korelacji score:
  ggplot(assoc_df, aes(x = score, y = reorder(word, score), color = score)) +
    geom_segment(aes(x = 0, xend = score, y = word, yend = word), size = 1.2) +
    geom_point(size = 4) +
    geom_text(aes(label = round(score, 2)), hjust = -0.3, size = 3.5, color = "black") +
    scale_color_gradient(low = "#bcbddc", high = "#54278f") +
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
}

#' # 8.1. Asocjacje ze słowem "work"
# 8.1. Asocjacje ze słowem "work" ----

findAssocs(tdm_entire_review, "work", 0.5)

# Wytypowane słowo i próg asocjacji
target_word <- "work"
cor_limit <- 0.5

wykres_asocjacji(target_word, cor_limit)

#' > Asocjacji z "work" jest dużo, trudno wyciągać bardziej szczegółowe wnioski
#' > na podstawie wyników.
#' 

#' # 8.2. Asocjacje ze słowem "holidays"
# 8.2. Asocjacje ze słowem "holidays" ----

findAssocs(tdm_entire_review, "holidays", 0.5)

# Wytypowane słowo i próg asocjacji
target_word <- "holidays"
cor_limit <- 0.5

wykres_asocjacji(target_word, cor_limit)

#' > Asocjacje ze słowamu "refuse", "horribly", "forced" mogą sugerować problemy
#' > z uzyskiwaniem wolnego.
#' 

#' # 8.3. Asocjacje ze słowem "free"
# 8.3. Asocjacje ze słowem "free" ----

findAssocs(tdm_entire_review, "free", 0.5)

# Wytypowane słowo i próg asocjacji
target_word <- "free"
cor_limit <- 0.5

wykres_asocjacji(target_word, cor_limit)

#' > Asocjacje ze słowami "shift" może wskazywać na coś darmowego w trakcie zmiany.
#' 
#' # 8.4. Asocjacje ze słowem "hours"
# 8.4. Asocjacje ze słowem "hours" ----

findAssocs(tdm_entire_review, "hours", 0.5)

# Wytypowane słowo i próg asocjacji
target_word <- "hours"
cor_limit <- 0.5

wykres_asocjacji(target_word, cor_limit)

#' > Pojawianie się asocjacji ze "students" i "uni" może sugerować godziny pracy,
#' > które odpowiadają studentom.
#' > Chociaż możliwe jest też np., że pracownicy zwracali uwagę na godziny, w których
#' > najczęściej przychodzą studenci. 