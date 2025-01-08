library(rvest)
library(tm)
library(SnowballC)
library(tidyverse)
library(textstem)
library(hunspell)

contractions_dict <- c(
  "I'm" = "I am",
  "you're" = "you are",
  "he's" = "he is",
  "she's" = "she is",
  "it's" = "it is",
  "we're" = "we are",
  "they're" = "they are",
  "don't" = "do not",
  "doesn't" = "does not",
  "can't" = "cannot",
  "won't" = "will not",
  "didn't" = "did not"
)


expand_contractions <- function(text, contractions_dict) {
  for (contraction in names(contractions_dict)) {
    text <- gsub(contraction, contractions_dict[contraction], text, ignore.case = TRUE)
  }
  return(text)
}

url <- 'https://stackoverflow.com/questions'
page <- read_html(url)
text_data <- page %>% html_nodes('body') %>% html_text() %>% .[1]
cat("Original scraped text:\n", text_data, "\n\n")

clean_text <- function(text) {
  text <- gsub("<[^>]+>", "", text)
  text <- gsub("[[:punct:]]", " ", text)
  text <- gsub("\\s+", " ", text)
  text <- tolower(text)
  return(text)
}

cleaned_text <- clean_text(text_data)
cat("Text after cleaning:\n", cleaned_text, "\n\n")

tokenized_text <- strsplit(cleaned_text, "\\s+")[[1]]
cat("Text after tokenization:\n", toString(tokenized_text), "\n\n")

stopwords_list <- stopwords("en")
cleaned_tokens <- tokenized_text[!tokenized_text %in% stopwords_list]
cat("Text after removing stopwords:\n", toString(cleaned_tokens), "\n\n")

stem_tokens <- function(tokens) {
  sapply(tokens, stemDocument, language = "english")
}

stemmed_text <- stem_tokens(cleaned_tokens)
cat("Text after stemming:\n", toString(stemmed_text), "\n\n")

lemmatize_tokens <- function(tokens) {
  lapply(tokens, lemmatize_words)
}

lemmatized_text <- unlist(lemmatize_tokens(list(stemmed_text)))  
cat("Text after lemmatization:\n", toString(lemmatized_text), "\n\n")

text_with_contractions <- expand_contractions(toString(lemmatized_text), contractions_dict)
cat("Text after handling contractions:\n", text_with_contractions, "\n\n")

remove_emojis <- function(text) {
  gsub("[^\x01-\x7F]", "", text)
}

text_no_emojis <- remove_emojis(text_with_contractions)
cat("Text after removing emojis and special characters:\n", text_no_emojis, "\n\n")

correct_spelling <- function(tokens) {
  corrections <- sapply(tokens, function(token) {
    suggestions <- hunspell::hunspell_suggest(token)
    if (length(suggestions[[1]]) > 0) suggestions[[1]][1] else token
  })
  return(corrections)
}

corrected_text <- correct_spelling(stemmed_text)
cat("Text after spell checking:\n", toString(corrected_text), "\n\n")
output_file <- "H:/Theory/DS/Mid_Project_DataScience/cleaned_text.txt"

writeLines(cleaned_text, output_file)

cat("Cleaned text has been exported to", output_file, "\n")




