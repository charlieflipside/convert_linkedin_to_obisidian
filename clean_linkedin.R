# Clean Shared Commentary ----
library(readr) # handles common csv issues like line breaks better
library(tm) # stopwords 

shared <- readr::read_csv("linkedin-exports/Shares.csv")
shared <- shared[, c("Date", "ShareLink", "ShareCommentary")]
shared <- unique(shared)

remove_quotes <- function(txt){
  gsub(pattern = "\"\"|\"", "", txt)
}

remove_skips <- function(txt){
  # Replace multiple consecutive newline characters with just one newline
  txt <- gsub(pattern = "\\r+|\\n+", "\n", txt)
  txt <- gsub(pattern = "\r+|\n+", "\n", txt)
  txt <- gsub(pattern = "---+", "", txt)
  
  # Remove leading and trailing newlines
  txt <- gsub(pattern = "^\\s*\n+|\\s*\n+$", "", txt)
  txt <- gsub(pattern = "^\\s*\n+|\\s*\n+$", "", txt)
  
  return(txt)
}

shared$ShareCommentary <- remove_quotes(shared$ShareCommentary)

shared$ShareCommentary <- remove_skips(shared$ShareCommentary)
shared$length <- nchar(shared$ShareCommentary)

shared <- shared[shared$length >= 100, ]

# Clean Comments ----
comments <- readr::read_csv("linkedin-exports/Comments.csv")
# fix errant csv breaks 
problem_rows <- which(!grepl("^[0-9]", x = comments$Date))

for(i in problem_rows){
  comments[i, 3] <- comments[i, 1]
  comments[i, 1:2] <- comments[i-1, 1:2]
}

# Identify unique keywords for tags ----

posts <- gsub("[[:punct:]]|\\\r|\\\n"," ", shared$ShareCommentary)

# Create a Corpus
corpus <- Corpus(VectorSource(posts))

# Remove stopwords
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))

# Retrieve the cleaned text
cleaned_text <- sapply(corpus, as.character)

# Select terms as tags ----

tags_tbl <- read_csv("select_tags.csv")

apply_tags <- function(txt, tags_tbl){

    ct_index <- unlist(lapply(tags_tbl$terms, function(srch){
      return(grepl(pattern = srch,x = txt))
    }))
    
    tags <- unique( c(tags_tbl$consolidated_category[ct_index], tags_tbl$category[ct_index]) )
    if(length(tags) == 0){
      tags = ""
    }
    return(tags)
}
  
tags_list <- lapply(cleaned_text, apply_tags, tags_tbl = tags_tbl)

# first n words as title ----

extract_first_n_words <- function(input_text, n = 6) {
  # Create a Corpus from the input text
  corpus <- Corpus(VectorSource(input_text))
  
  # Define a function to preprocess the text
  preprocess_text <- function(text) {
    text <- tolower(text)
    text <- removePunctuation(text)
    text <- removeNumbers(text)
    return(text)
  }
  
  # Apply the preprocessing function to the corpus
  corpus <- tm_map(corpus, content_transformer(preprocess_text))
  
  # Extract the first n words
  content <- as.character(corpus[[1]])
  words <- strsplit(content, "\\s+")[[1]]
  first_n_words <- paste(words[1:min(n, length(words))], collapse = " ")
  
  return(first_n_words)
}

titles <-  unlist(lapply(shared$ShareCommentary, extract_first_n_words))

# Create md frame ----

note_frame <- {
"---
tags:
- THE_TAGS
---
  
# TITLE
Created: SYSTIME

CONTENT

## References
1. THE_REFERENCES
  "
}

# Swap frame ----

swap_frame <- function(notetemplate = note_frame, tags="",
                       title, systime, content, references = ""){
  
  note_frame <- gsub(pattern = 'TITLE', title, x = notetemplate) 
  note_frame <- gsub(pattern = 'SYSTIME', systime, x = note_frame) 
  note_frame <- gsub(pattern = 'CONTENT', content, x = note_frame) 
  
  # tag(s)
  ln = length(tags)
  if(ln == 1){
    note_frame <- gsub(pattern = 'THE_TAGS', tags, x = note_frame) 
  } else if(ln > 1) {
    temp_tag = tags[1]
    for(i in 2:length(tags)){
      temp_tag = paste0(temp_tag, "\n- ", tags[i])
    }
    note_frame <- gsub(pattern = 'THE_TAGS', temp_tag, x = note_frame) 
  }
  
  # reference(s)
  
  ln = length(references)
  if(ln == 1){
    note_frame <- gsub(pattern = 'THE_REFERENCES', references, x = note_frame) 
  } else if(ln > 1) {
   temp_ref = references[1]
  for(i in 2:length(references)){
    temp_ref = paste0(temp_ref, "\n", i, ". ", references[i])
  }
   note_frame <- gsub(pattern = 'THE_REFERENCES', temp_ref, x = note_frame) 
  }
  
  # return 
  
  return(note_frame)
}

for(i in 1:nrow(shared)){
  
  note <- swap_frame(note_frame, tags = tags_list[[i]], 
                     title = titles[i],
                     systime = substr(shared$Date[i], start = 0, stop = 16),
                     content = shared$ShareCommentary[i],
                     references = shared$ShareLink[i])
  
  write(note, file = paste0("outputs/", titles[i], ".md"))
  
}

