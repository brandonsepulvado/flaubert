#####
##### flaubert analyses: part one
#####
library(gutenbergr)
library(stringr)
library(dplyr)
library(tidytext)
library(tokenizers)

# get data from gutenbergr
gutenberg_metadata
names(gutenberg_metadata)
  # author, language, has_text (TRUE/FALSE)

# get french flaubert texts
ids <- gutenberg_metadata %>% 
  mutate(flaubert = str_detect(gutenberg_metadata$author,
                               "Flaubert")) %>% 
  filter(language == "fr" & flaubert == TRUE
         & has_text == TRUE) %>% 
  select(gutenberg_id) # 9 works
  
# download texts
texts <- gutenberg_download(ids, meta_fields = "title")

# need french stop words to tidy texts
stopwords <- get_stopwords(language = "fr", 
                          source = "snowball")

# remove punctuation that makes cleaning french
# more difficult
texts$text <- str_replace_all(texts$text, "[[:punct:]]",
                              " ")

# stem words
tokenized <- tokenize_word_stems(texts$text,
                    language = "french", 
                    stopwords = stopwords,
                    simplify = TRUE)

# put list elements in respective texts cell
texts <- texts %>% 
  mutate(tokenized = NA)
for (i in 1:nrow(texts)) {
  if (length(tokenized[[i]] > 0)) {
    texts$tokenized[i] <- paste(tokenized[[i]], 
                                collapse = " ")
  } else {
    texts$tokenized[i] <- " "
  }
}

# tidy the texts
tidy.flaubert <- texts %>% 
  unnest_tokens(word, tokenized) %>% 
  anti_join(stopwords)