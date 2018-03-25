#####
##### flaubert analyses: part one
#####
library(gutenbergr)
library(stringr)
library(dplyr)
library(tidytext)
library(tokenizers)
library(SnowballC)
library(stopwords)

###
### data preparation
###

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
stopwords.snowball <- get_stopwords(language = "fr", 
                          source = "snowball")
  # snowball is the only source in the stopwords
  # package with french option

# expand stopword dictionary
stopwords.exp <- as.tbl(read.table("stopwords-fr.txt")) %>% 
  rename(word = V1) %>% 
  mutate(lexicon = "stopwords-fr")
stopwords.exp$word <- as.character(stopwords.exp$word)

# combine unique stopwords into single tbl
stopwords.combined <- bind_rows(stopwords.snowball,
                                stopwords.exp) %>% 
  distinct(word)


# remove punctuation that makes cleaning french
# more difficult
texts$text <- str_replace_all(texts$text, "[[:punct:]]",
                              " ")
  # this puts a space that will be removed and 
  # isolated contracted stop words.

# using just snowballC
tidy.flaubert <- texts %>% 
  unnest_tokens(word, text) %>% 
  mutate(stem = wordStem(word, language = "french")) %>% 
  anti_join(stopwords.combined)

# seem to be many single letter cells
# count them to possibly remove (keeping
# a, as it is a verb)
tidy.flaubert %>% 
  mutate(stem.length = str_length(stem)) %>% 
  filter(stem.length == 1 & stem != "a") %>% 
  summarise(n())
  # 6131 such cases, and many seem to be the result 
  # of parsing problem. drop for now.

# because I want to do further investigation,
# I am going to say these cases into an object
# and then use anti_join() rather than filtering.
singles <- tidy.flaubert %>% 
  mutate(stem.length = str_length(stem)) %>% 
  filter(stem.length == 1 & stem != "a")

tidy.flaubert <- tidy.flaubert %>% 
  anti_join(singles)

# make works a factor variable
tidy.flaubert$title <- as.factor(tidy.flaubert$title)



###
### basic analyses
###

# let's count the overall stem occurence 
tidy.flaubert %>% 
  count(stem, sort = TRUE) %>% 
  head(n = 20)
  # not good: too many dans, si, bien, etc


