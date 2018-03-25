#####
##### flaubert analyses: part one
#####
library(gutenbergr)
library(stringr)
library(dplyr)

# get data from gutenbergr
gutenberg_metadata
names(gutenberg_metadata)
  # author, language, has_text (TRUE/FALSE)

# get french flaubert texts
ids <- gutenberg_metadata %>% 
  mutate(flaubert = str_detect(gutenberg_metadata$author,
                               "Flaubert")) %>% 
  filter(language == "fr") %>% 
  