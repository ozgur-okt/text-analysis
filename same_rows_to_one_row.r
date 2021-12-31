
library(dplyr)

data <- tibble::tribble(
  ~id,        ~word,
   1L,     "arrive",
   1L,        "bus",
   1L,       "stop",
   1L,       "time",
   1L,      "beard",
   1L,    "bearded",
   1L,       "sits",
   2L,     "whilst",
   2L,      "begin",
   2L,      "argue",
   2L,       "seat",
   2L,       "time",
   2L,     "police",
   3L,    "officer",
   3L,      "walks",
   3L, "intervenes"
  )

data %>% 
  group_by(id) %>% 
  mutate(word = paste0(word, collapse = " ")) %>% 
  slice(1) # Take the first line from each group
  ungroup()
  
  #or
  
  unique(a[, "new"])
  
 #or better (so you don't need the slice):

data %>% 
  group_by(id) %>% 
  summarise(word = paste0(word, collapse = " "))



