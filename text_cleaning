source: https://towardsdatascience.com/text-mining-with-r-gathering-and-cleaning-data-8f8b0d65e67c


# Get the text column
text <- data_fix$text

# Set the text to lowercase
text <- tolower(text)

# Remove mentions, urls, emojis, numbers, punctuations, etc.
text <- gsub("@\\w+", "", text)
text <- gsub("https?://.+", "", text)
text <- gsub("\\d+\\w*\\d*", "", text)
text <- gsub("#\\w+", "", text)
text <- gsub("[^\x01-\x7F]", "", text)
text <- gsub("[[:punct:]]", " ", text)

# Remove spaces and newlines
text <- gsub("\n", " ", text)
text <- gsub("^\\s+", "", text)
text <- gsub("\\s+$", "", text)
text <- gsub("[ |\t]+", " ", text)

# Put the data to a new column
data_fix["fix_text"] <- text
head(data_fix$fix_text, 10)





library(tidytext)

# Remove Stopwords
stop_id <- scan(paste(getwd(), "/stopwords-id.txt", sep=""), character(), sep="\n")

# Create dataframe of the stop words
stop_words <- data.frame(
  word <- stop_id,
  stringsAsFactors = F
)
colnames(stop_words) <- "word"

# Convert to tidy format
tidy_text <- data_fix %>%
  select(created_at, id, fix_text) %>%
  # Tokenize the word from the tweets
  unnest_tokens(input = fix_text, output = word) %>%
  # Remove stop words
  anti_join(stop_words, by="word")
