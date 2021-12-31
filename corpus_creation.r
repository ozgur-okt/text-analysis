myCorpus<-Corpus(VectorSource(myFile$myColumn)) #converts the relevant part of your file into a corpus

myCorpus = tm_map(myCorpus, PlainTextDocument) # an intermediate preprocessing step

myCorpus = tm_map(myCorpus, tolower) # converts all text to lower case

myCorpus = tm_map(myCorpus, removePunctuation) #removes punctuation

myCorpus = tm_map(myCorpus, removeWords, stopwords("english")) #removes common words like “a”, “the” etc

myCorpus = tm_map(myCorpus, stemDocument) # removes the last few letters of similar words such as get, getting, gets

dtm = DocumentTermMatrix(myCorpus) #turns the corpus into a document term matrix

notSparse = removeSparseTerms(dtm, 0.99) # extracts frequently occuring words

finalWords=as.data.frame(as.matrix(notSparse)) # most frequent words remain in a dataframe, with one column per word

words<-data.frame(colnames(finalWords), colSums(finalWords))
View(words)

words<-words[,-1]

wordss<-data.frame(colnames(finalWords), words)

barplot(wordss$words, names.arg = TRUE)
