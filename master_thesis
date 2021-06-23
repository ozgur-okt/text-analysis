
                                          #IMPORT

df<-X203_text

                                     #FINGER PRINT FUNC


finger<-get_fingerprint_from_text(texts = text)




                                # CHARACTER TO NUMERIC 

colnames(df[37:57])
table(df$PE1)

df2 <- df %>% mutate_at(c("GC_1" , "GC_2",  "GC_3" , "GC_4" , "GC_5" , "GC_6",  "GC_7" , "GC_8" , "GC_9" , "GC_10" ,
                          "GC_11", "GC_12", "GC_13" ,"GC_14" ,"GC_15", "GC_16"), 
                        funs(recode(.,"Definitely not true"=1, "Probably not true"=2, "Not sure / cannot decide"=3, "Probably true" = 4, "Definitely true"=5)))




df3 <- df2 %>% mutate_at(c("PE1" , "PE2" , "PE3" , "PE4" , "PE5" , "PE6" , "PE7" , "PE8" , "PE9" , "PE10","PE11" ,"PE12", "PE13" ,"PE14" ,"PE15", "PE16", "PE17", "PE18" ,"PE19" ,"PE20" ,"PE21"), 
                         
                         
                         funs(recode(.,"Strongly Disagree"=1, "Disagree"=2, "Neutral"=3, "Agree" = 4, "Strongly Agree"=5)))


#df3 <- df3 %>% mutate_at(c("PE1" , "PE2" , "PE3" , "PE4" , "PE5" , "PE6" , "PE7" , "PE8" , "PE9" , "PE10"), 
                         
                         
                         funs(recode(.,"Strongly Disagree"=5, "Disagree"=4, "Neutral"=3, "Agree" = 2, "Strongly Agree"=1)))

## REMOVE

which(colnames(df3)=="GC_15" )

df3<-df3[,-36]


  
  which(colnames(df3)=="PE21")

df3<-df3[,-56]


  
                        ## NEW COLM VIOLENCE CT BELIEF AND NORMATIVE
  
which(colnames(df3)=="GC_15" )

df3$ctbelief<-rowMeans(df3[,21:35], na.rm = TRUE)


which(colnames(df3)=="PE20")

df3$violence<-rowMeans(df3[,46:55], na.rm = TRUE)

df3$normative<-rowMeans(df3[,36:45], na.rm = TRUE)



                                         # TEST

cor.test(df3$violence,df3$ctbelief)

t.test(df3$violence,df3$ctbelief)

mean(df3$GC_1)

plot(df3$violence, df3$ctbelief)


                                #FINGER AND VIOLENCE - BELIEF COR

cor.test(df3$violence,finger$D_Liwc_Anger)

vbt<-data.frame(df3$ctbelief, df3$violence,df3$normative, finger) 





###  lm([target variable] ~ [predictor variables], data = [data source])

summary(lm(formula= vbt$D_Empath_aggression~vbt$df3.ctbelief + vbt$df3.violence, data= vbt))

summary(lm(vbt$D_Empath_anger~vbt$df3.ctbelief + vbt$df3.violence, data= vbt))

summary(lm(vbt$D_Liwc_Anger~vbt$df3.ctbelief + vbt$df3.violence, data= vbt))

summary(lm(vbt$D_Empath_hate~vbt$df3.ctbelief + vbt$df3.violence, data= vbt))

summary(lm(vbt$D_Empath_violence ~vbt$df3.ctbelief + vbt$df3.violence, data= vbt))


###

summary(lm(formula= vbt$D_Liwc_Negemo~vbt$df3.ctbelief + vbt$df3.violence, data= vbt))

summary(lm(vbt$D_Liwc_Anx~vbt$df3.ctbelief + vbt$df3.violence, data= vbt))

summary(lm(vbt$D_Liwc_Sad~vbt$df3.ctbelief + vbt$df3.violence, data= vbt))

summary(lm(vbt$D_Empath_crime~vbt$df3.ctbelief + vbt$df3.violence, data= vbt))

summary(lm(vbt$D_Empath_nervousness ~vbt$df3.ctbelief + vbt$df3.violence, data= vbt))

summary(lm(vbt$D_Empath_sadness ~vbt$df3.ctbelief + vbt$df3.violence, data= vbt))


Liwc_Negemo -> negative emotions
Liwc_Anx
Liwc_Sad
Empath_crime
Empath_nervousness
Empath_sadness

                             # REMOVE SAME TEXT and BAD TEXT

which(df3$IPAddress== "23.226.24.204")

df3<-df3[-86,]

which(df3$IPAddress== "31.18.80.131")

df3<-df3[-197,]

which(df3$IPAddress== "89.205.137.203")

df3<-df3[-196,]

which(df3$IPAddress== "51.9.162.138")

df3<-df3[-195,]




                                     # TEXT CLEANING 

# Get the text column
text <- df3$S1

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
