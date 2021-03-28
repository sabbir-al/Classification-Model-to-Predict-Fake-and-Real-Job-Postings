install.packages("ggplot2")
install.packages("RColorBrewer")
install.packages("wordcloud2")
install.packages("DataExplorer")
install.packages("scales")
library(scales)
library(ggplot2)
library(widyr)
library(RColorBrewer)
library(wordcloud2)
library(caret)
library(quanteda)


fjp <- read.csv("C:/Users/Sabbir.alam/Downloads/fake_job_postings.csv",header=T,na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
fjp$fraudulent = as.factor(fjp$fraudulent)
fjp$description = as.character(fjp$description)
fjp$telecommuting = as.factor(fjp$telecommuting)
fjp$has_company_logo = as.factor(fjp$has_company_logo)
fjp$has_questions = as.factor(fjp$has_questions)
fjp$employment_type = as.factor(fjp$employment_type)
fjp$required_experience = as.factor(fjp$required_experience)
fjp$required_education = as.factor(fjp$required_education)
fjp$industry = as.factor(fjp$industry)
fjp$function. = as.factor(fjp$function.)


#############################################################
######################BARCHARTS##############################
#Barchart of No. of fraud vs non-fraud with fraud
ggplot(fjp, aes(fraudulent))+
  geom_bar(aes(fill = fraudulent), stat = "count") + 
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  geom_text(aes(label=..count..),stat='count',position=position_stack(vjust=0.5)) +
  ggtitle("Real vs. Fake Jobs Count") + xlab("0=Not Fraud OR 1=Fraud") + ylab("Job Count") + theme_bw()


#Barchart of Company Logo or Not with fraud
ggplot(fjp, aes(x = has_company_logo, fill = fraudulent)) +
  geom_bar(position = 'dodge', aes(y = (..count..)/sum(..count..))) +   
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.25, size = 4, position = position_dodge(width = 0.9)) + 
  scale_y_continuous(labels = percent) + 
  ylab("Percent") + xlab("Does the posting have company logo") +   
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

ggplot(fjp, aes(has_company_logo, fill = fraudulent))+
  geom_bar(position = 'fill')+
  theme_classic()+
  labs(title = 'Company logo with fradulent ratio')+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


#Barchart of Questions or no Questions with fraud
ggplot(fjp, aes(x = has_questions, fill = fraudulent)) +
  geom_bar(position = 'dodge', aes(y = (..count..)/sum(..count..))) +   
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.25, size = 4, position = position_dodge(width = 0.9)) + 
  scale_y_continuous(labels = percent) + 
  ylab("Percent") + xlab("Does the job inteview have questions") +   
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

ggplot(fjp, aes(has_questions, fill = fraudulent))+
  geom_bar(position = 'fill')+
  theme_classic()+
  labs(title = 'Has_questions with fradulent ratio')+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


#Barchart of Employment type with fraud
ggplot(fjp, aes(x = employment_type, fill = fraudulent)) +
  geom_bar(position = 'dodge', aes(y = (..count..)/sum(..count..))) +   
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.25, size = 3.5, position = position_dodge(width = 0.9)) + 
  scale_y_continuous(labels = percent) + 
  ylab("Percent") + xlab("Employment type with fraudulent") +   
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

#Barchart of employment type with different education levels
ggplot(fjp, aes(employment_type))+
  geom_bar(aes(fill = required_education), stat = "count") + 
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  geom_text(aes(label=..count..),stat='count',position=position_stack(vjust=0.5)) +
  ggtitle("Count of employment type with different education levels") + xlab("") + ylab("Job Count") + theme_bw()

#Missing values percent graph
library(DataExplorer)
plot_missing(fjp,title="Percent of missing values")



#############################################################
######################Word Clouds############################

set.seed(12345)
bob = createDataPartition(fjp$fraudulent, p=0.7, list = FALSE)
train = fjp[bob,]

##########
#Word clouds for fraud and non fraud for description column
token.fraudandnonfraud=split(train$description,train$fraudulent)

tokens.fraud <- tokens(token.fraudandnonfraud$`1`, what = "word", remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, split_hyphens = TRUE)
tokens.fraud <- tokens_tolower(tokens.fraud)
tokens.fraud <- tokens_select(tokens.fraud, stopwords(), selection = "remove")
tokens.fraud <- tokens_wordstem(tokens.fraud, language = "english")
tokens.fraud <- tokens_ngrams(tokens.fraud, n = 1:2)

tokens.nonfraud <- tokens(token.fraudandnonfraud$`0`, what = "word", remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, split_hyphens = TRUE)
tokens.nonfraud <- tokens_tolower(tokens.nonfraud)
tokens.nonfraud <- tokens_select(tokens.nonfraud, stopwords(), selection = "remove")
tokens.nonfraud <- tokens_wordstem(tokens.nonfraud, language = "english")
tokens.nonfraud <- tokens_ngrams(tokens.nonfraud, n = 1:2)

dfm_fraud <- dfm(tokens.fraud, tolower = F)
dfm_nonfraud <- dfm(tokens.nonfraud, tolower = F)

#Wordcloud of not fraud in description
words_nonfraud <- sort(colSums(dfm_nonfraud),decreasing=TRUE) 
df_nonfraud <- data.frame(word = names(words_nonfraud),freq=words_nonfraud)
wordcloud2(data=df_nonfraud, size=0.7, color='random-dark')

#Wordcloud of fraud in description
words_fraud <- sort(colSums(dfm_fraud),decreasing=TRUE) 
df_fraud <- data.frame(word = names(words_fraud),freq=words_fraud)
wordcloud2(data=df_fraud, size=0.7, color='random-dark')


###########
#Word clouds for fraud and non fraud for compant profile column
token.fraudandnonfraud=split(train$company_profile,train$fraudulent)

tokens.fraud <- tokens(token.fraudandnonfraud$`1`, what = "word", remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, split_hyphens = TRUE)
tokens.fraud <- tokens_tolower(tokens.fraud)
tokens.fraud <- tokens_select(tokens.fraud, stopwords(), selection = "remove")
tokens.fraud <- tokens_wordstem(tokens.fraud, language = "english")
tokens.fraud <- tokens_ngrams(tokens.fraud, n = 1:2)

tokens.nonfraud <- tokens(token.fraudandnonfraud$`0`, what = "word", remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, split_hyphens = TRUE)
tokens.nonfraud <- tokens_tolower(tokens.nonfraud)
tokens.nonfraud <- tokens_select(tokens.nonfraud, stopwords(), selection = "remove")
tokens.nonfraud <- tokens_wordstem(tokens.nonfraud, language = "english")
tokens.nonfraud <- tokens_ngrams(tokens.nonfraud, n = 1:2)

dfm_fraud <- dfm(tokens.fraud, tolower = F)
dfm_nonfraud <- dfm(tokens.nonfraud, tolower = F)

#Wordcloud of non fraud in company_profile
words_nonfraud <- sort(colSums(dfm_nonfraud),decreasing=TRUE) 
df_nonfraud <- data.frame(word = names(words_nonfraud),freq=words_nonfraud)
wordcloud2(data=df_nonfraud, size=0.7, color='random-dark')

#Wordcloud of fraud in company_profile
words_fraud <- sort(colSums(dfm_fraud),decreasing=TRUE) 
df_fraud <- data.frame(word = names(words_fraud),freq=words_fraud)
wordcloud2(data=df_fraud, size=0.7, color='random-dark')

###########
#Word clouds for fraud and non fraud for benefits column
token.fraudandnonfraud=split(train$benefits,train$fraudulent)

tokens.fraud <- tokens(token.fraudandnonfraud$`1`, what = "word", remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, split_hyphens = TRUE)
tokens.fraud <- tokens_tolower(tokens.fraud)
tokens.fraud <- tokens_select(tokens.fraud, stopwords(), selection = "remove")
tokens.fraud <- tokens_wordstem(tokens.fraud, language = "english")
tokens.fraud <- tokens_ngrams(tokens.fraud, n = 1:2)

tokens.nonfraud <- tokens(token.fraudandnonfraud$`0`, what = "word", remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, split_hyphens = TRUE)
tokens.nonfraud <- tokens_tolower(tokens.nonfraud)
tokens.nonfraud <- tokens_select(tokens.nonfraud, stopwords(), selection = "remove")
tokens.nonfraud <- tokens_wordstem(tokens.nonfraud, language = "english")
tokens.nonfraud <- tokens_ngrams(tokens.nonfraud, n = 1:2)

dfm_fraud <- dfm(tokens.fraud, tolower = F)
dfm_nonfraud <- dfm(tokens.nonfraud, tolower = F)

#Wordcloud of non fraud in benefits
words_nonfraud <- sort(colSums(dfm_nonfraud),decreasing=TRUE) 
df_nonfraud <- data.frame(word = names(words_nonfraud),freq=words_nonfraud)
wordcloud2(data=df_nonfraud, size=0.7, color='random-dark')

#Wordcloud of fraud in benefits
words_fraud <- sort(colSums(dfm_fraud),decreasing=TRUE) 
df_fraud <- data.frame(word = names(words_fraud),freq=words_fraud)
wordcloud2(data=df_fraud, size=0.7, color='random-dark')

###########
#Word clouds for fraud and non fraud for requirement column
token.fraudandnonfraud=split(train$requirements,train$fraudulent)

tokens.fraud <- tokens(token.fraudandnonfraud$`1`, what = "word", remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, split_hyphens = TRUE)
tokens.fraud <- tokens_tolower(tokens.fraud)
tokens.fraud <- tokens_select(tokens.fraud, stopwords(), selection = "remove")
tokens.fraud <- tokens_wordstem(tokens.fraud, language = "english")
tokens.fraud <- tokens_ngrams(tokens.fraud, n = 1:2)

tokens.nonfraud <- tokens(token.fraudandnonfraud$`0`, what = "word", remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, split_hyphens = TRUE)
tokens.nonfraud <- tokens_tolower(tokens.nonfraud)
tokens.nonfraud <- tokens_select(tokens.nonfraud, stopwords(), selection = "remove")
tokens.nonfraud <- tokens_wordstem(tokens.nonfraud, language = "english")
tokens.nonfraud <- tokens_ngrams(tokens.nonfraud, n = 1:2)

dfm_fraud <- dfm(tokens.fraud, tolower = F)
dfm_nonfraud <- dfm(tokens.nonfraud, tolower = F)

#Wordcloud of non fraud in requirement
words_nonfraud <- sort(colSums(dfm_nonfraud),decreasing=TRUE) 
df_nonfraud <- data.frame(word = names(words_nonfraud),freq=words_nonfraud)
wordcloud2(data=df_nonfraud, size=0.7, color='random-dark')

#Wordcloud of fraud in requirement
words_fraud <- sort(colSums(dfm_fraud),decreasing=TRUE) 
df_fraud <- data.frame(word = names(words_fraud),freq=words_fraud)
wordcloud2(data=df_fraud, size=0.7, color='random-dark')


###########
#creating a feature co-occurance matrix
#Run this code after the main program has executed
dfm1_trim=dfm_trim(train.tokens.dfm,min_termfreq=850)
fcm_1<-fcm(dfm1_trim)
feat <- names(topfeatures(fcm_1, 100))
fcm_select1 <- fcm_select(fcm_1, pattern = feat)
size <- log(colSums(dfm_select(dfm3, feat)))
textplot_network(fcm_select1, min_freq = 0.8, vertex_size = size / max(size) * 3)


