library(randomForest)
library(quanteda)
library(caret)
library(irlba)
library(readxl)
library(ISLR)
library(e1071)
library(ggplot2)


#IMPORTING THE DATASET
fjp <- read_excel("D:/AUS/Semester 4/STA 401/Project data/fake_job_postings.xlsx")
colnames(fjp)


#SPECIFYING THE CLASS OF THE DATA
fjp$fraudulent = as.factor(fjp$fraudulent)
fjp$description = as.character(fjp$description)
fjp$telecommuting = as.factor(fjp$telecommuting)
fjp$has_company_logo = as.factor(fjp$has_company_logo)
fjp$has_questions = as.factor(fjp$has_questions)
fjp$employment_type = as.factor(fjp$employment_type)
fjp$required_experience = as.factor(fjp$required_experience)
fjp$required_education = as.factor(fjp$required_education)
fjp$industry = as.factor(fjp$industry)
fjp$func = as.factor(fjp$func)


#CREATING THE DATA PARTITION
set.seed(12345)
bob = createDataPartition(fjp$fraudulent, p=0.7, list = FALSE)
train = fjp[bob,]
test = fjp[-bob,]
dim(test)


#FOR PREDICTION, ONLY RUN THE TOKENIZATION CODE FOR THE DESCRIPTION
#FOR ANALYSIS AND VISUALIZTION, RUN ALL 4 OF THEM (ALONG WITH THE OTHER R CODE)
#TOKENIZING THE DESCRIPTION
train.tokens <- tokens(train$description, what = "word", remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, split_hyphens = TRUE)
train.tokens <- tokens_tolower(train.tokens)
train.tokens <- tokens_select(train.tokens, stopwords(), selection = "remove")
train.tokens <- tokens_wordstem(train.tokens, language = "english")
train.tokens <- tokens_ngrams(train.tokens, n = 1:2)

train.tokens1 <- tokens(train$company_profile, what = "word", remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, split_hyphens = TRUE)
train.tokens1 <- tokens_tolower(train.tokens1)
train.tokens1 <- tokens_select(train.tokens1, stopwords(), selection = "remove")
train.tokens1 <- tokens_wordstem(train.tokens1, language = "english")
train.tokens1 <- tokens_ngrams(train.tokens1, n = 1:2)

train.tokens2 <- tokens(train$requirements, what = "word", remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, split_hyphens = TRUE)
train.tokens2 <- tokens_tolower(train.tokens2)
train.tokens2 <- tokens_select(train.tokens2, stopwords(), selection = "remove")
train.tokens2 <- tokens_wordstem(train.tokens2, language = "english")
train.tokens2 <- tokens_ngrams(train.tokens2, n = 1:2)

train.tokens3 <- tokens(train$benefits, what = "word", remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, split_hyphens = TRUE)
train.tokens3 <- tokens_tolower(train.tokens3)
train.tokens3 <- tokens_select(train.tokens3, stopwords(), selection = "remove")
train.tokens3 <- tokens_wordstem(train.tokens3, language = "english")
train.tokens3 <- tokens_ngrams(train.tokens3, n = 1:2)


#CREATING THE DFM FOR THE TOKENS
train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)


#DO NOT NEED TO RUN THESE 6 LINES
dfm1 <- dfm(train.tokens1, tolower = F)
dfm2 <- dfm(train.tokens2, tolower = F)
dfm3 <- dfm(train.tokens3, tolower = F)
train.tokens.dfm <- cbind(train.tokens.dfm,dfm1)
train.tokens.dfm <- cbind(train.tokens.dfm,dfm2)
train.tokens.dfm <- cbind(train.tokens.dfm,dfm3)


#CREATING THE FINALZIED TRAIN DFM
train.tokens.dfm = dfm_compress(train.tokens.dfm, margin = "features")
dim(train.tokens.dfm)


#CALCULATIING AND APPLYING THE TFIDF ON THE TRAINING DATA
train.tokens.tfidf <- dfm_tfidf(train.tokens.dfm, scheme_tf = "prop", scheme_df = "inverse", base = 10)


#RUNNING THE SVD CODE
#PLEASE NOTE THAT THIS WILL TAKE A LONG TIME, AROUND 3 HOURS 
start.time <- Sys.time()
train.tokens.svd = irlba(train.tokens.tfidf, nv = 300)
total.time <- Sys.time() - start.time
total.time


#REQUIRED FOR PROJECTING THE TEST DATA ONTO THE TRAINING DATA
dim(train.tokens.svd$u)
sigma.inverse <- 1 / train.tokens.svd$d
v.transpose <- t(train.tokens.svd$v)
dim(train.tokens.svd$v)


#UTILIZING THE U MATRIX THAT NOW CONTAINS THE CONDENSED 300 COLUMNS
#AND MAKING IT OUR FINAL TRAINING DATA BY ADDING THE RESPONSE AND REQUIRED VARIABLES
train.tokens.final = as.data.frame(train.tokens.svd$u)
train.tokens.final$fraud = train$fraudulent 
train.tokens.final$func = as.factor(train$func)
train.tokens.final$q = as.factor(train$has_questions)
train.tokens.final$clogo = as.factor(train$has_company_logo)
train.tokens.final$telecommuting = as.factor(train$telecommuting)
train.tokens.final$employment_type = as.factor(train$employment_type)
train.tokens.final$required_experience = as.factor(train$required_experience)
train.tokens.final$required_education = as.factor(train$required_education)


#train.tokens.final$cprof = train$company_profile
#train.tokens.final$cprof[!is.na(train.tokens.final$cprof)] = "1"
#train.tokens.final$cprof[is.na(train.tokens.final$cprof)] = "0"
#train.tokens.final$cprof = as.factor(train.tokens.final$cprof)


#VARIABLES DROPPED FROM THE RANDOM FOREST
#DO NOT RUN
train.tokens.final$industry = as.factor(train$industry)
train.tokens.final$title = as.character(train$title)
train.tokens.final$location = as.character(train$location)
train.tokens.final$department = as.character(train$department)
train.tokens.final$srange = as.character(train$salary_range)
train.tokens.final = na.roughfix(train.tokens.final)


#TRAINING THE RANDOM FOREST MODEL AND CREATING THE VARIABLE IMPORTANCE PLOT
set.seed(12345)
train.rf = randomForest(fraud~., data = train.tokens.final, cv.folds = 5, mtry = ncol(train.tokens.final)/2 , ntree = 100, na.action = na.roughfix)
confusionMatrix(train.rf$predicted, train.tokens.final$fraud)
Random_Forest = train.rf
varImpPlot(Random_Forest,n.var = 10)


#TOKENIZING THE TEST DATA
test.tokens <- tokens(test$description, what = "word", remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, split_hyphens = TRUE)
test.tokens <- tokens_tolower(test.tokens)
test.tokens <- tokens_select(test.tokens, stopwords(), selection = "remove")
test.tokens <- tokens_wordstem(test.tokens, language = "english")
test.tokens <- tokens_ngrams(test.tokens, n = 1:2)


#CONVERTING INTO A DFM AND MATCHING IT WITH THE TRAIN DATA
#AND THEN PERFORMING TFIDF
test.tokens.dfm <- dfm(test.tokens, tolower = FALSE)
test.tokens.dfm <- dfm_match(test.tokens.dfm, features = colnames(train.tokens.dfm))
test.tokens.tf = dfm_tfidf(test.tokens.dfm, scheme_tf = "prop", scheme_df = "inverse", base = 10)


#PROJECTING THE TEST DATA ONTO THE TRAINING DATA
#CONVERTING THE SVD PROJECTED TEST DATA INTO A DATA FRAME
#APPENDING ALL THE REQUIRED COLUMNS
test.tokens.final = t(sigma.inverse * v.transpose %*% t(test.tokens.tf))
test.tokens.final = as.matrix(test.tokens.final)
test.tokens.final = as.data.frame(test.tokens.final)
test.tokens.final$clogo = as.factor(test$has_company_logo)
test.tokens.final$q = as.factor(test$has_questions)
test.tokens.final$func = as.factor(test$func)
test.tokens.final$telecommuting = as.factor(test$telecommuting)
test.tokens.final$employment_type = as.factor(test$employment_type)
test.tokens.final$required_experience = as.factor(test$required_experience)
test.tokens.final$required_education = as.factor(test$required_education)


#IF CPROF WAS INCLUDED EARLIER, INCLUDE THIS
#test.tokens.final$cprof = test$company_profile
#test.tokens.final$cprof[!is.na(test.tokens.final$cprof)] = "1"
#test.tokens.final$cprof[is.na(test.tokens.final$cprof)] = "0"
#test.tokens.final$cprof = as.factor(test.tokens.final$cprof)


#DROPPED FROM THE MODEL REQUIRED TO RUN RANDOM FOREST
#DO NOT RUN
test.tokens.final$industry = as.factor(test$industry)
test.tokens.final$title = as.character(test$title)
test.tokens.final$location = as.character(test$location)
test.tokens.final$department = as.character(test$department)
test.tokens.final$srange = as.character(test$salary_range)


#RANDOM FOREST PREDICTION
test.tokens.final = na.roughfix(test.tokens.final)
names(test.tokens.final) <- make.names(names(test.tokens.final)) 
pred = as.factor(predict(train.rf,test.tokens.final))
Answer = as.factor(test$fraudulent)
confusionMatrix(pred, Answer)