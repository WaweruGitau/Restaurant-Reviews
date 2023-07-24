# Since the file had an issue with line endings, we imported data differently

library(readr)
dataset <- read_tsv("Restaurant_Reviews.tsv")
dataset

# Cleaning text
library(tm)
library(SnowballC)
corpus = VCorpus(VectorSource(dataset$Review))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords())
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)

# Creating bag of words model
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.999)

# Converting to a Dataframe
Dataset = as.data.frame(as.matrix(dtm))
Dataset$Liked = dataset$Liked


# Fitting a classification Model
# RandomForest

# Encoding target feature as a factor
Dataset$Liked = factor(Dataset$Liked, levels = c(0,1))

# Splitting into training and test set
install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(Dataset$Liked, SplitRatio = 0.8)
training_set = subset(Dataset, split == TRUE)
test_set = subset(Dataset, split == FALSE)


install.packages("randomForest")
library(randomForest)
classifier = randomForest(x= training_set[-692],
                          y= training_set$Liked,
                          ntree = 10)

#Predicting Test Results
y_pred = predict(classifier, newdata = test_set[-692])

#Confussion matrix
cm = table(test_set[,692], y_pred)

# Accuracy
Acc = (82 + 77)/200






