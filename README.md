# Restaurant Review Sentiment Classifier

A bag-of-words NLP pipeline in R that classifies restaurant reviews as
positive or negative sentiment. Trained on 1,000 labeled reviews (`Liked`:
1 = positive, 0 = negative).

## Pipeline

1. **Text cleaning** (via `tm` + `SnowballC`) — lowercasing, punctuation
   removal, stopword removal, and stemming on each review.
2. **Bag-of-words model** — builds a document-term matrix from the cleaned
   corpus, then drops terms appearing in fewer than 0.1% of reviews to
   control dimensionality.
3. **Classification** — an 80/20 train/test split, with a Random Forest
   classifier (`ntree = 10`) trained on the resulting term-frequency
   features.

## Results

Evaluated via confusion matrix on the 200-review test set:

| Metric | Value |
|---|---|
| Accuracy | ~79.5% |

## Tech Stack

- **R**
- **tm** / **SnowballC** — text cleaning and stemming
- **randomForest** — classification
- **caTools** — train/test splitting

## Running it

1. Install the required packages:
   ```r
   install.packages(c("readr", "tm", "SnowballC", "caTools", "randomForest"))
   ```
2. Place `Restaurant_Reviews.tsv` (tab-separated, with `Review` and `Liked`
   columns) in the working directory.
3. Run `Reviews.R` in R or RStudio.

## Project Structure

```
.
└── Reviews.R   # Text cleaning, bag-of-words, Random Forest classifier, evaluation
```
