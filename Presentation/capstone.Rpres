Word Predict
========================================================
author: Juan Carlos Gonzalez Cardona
date: 3-dic-2017
autosize: true

Word predict is a Shiny app write in R to predict the next word based a sentence write for user using a 5-gram language model and Katz's Backoff Model.
Is designed for used of diferents corpus and size of samples for original sources.

[Shiny web app](https://dechontaduro.shinyapps.io/predictnextword/)     
[Exploratory Corpus](http://rpubs.com/dechontaduro/CapstoneExploratory)

How It Works
========================================================
- First select corpus: I think that the prediction can be better depends of context.
- Write your sentence: while 
- View results
  + Gram: First terms of gram used for predictions.
  + Next word: Best next word according to final probability using discount and Katz's Backoff.
  + Grams: Table with all predictions ordered by probability.

Katz's backoff implementation was based on Thachtranerc's article. [Katz's Backoff Model Implementation in R](https://thachtranerc.wordpress.com/2016/04/12/katzs-backoff-model-implementation-in-r/)


1.Algorithm
========================================================
1. Create ngrams (1 to 5) to predict [ngrams](https://github.com/dechontaduro/DataScienceCapstoneJHU/blob/master/ngrams.R)
  + Sample factor: 30%, size of sample to create model
  + minDocfreq: 2, Counts that grams appear in a corpus.
  + Merge all samples in one named "all".
  + Don't remove stopWords and not used steamming.
  + Used quanteda to create tokens and ngrams.
  + Calculate Good-Turing Discounting to used with Katz's Backoff.
  + Due to memory limits Between stages save files in memory and remove objects, and the next stage load previous files.

2.Algorithm
========================================================
2. Clean, split and count words in sentence to define with which ngram model start.
3. Looking across ngrams models finding the sentence until found grams matching.
4. Calculate final probability with Katz's backoff
5. Take the first row of results and show last term.

Code from generate ngrams to predict, and shiny app are in [Code GitHub](https://github.com/dechontaduro/DataScienceCapstoneJHU)

Next steps
========================================================
- Can change sample size to used huge corpus when is possible.
- Evaluate performance change minDocfreq for each ngram model.
- Use of synonymous for improve coverage.
- Improve usability and look and feel using css.
- Catch errors.

