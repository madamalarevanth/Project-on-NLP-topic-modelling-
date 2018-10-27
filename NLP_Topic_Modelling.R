# general utility & workflow functions
library(tidyverse)
# tidy implimentation of NLP methods
library(tidytext)
# for LDA topic modelling
library(topicmodels)  
# general text mining functions, making document term matrixes
library(tm)
# for stemming
library(SnowballC)

# reading data
reviews <- read_csv("C:/Users/REVANTH/Desktop/deceptive-opinion.csv")

#Unsupervised topic modeling with LDA

# function to get & plot the most informative terms by a specificed number
# of topics, using LDA
top_terms_by_topic_LDA <- function(input_text, #text column
                                   plot = T, # return a plot? TRUE by defult
                                   number_of_topics = 4) # number of topics (4 by default)
{    
  Corpus <- Corpus(VectorSource(input_text)) # make a corpus object
  DTM <- DocumentTermMatrix(Corpus) # get the count of words/document
  
  # remove any empty rows / duplicate rows in our document term matrix (if there are any 
  #we'll get an error when we try to run our LDA)
  unique_indexes <- unique(DTM$i) # get the index of each unique value
  DTM <- DTM[unique_indexes,] # get a subset of only those indexes
  
  # preform LDA & get the words/topic in a tidy text format
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  
  # get the top ten terms for each topic
  top_terms <- topics  %>% # take the topics from data frame 
    group_by(topic) %>% # group each topic
    top_n(10, beta) %>% # get the top 10 most informative words
    ungroup() %>% # ungroup it 
    arrange(topic, -beta) # arrange words in descending informativeness
  
  if(plot == T){
    # plot the top ten terms for each topic in order
    top_terms %>% 
      mutate(term = reorder(term, beta)) %>% 
      ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
      geom_col(show.legend = FALSE) + # as a bar plot
      facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
      labs(x = NULL, y = "Beta") + # label the data
      coord_flip() # turn bars sideways
  }else{ 
    #return the top 10 list 
    return(top_terms)
  }
}



# creating a document term matrix to clean the text
reviewsCorpus <- Corpus(VectorSource(reviews$text)) 
reviewsDTM <- DocumentTermMatrix(reviewsCorpus)

# convert the document term matrix to a tidytext corpus
reviewsDTM_tidy <- tidy(reviewsDTM)

# as the modelling is done on hotwl rooms review these two words hotel and review aren't any intrest to us
#so considering these two words as stop words
custom_stop_words <- tibble(word = c("hotel", "room"))

# remove stopwords
reviewsDTM_tidy_cleaned <- reviewsDTM_tidy %>%
  anti_join(stop_words, by = c("term" = "word")) %>% # remove  stopwords and...
  anti_join(custom_stop_words, by = c("term" = "word")) # remove my custom stopwords

# reconstruct cleaned documents (so that each word shows up the correct number of times)
cleaned_documents <- reviewsDTM_tidy_cleaned %>%
  group_by(document) %>% 
  mutate(terms = toString(rep(term, count))) %>%
  select(document, terms) %>%
  unique()

head(cleaned_documents)

#get the top ten informative words list and plot the data 
top_terms_by_topic_LDA(cleaned_documents$terms, number_of_topics = 2)

#stemming the data to remove the inflection in the text 

# stem the words (e.g. convert each word to its stem, where applicable)
reviewsDTM_tidy_cleaned <- reviewsDTM_tidy_cleaned %>% 
  mutate(stem = wordStem(term))

# reconstruct our documents
cleaned_documents <- reviewsDTM_tidy_cleaned %>%
  group_by(document) %>% 
  mutate(terms = toString(rep(stem, count))) %>%
  select(document, terms) %>%
  unique()

# the new most informative terms
top_terms_by_topic_LDA(cleaned_documents$terms, number_of_topics = 2)

#supervised topic Modelling using TF-IDF

# function that takes in a dataframe and the name of the columns
# with the document texts and the topic labels. If plot is set to
# false it will return the tf-idf output rather than a plot.
top_terms_by_topic_tfidf <- function(text_df, text_column, group_column, plot = T){
  
  group_column <- enquo(group_column)
  text_column <- enquo(text_column)
  
  # get the count of each word in each review
  words <- text_df %>%
    unnest_tokens(word, !!text_column) %>%
    count(!!group_column, word) %>% 
    ungroup()
  
  # get the number of words per text
  total_words <- words %>% 
    group_by(!!group_column) %>% 
    summarize(total = sum(n))
  
  # combine the two dataframes we just made
  words <- left_join(words, total_words)
  
  # get the tf_idf & order the words by degree of relevence
  tf_idf <- words %>%
    bind_tf_idf(word, !!group_column, n) %>%
    select(-total) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word))))
  
  if(plot == T){
    # convert "group" into a quote of a name
    group_name <- quo_name(group_column)
    
    # plot the 10 most informative terms per topic
    tf_idf %>% 
      group_by(!!group_column) %>% 
      top_n(10) %>% 
      ungroup %>%
      ggplot(aes(word, tf_idf, fill = as.factor(group_name))) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(reformulate(group_name), scales = "free") +
      coord_flip()
  }else{
    # return the entire tf_idf dataframe
    return(tf_idf)
  }
}

# let's see what our most informative deceptive words are
top_terms_by_topic_tfidf(text_df = reviews,text_column = text,group_column = deceptive,plot = T) 

#from the plot we can see that 
#false reviews tend to use a lot of glowing praise in their reviews ("pampered", "exquisite"), while
# truthful reviews tend to talk about how they booked thier room ("priceline", "hotwire")


# look for the most informative words for postive and negative reveiws
top_terms_by_topic_tfidf(text_df = reviews,text_column = text,group = polarity,plot = T)

#we can see that negative reviews include words like "worst", "broken", "odor" and "stains", while 
#positive reviews really harp on the bathrobes (both "robes" and "bathrobes") and spotless and stunning


# justr get the tf-idf output for the hotel topics
reviews_tfidf_byHotel <- top_terms_by_topic_tfidf(text_df = reviews,text_column = text, group = hotel,plot = F)

# do our own plotting
reviews_tfidf_byHotel  %>% 
  group_by(hotel) %>% #grouping by hotel
  top_n(5) %>% #get the top 5 topics
  ungroup %>%  #ungroup it 
  ggplot(aes(word, tf_idf, fill = hotel)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~hotel, ncol = 4, scales = "free", ) +
  coord_flip()
