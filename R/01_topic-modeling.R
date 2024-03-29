# Install library yang dibutuhkan 
install.packages("tidyverse")
install.packages("tidytext")
install.packages("topicmodels")
install.packages("tm")
install.packages("SnowballC")

# Import Library
library(tidyverse) 
library(tidytext)
library(topicmodels)
library(tm)
library(SnowballC)

# read in our data
reviews <- read_csv("data-raw/berita-batubara.csv")

# function to get & plot the most informative terms by a specificed number
# of topics, using LDA
top_terms_by_topic_LDA <- function(input_text, # should be a columm from a dataframe
                                   plot = T, # return a plot? TRUE by defult
                                   number_of_topics = 4) # number of topics (4 by default)
{    
  # create a corpus (type of object expected by tm) and document term matrix
  Corpus <- Corpus(VectorSource(input_text)) # make a corpus object
  DTM <- DocumentTermMatrix(Corpus) # get the count of words/document
  
  # remove any empty rows in our document term matrix (if there are any 
  # we'll get an error when we try to run our LDA)
  unique_indexes <- unique(DTM$i) # get the index of each unique value
  DTM <- DTM[unique_indexes,] # get a subset of only those indexes
  
  # preform LDA & get the words/topic in a tidy text format
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  
  # get the top ten terms for each topic
  top_terms <- topics  %>% # take the topics data frame and..
    group_by(topic) %>% # treat each topic as a different group
    top_n(10, beta) %>% # get the top 10 most informative words
    ungroup() %>% # ungroup
    arrange(topic, -beta) # arrange words in descending informativeness
  
  # if the user asks for a plot (TRUE by default)
  if(plot == T){
    # plot the top ten terms for each topic in order
    top_terms %>% # take the top terms
      mutate(term = reorder(term, beta)) %>% # sort terms by beta value 
      ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
      geom_col(show.legend = FALSE) + # as a bar plot
      facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
      labs(x = NULL, y = "Beta") + # no x label, change y label 
      coord_flip() # turn bars sideways
  }else{ 
    # if the user does not request a plot
    # return a list of sorted terms instead
    return(top_terms)
  }
}


# create a document term matrix to clean
reviewsCorpus <- Corpus(VectorSource(reviews$text)) 
reviewsDTM <- DocumentTermMatrix(reviewsCorpus)

# convert the document term matrix to a tidytext corpus
reviewsDTM_tidy <- tidy(reviewsDTM)

# I'm going to add my own custom stop words that I don't think will be
# very informative in hotel reviews
custom_stop_words <- tibble(word = c("saya", "jangan", "isi", "maaf", "2018", "tiba", "katanya", "gara", "gimana", "pokoknya", "selama", "traveloka", "sama", "kami", "itu", "menggunakan", "pada", "aku", "padahal", "kita", "pas", "kali", "pelayanan", "proses", "...", "harus", "katanya", "terus", "karena", "menyelesaikan", "aplikasinya", "minta", "pokoknya", "sampe", "tidak", "aplikasi", "ini", "setelah", "seperti", "lebih", "mau", "yang", "jadi", "sangat", "sekali", "dan", "terimakasih", "untuk", "banget", "banyak", "ada", "sudah", "tapi", "belum", "dari", "kok", "nya", "juga", "sampai", "bisa", "kenapa", "kalau", "lumayan", "cukup", "biasa", "udah", "lagi", "pakai", "oke", "aja", "saja", "dengan", "semua", "dapat", "dalam", "baik", "buat", "baru", "disini", "lain", "000", "luar", "selalu", "saat", "masih", "sering", "katanya", "sampe"))

# remove stopwords
reviewsDTM_tidy_cleaned <- reviewsDTM_tidy %>% # take our tidy dtm and...
  anti_join(stop_words, by = c("term" = "word")) %>% # remove English stopwords and...
  anti_join(custom_stop_words, by = c("term" = "word")) # remove my custom stopwords

# reconstruct cleaned documents (so that each word shows up the correct number of times)
cleaned_documents <- reviewsDTM_tidy_cleaned %>%
  group_by(document) %>% 
  mutate(terms = toString(rep(term, count))) %>%
  select(document, terms) %>%
  unique()

# check out what the cleaned documents look like (should just be a bunch of content words)
# in alphabetic order
head(cleaned_documents)

# plot top ten terms in the hotel reviews by topic
top_terms_by_topic_LDA(cleaned_documents$terms, number_of_topics = 5)

