##### LIBRARIES ####
library(ggplot2)
library(widyr)
library(stringr)
library(dplyr)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tidytext)
library(tidyverse)
library(quanteda) # for working with corpora
library(quanteda.textplots) # for plotting 'keyness'
library(readtext) # for getting documents and their info into a data frame
library(quanteda.textstats)
#require(quanteda.corpora)
require(ggplot2)
library(readr)
library(tidyverse)
library(tidytext)
library(textdata)
library(readr)
library(utils)
library(readxl)
library(tidyverse)
library(tidytext)
library(tm)
library(stopwords)
library(hunspell)
library(textstem)
library(tidytext)
library(dplyr)
library(readxl)
library(tidyr)
library(ggraph)
library(igraph)
library(grid)
#install.packages("LDAvis")
library(LDAvis)
##### DATA CLEANING AND TOKENISATION #####
data <- read_excel("propalestine.xlsx")
words <- gsub("[[:punct:]]", "", data$title)
tokens_df <- data.frame(Combined_Text = words) %>%
  unnest_tokens(word, Combined_Text)

tokens_df <- data.frame(Combined_Text = words) %>%
  unnest_tokens(word, Combined_Text) %>%
  filter(!is.numeric(word)) %>%
  anti_join(get_stopwords(source = "smart", language = "en"), by = c("word" = "word")) %>%
  filter(!word %in% c("israel","says","palestine","â","sa","2","icj","palestinians","hamas","israelhamas","israeli", "palestinian", "gaza","israel","war"))


freq_analysis <- tokens_df %>%
  count(word, sort = TRUE)

head(freq_analysis, 20)

##### WORDCLOUD #####
# Show top words by frequency

top_words <- head(freq_analysis, 100)  # Change 100 to the number of top words you want to display

# Create square word cloud
wordcloud2(data = top_words, size = 0.5, backgroundColor = "white", shape = "square")


##### NRC lexicon ###########
nrc_lexicon <- get_sentiments("nrc")
nrc<-get_sentiments("nrc")
sentiment_df <- tokens_df %>%
  inner_join(nrc_lexicon, by = "word")

sentiment_counts <- sentiment_df %>%
  group_by(sentiment) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

head(sentiment_counts)

ggplot(sentiment_counts, aes(x = reorder(sentiment, count), y = count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Sentiment", y = "Count", title = "Overall Sentiment Distribution") +
  theme_minimal()

# 10 top positve and Negative words:
positive_words <- sentiment_df %>%
  filter(sentiment == "positive")

negative_words <- sentiment_df %>%
  filter(sentiment == "negative")

positive_word_counts <- positive_words %>%
  count(word, sort = TRUE)

negative_word_counts <- negative_words %>%
  count(word, sort = TRUE)

head(positive_word_counts, 20)
head(negative_word_counts, 20)

top_positive_words <- positive_word_counts %>%
  head(10)

top_negative_words <- negative_word_counts %>%
  head(10)

ggplot(top_positive_words, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(x = "Words", y = "Count", title = "Top 10 Positive Words") +
  theme_minimal()

ggplot(top_negative_words, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(x = "Words", y = "Count", title = "Top 10 Negative Words") +
  theme_minimal()

# other visuals for nrc
word_sent2 <- tokens_df %>% 
  inner_join(nrc_polarity, by = c('word' = 'word')) %>% 
  count(word, sentiment, sort = TRUE) 

word_sent_top <- word_sent2 %>% 
  group_by(sentiment) %>% 
  top_n(15, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) 
# Plotting most frequent words by sentiment using NRC lexicon
ggplot(word_sent_top, aes(x = word, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_grid(~sentiment, scales = 'free_x') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Additional sentiment analysis
data_sent3 <- tokens_df %>% 
  inner_join(nrc) %>% 
  count(sentiment, sort = TRUE) %>% 
  spread(sentiment, n)
scoreSentiment3 <- data.frame(t(data_sent3))
colnames(scoreSentiment3) <- c('value')
pl <- colnames(t(scoreSentiment3))
plot <- as.data.frame(cbind(pl, as.numeric(t(scoreSentiment3))))
colnames(plot) <- c('emotion', 'value')
plot$value <- as.numeric(plot$value)


# Plotting sentiment distribution
ggplot(data=plot, aes(x=emotion, y=value)) +
  geom_bar(stat="identity", fill=c('white','white','white','white','white','#ff7473','#28bfc9','white','white','white'), 
           colour="black") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank(),
        legend.position = "none")

##### AFINN #####
afinn<-get_sentiments("afinn")

corpusBi <- Corpus(VectorSource(data$title))

corpusBi <- tm_map(corpusBi, content_transformer(tolower)) # Convert to lowercase
corpusBi <- tm_map(corpusBi, removeNumbers) # Remove numbers
corpusBi <- tm_map(corpusBi, removeWords, c(stopwords('en'))) # Remove English stopwords
corpusBi <- tm_map(corpusBi, content_transformer(str_replace_all), '-', ' ') # Replace dashes with spaces
corpusBi <- tm_map(corpusBi, content_transformer(str_replace_all), '–', ' ') # Replace en-dashes with spaces
corpusBi <- tm_map(corpusBi, removePunctuation) # Remove punctuation
corpusBi <- tm_map(corpusBi, stripWhitespace) # Remove extra whitespaces

# Convert corpus to a data frame
dfBi <- data.frame(text = sapply(corpusBi, as.character), stringsAsFactors = FALSE)

# Extract bigrams
bigrams <- dfBi %>% 
  unnest_tokens(bigram, text, token = 'ngrams', n = 2)  %>% 
  separate(bigram, c('c1', 'c2'), sep = " " )

# Calculate bigram counts
bigram_counts <- bigrams %>% 
  count(c1, c2, sort = TRUE) 

# Filter bigrams with count > 200 for graph
bigram_graph <- bigram_counts %>% 
  filter(n > 100) %>% 
  graph_from_data_frame() 


# Define negation words
negation_words <- c('ceasefire', 'attack', 'hostage', 'conflict', 'genocide', 'aid') 

# Find words preceded by negation
negated_words <- bigrams %>% 
  filter(c1 %in% negation_words) %>% 
  inner_join(afinn, by = c('c2' = 'word')) %>% 
  count(c1, c2, value, sort = TRUE) %>% 
  ungroup

# Get top negative words
top_neg_word <- negated_words %>% 
  mutate(contribution = n * value) %>% 
  arrange(desc(abs(contribution))) %>% 
  group_by(c1) %>% 
  top_n(5, abs(contribution)) %>% 
  ungroup() %>% 
  mutate(c2 = reorder(c2, contribution)) 

# Plot frequencies of words preceded by negation
ggplot(top_neg_word, aes(c2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("") +
  ylab("Sentiment score * number of occurrences") +
  facet_wrap(~c1, ncol = 2, scales = 'free') + 
  coord_flip()




##### BIGRAMS #####

unigrams_df <- data.frame(Combined_Text = words) %>%
  unnest_tokens(word, Combined_Text) %>%
  filter(!is.numeric(word)) %>%
  anti_join(get_stopwords(source = "smart", language = "en"), by = c("word" = "word")) %>%
  filter(!word %in% c("israel","says","palestine","â","sa","2","icj","palestinians","hamas","israelhamas","israeli", "palestinian", "gaza","israel","war"))

bigrams_df <- unigrams_df %>%
  mutate(next_word = lead(word)) %>%
  filter(!is.na(next_word)) %>%
  unite(bigram, word, next_word, sep = " ") %>%
  count(bigram, sort = TRUE)

top_bigrams <- bigrams_df %>%
  head(40)

top_bigrams_separated <- top_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigram_graph <- graph_from_data_frame(top_bigrams_separated)

a <- grid::arrow(type = 'closed', length = unit(.1, 'inches'))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "red", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

###### LOUGHRAN #####
loughran_lexicon <- get_sentiments("loughran")

sentiment_df <- tokens_df %>%
  inner_join(loughran_lexicon, by = "word")

# Step 4: Visualize Sentiment Distribution
sentiment_counts <- sentiment_df %>%
  group_by(sentiment) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

ggplot(sentiment_counts, aes(x = reorder(sentiment, count), y = count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Sentiment", y = "Count", title = "Loughran Sentiment Distribution") +
  theme_minimal()

# Step 5: Top Positive and Negative Words
positive_words <- sentiment_df %>%
  filter(sentiment == "positive")

negative_words <- sentiment_df %>%
  filter(sentiment == "negative")

positive_word_counts <- positive_words %>%
  count(word, sort = TRUE)

negative_word_counts <- negative_words %>%
  count(word, sort = TRUE)

head(positive_word_counts, 10)
head(negative_word_counts, 10)

#Combine top 10 positive and negative word counts
top_words_combined <- rbind(head(positive_word_counts, 20), head(negative_word_counts, 20))

# Create a color vector based on sentiment
colors <- ifelse(top_words_combined$word %in% head(positive_word_counts, 10)$word, "green", "red")


# Create a dataframe with word and frequency
word_freq_df <- data.frame(word = top_words_combined$word, freq = top_words_combined$n)

# Convert the dataframe into a format suitable for wordcloud2
wordcloud2_data <- as.data.frame(word_freq_df)

# Create word cloud using wordcloud2
wordcloud2(wordcloud2_data, color = colors, size = 1.5,
           backgroundColor = "white",
           rotateRatio = 0.3)

##### CORRELATIONS  ####
corpus2 <- Corpus(VectorSource(data$title))
# 
corpus2 <- tm_map(corpus2, content_transformer(tolower)) #all lowercase
corpus2 <- tm_map(corpus2, removeNumbers) #remove numbers
corpus2 <- tm_map(corpus2, removeWords, c(stopwords('en'), c("sas","africaa","icj","keir","starmer","sa","israel","palestine","says","â","palestinians","hamas","israelhamas","israeli", "palestinian", "gaza","israel","war","plessis","dricus")))  # Remove English stopwords and "share"
corpus2 <- tm_map(corpus2, content_transformer(str_replace_all), '-', ' ') #replace dashes with spaces
corpus2 <- tm_map(corpus2, content_transformer(str_replace_all), '–', ' ')
corpus2 <- tm_map(corpus2, removePunctuation) #remove punctuation
corpus2 <- tm_map(corpus2, stripWhitespace) #remove white spaces
# 
df2 <- data.frame(text = sapply(corpus2, as.character), stringsAsFactors = FALSE)
# # 

df2$title <- rep(c(1:2494),each = 4)
t = df2 %>% unnest_tokens(word, text, drop = FALSE) %>% 
  select(-text) %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, title, sort = TRUE) 
# 
# 
# 
# 
# 
# # We can pick some interesting words and find other most associated with them:
word_cors_top6 <- t %>%
  filter(item1 %in% c('ceasefire', 'truce', 'hostage', 'conflict', 'genocide', 'aid')) %>% # we choose those words
  group_by(item1) %>% 
  top_n(10) %>% # we choose top 10 (frequent) words 
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) # new variable, that arranges second word acording to correlation
# 
# # graph With colors:
ggplot(word_cors_top6, aes(item2, correlation, fill = correlation)) + 
  geom_col() + 
  facet_wrap(~ item1, scales = "free") + # to organize plots
  coord_flip() # flips orientation

# 
# 
# # We can now visualize the correlations and clusters of words:
# set.seed(2016)
# 
# # Let's use correlation matrix for network analysis:
t %>% filter(correlation > .50) %>% # filters data 
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(show.legend = FALSE) +
  geom_node_point(color = "#277BC0", size = 4) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
# 
# # You can see that the output is rather symmetrical and there are no arrows - because
# # the relationship isn't directional. 
# # common (like in the bigram analysis earlier)



##### LDA MODEL NON FUNZIONA #####
library(tm)  # For text mining functions
library(topicmodels)  # For LDA modeling

# Tokenization and Preprocessing
corpus <- Corpus(VectorSource(data$title))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "says","sas","africaa","icj","keir","starmer","sa","plessis","dricus"))  # Exclude "share"

# Remove documents with no terms
corpus <- corpus[sapply(corpus, function(doc) length(doc) > 0)]

# Create Document-Term Matrix
dtm <- DocumentTermMatrix(corpus)

# Convert to matrix format
dtm_matrix <- as.matrix(dtm)

# Run LDA Model
num_topics <- 3  # Define the number of topics
lda_model <- LDA(dtm, k = num_topics, method= "Gibbs")

# Interpret Topics
topics <- as.data.frame(terms(lda_model, 10))  # Get top terms for each topic
colnames(topics) <- paste("Topic", 1:num_topics)
print(topics)
# Interpret Topics
top_terms <- terms(lda_model, 10)  # Get top terms for each topic
colnames(top_terms) <- paste("Topic", 1:num_topics)

# Create LDAvis JSON
lda_vis <- createJSON(phi = posterior(lda_model)$terms,
                      theta = posterior(lda_model)$topics,
                      doc.length = rowSums(as.matrix(dtm)),
                      vocab = colnames(as.matrix(dtm)),
                      term.frequency = colSums(as.matrix(dtm)))


# Serve LDAvis
serVis(lda_vis) # It will open a dashboard in your default web browser


