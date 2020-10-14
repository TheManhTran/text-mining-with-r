library(gutenbergr) # For downloading books from Project Gutenberg 
library(tidyverse) # For ggplot, dplyr, etc.
library(tidytext) # For generating insights from the literature, news and social media

# 174 Get The_Picture_of_Dorian_Gray
The_Picture_of_Dorian_Gray_original <- gutenberg_download(174, meta_fields = "title")

# Clean up
The_Picture_of_Dorian_Gray <- The_Picture_of_Dorian_Gray_original %>% 
  # The actual book doesn't start until line 58
  slice(58:n()) %>% 
  # Get rid of rows where text is missing
  drop_na(text) %>% 
  # Chapters start with CHAPTER X, so mark if each row is a chapter start
  # cumsum() calculates the cumulative sum, so it'll increase every time 
  # there's a new chapter and automatically make chapter numbers
  mutate(chapter = str_detect(text, "^CHAPTER"),
         chapter_number = cumsum(chapter)) %>% 
  # Get rid of these columns
  select(-gutenberg_id, -title, -chapter)

word_frequencies <- The_Picture_of_Dorian_Gray %>%
  # The unnest_tokens() functions from tidytext counts words 
  # or bigram or paragraph to be in its own row
  unnest_tokens(word, text) %>% 
  # Remove stop words
  anti_join(stop_words) %>% 
  # use str_extract() here because the UTF-8 encoded texts 
  # from Project Gutenberg have some examples of words with 
  # underscores around them to indicate emphasis (like italics, 
  # ex: count "_any_" separately from "any" not good for counting word).
  mutate(word = str_extract(word, "[a-z']+")) %>% 
  # Count all the words
  count(word, sort = TRUE)

word_frequencies %>% 
  # Keep top 15
  top_n(15) %>%
  # Make the words an ordered factor so they plot in order
  mutate(word = fct_inorder(word)) %>% 
  ggplot(aes(x = n, y = word))+
  geom_col()

# Bigrams
The_Picture_of_Dorian_Gray_bigrams <- The_Picture_of_Dorian_Gray %>% 
  # n = 2 here means bigrams, trigrams (n = 3) or any type of n-gram
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  # Split the bigrams into two words so we can remove stopwords
  separate(bigram, c("w1","w2"), sep = " ") %>% 
  filter(!w1 %in% stop_words$word,
         !w2 %in% stop_words$word) %>% 
  # Put the two word columns back together
  unite(bigram, w1, w2, sep = " ")

bigram_frequencies <- The_Picture_of_Dorian_Gray_bigrams %>% 
  # Count all the bigrams
  count(bigram, sort = TRUE)
 
bigram_frequencies %>% 
  top_n(15) %>%
  mutate(bigram = fct_inorder(bigram)) %>% 
  ggplot(aes(x = n, y = bigram))+
  geom_col()

# Term frequency-inverse document frequency tf-idf
# formula
$$
\begin{aligned}
tf(\text{term}) &= \frac{n_{\text{term}}}{n_{\text{terms in document}}} \\
idf(\text{term}) &= \ln{\left(\frac{n_{\text{documents}}}{n_{\text{documents containing term}}}\right)} \\
tf\text{-}idf(\text{term}) &= tf(\text{term}) \times idf(\text{term})
\end{aligned}
$$
# Here, to do this analysis, add some books of Oscar Wilde
  books <- gutenberg_download(c(174,844,902,885), meta_fields = "title")
# The initial text sometimes isn't the actual text of the book, 
# removing those rows
  # 174: The Picture of Dorian Gray
  b1 <- gutenberg_download(174, meta_fields = "title")
  b1_clean <- b1 %>% 
    slice(58:n()) %>% 
    drop_na(text)
  # 844: The Importance of Being Earnest: A Trivial Comedy for Serious People
  b2 <- gutenberg_download(844, meta_fields = "title")
  b2_clean <- b2 %>% 
    slice(5:n()) %>% 
    drop_na(text)
  # 902: The Happy Prince, and Other Tales
  b3 <- gutenberg_download(902, meta_fields = "title")
  b3_clean <- b3 %>% 
    slice(63:n()) %>% 
    drop_na(text)
  # 885: An Ideal Husband
  b4 <- gutenberg_download(885, meta_fields = "title")
  b4_clean <- b4 %>% 
    slice(29:n()) %>% 
    drop_na(text)
# Use bind_rows() from dplyr to bind multiple data by row
  Oscar_Wilde_books <- bind_rows(b1_clean, b2_clean, b3_clean, b4_clean)
  
  book_words <- Oscar_Wilde_books %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>% 
    # use str_extract() here because the UTF-8 encoded texts 
    # from Project Gutenberg have some examples of words with 
    # underscores around them to indicate emphasis (like italics, 
    # ex: count "_any_" separately from "any" not good for counting word).
    mutate(word = str_extract(word, "[a-z']+")) %>% 
    count(title, word, sort = TRUE)
# find the words most distinctive to each document
book_words_tf_idf <-  book_words %>% 
  bind_tf_idf(word, title, n)
# Get the top 10 uniquest words
book_words_10 <- book_words_tf_idf %>% 
  arrange(desc(tf_idf)) %>% 
  group_by(title) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = fct_inorder(word))
# Plot
ggplot(book_words_10, 
       aes(y = fct_rev(word), x = tf_idf, fill = title)) +
  geom_col() +
  guides(fill = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~ title, scales = "free") +
  theme_bw()
# Sentiment analysis(sa)
# To see what the different dictionaries look like using get_sentiments()
# get_sentiments("afinn")  # Scoring system
# get_sentiments("bing")  # Negative/positive
# get_sentiments("nrc")  # Specific emotions
# get_sentiments("loughran")  # Designed for financial statements; positive/negative

The_Picture_of_Dorian_Gray_new <- The_Picture_of_Dorian_Gray %>% 
  # Split into word tokens
  unnest_tokens(word, text)

The_Picture_of_Dorian_Gray_sa <- The_Picture_of_Dorian_Gray_new %>% 
  # Join the sentiment dictionary
  inner_join(get_sentiments("bing"))

sentiment_analysis_chapter <- The_Picture_of_Dorian_Gray_sa %>% 
  # Get a count of postiive and negative words in each chapter 
  count(chapter_number, sentiment) %>% 
  # Convert the sentiment column into two columns named "positive" and "negative"
  pivot_wider(names_from = sentiment, values_from = n) %>% 
  # Calculate net sentiment
  mutate(net_sentiment = positive - negative)
  # Plot it
  sentiment_analysis_chapter %>% 
    ggplot(aes(x = chapter_number, y = net_sentiment)) +
    geom_line()
# More usefully, by splitting the data into groups of lines, 
# to show a more granular view of the progression of the plot
sentiment_analysis_range <- The_Picture_of_Dorian_Gray_sa %>% 
  mutate(line_number = row_number()) %>% 
  # Divide lines into groups of 100
  mutate(index = line_number %/% 100) %>% 
  # Get a count of postiive and negative words in each 100-line chunk
  count(index, sentiment) %>% 
  # Convert the sentiment column into two columns named "positive" and "negative"
  pivot_wider(names_from = sentiment, values_from = n) %>% 
  # Calculate net sentiment
  mutate(net_sentiment = positive - negative)
  # Plot it
  sentiment_analysis_range %>% 
    ggplot(aes(x = index, y = net_sentiment)) +
    geom_col(aes(fill = net_sentiment > 0))
