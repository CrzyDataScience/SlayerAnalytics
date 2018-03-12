library('rvest')
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytext)


# Find Slayer album sales
url <- 'https://en.wikipedia.org/wiki/Slayer_discography'

page_content <- read_html(url)

rank_table <- html_table(html_nodes(page_content, ".wikitable")[[1]], fill = TRUE)

# Remove the first and last rows
rank_table <- rank_table[-1,]
rank_table <- rank_table[-nrow(rank_table),]

# Remove the chart and certification features, charts are for posers!
rank_table <- rank_table[ -c(3:13,15)]

f <- function(x, output) {
  sales_extracted <- x[3]
  sales_extracted <- gsub(",","",sales_extracted)
  sum(as.numeric(str_extract_all(sales_extracted, "\\(?[0-9,.]+\\)?")[[1]]))
  
}

Total_Sales <- apply(rank_table,1,f)
rank_table <- cbind(rank_table, Total_Sales)

rank_table <- rank_table[ -c(3)]

# Finally, let's make the album details column a bit more useful
# by spreading the data into additional columns using TidyR's separate
rank_table <- separate(rank_table, "Album details", into = c("Album_title", "Release_date", "Label"), sep = "\n")

# Let's look at the cleaned table
rank_table

# Even better, plot it
p <- ggplot(data=rank_table, aes(x=Album_title, y=Total_Sales)) +
      geom_bar(stat="identity", fill="steelblue") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

# So Reign in Blood is the best sold Slayer album, like, ever!
# What songs are on that album?
reign_url <- 'https://en.wikipedia.org/wiki/Reign_in_Blood'

song_table <- reign_url %>%
  html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[3]') %>%
  html_table()
song_table <- song_table[[1]]

song_table

# import lyrics and place them into a data file, let's start with Angel of Death
angel <- readLines("songs/angel_of_death.txt")
angel_df <- data_frame(text = angel)
songname <- "Angel of Death"
reign_in_blood <- cbind(angel_df, songname)

piece <- readLines("songs/piece_by_piece.txt")
piece_df <- data_frame(text = piece)
songname <- "Piece by Piece"
reign_in_blood <- rbind(reign_in_blood, cbind(piece_df, songname))

necrophobic <- readLines("songs/necrophobic.txt")
necrophobic_df <- data_frame(text = necrophobic)
songname <- "Necrophobic"
reign_in_blood <- rbind(reign_in_blood, cbind(necrophobic_df, songname))

altar <- readLines("songs/altar_of_sacrifice.txt")
altar_df <- data_frame(text = altar)
songname <- "Altar of Sacrifice"
reign_in_blood <- rbind(reign_in_blood, cbind(altar_df, songname))

jezus <- readLines("songs/jezus_saves.txt")
jezus_df <- data_frame(text = jezus)
songname <- "Jezus Saves"
reign_in_blood <- rbind(reign_in_blood, cbind(jezus_df, songname))

criminal <- readLines("songs/criminally_insane.txt")
criminal_df <- data_frame(text = criminal)
songname <- "Criminally Insane"
reign_in_blood <- rbind(reign_in_blood, cbind(criminal_df, songname))

reborn <- readLines("songs/reborn.txt")
reborn_df <- data_frame(text = reborn)
songname <- "Reborn"
reign_in_blood <- rbind(reign_in_blood, cbind(reborn_df, songname))

epidemic <- readLines("songs/epidemic.txt")
epidemic_df <- data_frame(text = epidemic)
songname <- "Epidemic"
reign_in_blood <- rbind(reign_in_blood, cbind(epidemic_df, songname))

postmortem <- readLines("songs/postmortem.txt")
postmortem_df <- data_frame(text = postmortem)
songname <- "Postmortem"
reign_in_blood <- rbind(reign_in_blood, cbind(postmortem_df, songname))

raining <- readLines("songs/raining_blood.txt")
raining_df <- data_frame(text = raining)
songname <- "Raining Blood"
reign_in_blood <- rbind(reign_in_blood, cbind(raining_df, songname))

# Save all the lyrics in an Rdata file
# save(reign_in_blood, file = "reign_in_blood.RData")

# Load the Rdata file
load("reign_in_blood.RData")

# unnest tokens
reign_in_blood <- reign_in_blood %>%
                    unnest_tokens(word, text)

# remove stop words
reign_in_blood <- reign_in_blood %>%
            anti_join(stop_words,  by = c("word" = "word"))

# show some word counts
reign_in_blood %>% filter(songname == "Raining Blood") %>% count(word, sort = TRUE) 

# or better, show it in a graph, filter out words that occure more than two times
reign_in_blood_counts <- reign_in_blood %>%
  count(word, songname, sort = TRUE) %>%
  ungroup()

reign_in_blood_counts %>%
filter(n > 2) %>%
ggplot(aes(x=word, y=n, fill=songname)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Is there a positive sentiment or a negative one based on the words used on the album
reign_sentiment_words <- reign_in_blood %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

reign_sentiment_words %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# How about the sentiment range of each song?
reign_sentiment <- reign_in_blood %>%
  group_by(songname) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(reign_sentiment, aes(songname, sentiment, fill = songname)) +
  geom_col(show.legend = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Can we do a suggestion for a sentence for the next Slayer hit perhaps?
# We will use a Long Short Term Memory (LSTM) Recurrent Neural Network 
# We have to build it in python though
