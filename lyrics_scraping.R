library(tidyverse)
library(httr)
library(rvest)
library(lubridate)
library(xml2)
library(xslt)
library(tm)
library(qdapRegex)
library(SnowballC)
library(RColorBrewer)
library(wordcloud2)

# Import all Song-URLs + Bandname, Title, Duration and Album ----------------------------------------------------

# FLL <- list() # Final list with all downloaded Bands  # Only OVERRIDE if bands content requires new download!!!

# Lists of chosen bands (web-scraping required the "A-Z" display of each band to correctly scape its content)

start_time <- Sys.time()

band_pages <- as.character(
                           c(
                             "https://www.lyrics.com/artist.php?name=Manowar&aid=13602&o=1",          # 1 MANOWAR     
                             "https://www.lyrics.com/artist.php?name=ABBA&aid=3492&o=1",              # 2 ABBA
                             "https://www.lyrics.com/artist.php?name=Iron-Maiden&aid=4560&o=1",       # 3 IRON MAIDEN
                             "https://www.lyrics.com/artist.php?name=AC%2FDC&aid=3496&o=1",           # 4 AC/DC
                             "https://www.lyrics.com/artist.php?name=Michael-Jackson&aid=4576&o=1",   # 5 MICHAEL JACKSON
                             "https://www.lyrics.com/artist.php?name=The-Beatles&aid=3644&o=1",       # 6 Beatles
                             "https://www.lyrics.com/artist.php?name=The-Rolling-Stones&aid=5298&o=1",# 7 Rolling Stones
                             "https://www.lyrics.com/artist.php?name=Ramones&aid=5223&o=1",           # 8 Ramones
                             "https://www.lyrics.com/artist.php?name=Eminem&aid=347307&o=1",          # 9 Eminem
                             "https://www.lyrics.com/artist.php?name=Elvis-Presley&aid=5175&o=1",     # 10 Elvis
                             "https://www.lyrics.com/artist.php?name=Bob-Dylan&aid=4147&o=1",         # 11 Bob
                             "https://www.lyrics.com/artist.php?name=Queen&aid=5205&o=1",             # 12 Queen
                             "https://www.lyrics.com/artist.php?name=Nirvana&aid=5034&o=1",           # 13 Nirvana
                             "https://www.lyrics.com/artist.php?name=B.B.-King&aid=93923&o=1",        # 14 BB King
                             "https://www.lyrics.com/artist.php?name=Lady-Gaga&aid=1055684&o=1",       # 15 Lady Gaga
                             "https://www.lyrics.com/artist.php?name=Neil-Young&aid=5896&o=1",         # 16 Neil Young
                             "https://www.lyrics.com/artist.php?name=Depeche-Mode&aid=4071&o=1"       #17 Depeche Mode
                             )
                          ) 

# unwanted punctuations in LYRICS (keeps the apostrophe >> ' << and asterisk >> * << ). Used later to 
unwanted_punct <- "[\\.|,|\\?|\\!|\"|\\[|\\]|\\(|\\)|#|$]" 

#### BIG LOOP STARTS HERE ##############################################################################################
for (p in 1:length(band_pages)) {

start_loop <- Sys.time()
  
page <- read_html(band_pages[p]) # scrape page
Band <- html_node(page, "h1") %>% html_text() # extract band name

songs_table <- html_table(page, header = TRUE, trim = F, fill = TRUE) # extract songs table (raw version)
songs_table <- as_tibble(songs_table[[1]]) # convert to tibble 

songs_table2 <- songs_table %>%
                mutate(Song = str_to_lower(Song),
                       Song = str_squish(Song),
                       Album = str_to_lower(Album)) %>%
                filter( !(str_detect(Song, "live|remix|version|dvd|radio edit|edition|remastered|medley|take [:digit:]|intro|outro|demo|\\[") | # remove live, remix, dvd songs and albums
                          str_detect(Album, "live|best of|essential|essentials|unknown album|edition|greatest|hits|dvd|definitive|remastered|anthology|box set|vol. [:digit:]|collection|\\["))                    # remove live, best of, etc. albums
                      ) %>%
                mutate(Band = Band,                                                         # define Band name (extracted)
                       Year = as.integer(str_extract(Album, pattern = "[:digit:]{4}$")),    # extract Year
                       Album = str_remove(Album, pattern = "[:digit:]{4}$"),                                           # KEY COLUMN: lower case letters
                       Song = str_remove_all(Song, pattern = "[:punct:]"),                  # KEY COLUMN: remove punctuation in Song name
                       Song = str_squish(Song),                                             # KEY COLUMN: remove excess whitespaces
                       Duration = ms(Duration, quiet = TRUE)) %>%                           # convert Duration
                
                arrange(Duration, Song, Year) %>%                         # arrange by Song and Year
                group_by(Song) %>%                                        # group by Song
                filter(row_number(Year) == 1) %>% # this filters out the earliest song version. Since Live versions 
                                                                          # are removed above, this filters earliest song version.
                                                                          # Does not work perfectly if a) same song exists twice on an    
                                                                          # album (e.g., some special edition) or b) song is spelled differently
                                                                          # On some other album. 
                                                                          # Further duplicates are removed below by comparing lyrics 
                ungroup %>% 
                arrange(Year) %>%
                select(Band, Album, Song, Duration, Year)

# Extracting song name/url simultaneously ----------------------

Song <- html_nodes(page, "a") %>% html_text() %>% as.character()      #extracting nodes containing song names
link_tail <- html_attr(html_nodes(page, "a"), "href")  %>% as.character()   #extracting attribubtes of 'a' (url)
song_links <- tibble(Song, link_tail) %>%                              #create tibble
                   filter(str_detect(link_tail, "/lyric/"),                 #filter for "lyrics" urls. Removes unnecessary url
                          !is.na(link_tail)) %>%               
                   mutate(Song = str_to_lower(Song),                          # KEY COLUMN: lower case (as done above equivalently)
                          Song = str_remove_all(Song, pattern = "[:punct:]"), # KEY COLUMN: remove punctuation (as done above equivalently)
                          Song = str_squish(Song)) %>%                        # KEY COLUMN: remove excess whitespace (as done above equivalently)
                   group_by(Song) %>% 
                   filter(row_number(Song) == 1)                      # filter for the first url of every song 
                                                                      # (should be all the same, regardless of version)
# JOINING songs_table2 with songs_links_auto -------------------------------------------

joined_table <- left_join(songs_table2, song_links, by = "Song") %>% # join both tables (table containing Band, Album, Duration, Year
                                                                          # WITH table containg urls of lyrics)
                          mutate(full_url = paste0("https://www.lyrics.com", link_tail),                            
                                 Song = str_to_title(Song)) %>%           
                          select(-link_tail)                              # remove single url segments 

# DOWNLOADING lyrics for the full discography and add them to the previous table -----------------------------

raw_lyrics <- character() # create character vector (being used for the downloaded lyrics)
total_number_of_songs <- length(joined_table$full_url) # total number of songs (for tracker)

                 ########## TAKES A WHILE !!!!! ##### loop for downloading every song lyric (using the created link above) ##

for (i in 1:length(joined_table$full_url)) {
  
  lyrics_page <- read_html(joined_table$full_url[i])
  
  raw_lyrics[i] <- html_text(html_node(lyrics_page, "pre"), trim = T) %>% 
                   str_replace_all("\n", " ") %>%         # replaces html tags such as \n 
                   str_remove_all(unwanted_punct) %>%     # remove punctuations
                   str_squish() %>%                       # remove excess white space 
                   str_to_lower()                         # to lower case character
                   
  print(paste0(Band, " songs downloaded: ", i, " of ", total_number_of_songs)) # loop tracker
  
  }

                 ########## TAKES A WHILE !!!!! #############################################################################


joined_table2 <- joined_table %>% mutate(Lyrics = raw_lyrics) %>%                                      # add raw_lyrics as column to joined_table 
                                  select(-full_url) %>%                                                # remove url from tibble
                                  arrange(Song, Duration, Year)                                        # arrange by ... 

duplicates <- which(duplicated(joined_table2$Lyrics)) # detect duplicates
band_table <- joined_table2[-duplicates, ]            # remove dublicated lyrics. Problems with other version having some kind
                                                      # of intro deviating from the original one (e.g. Michael Jackson)

FLL[[p]] <- band_table                                ## move band_table df to FLL list (according to iterative indice)
names(FLL)[p] <- Band

end_loop <- Sys.time()
print(paste0("------------------ [ ", Band, " done after ", ms(round((end_loop - start_loop), 2)), " ]------------------"))

}

# calculating FULL execution time
end_time <- Sys.time()
print(paste0("__________________________[ All Lyrics downloaded in... ", ms(round((end_time - start_time), 2)), " ]__________________________")) 

#### BIG LOOP ENDS HERE ##############################################################################################

##### Converting lyrics into Corpus´ #####---------------------------------------------------------------

# list of stop words that are frequently used in English language, but do not carry the thematic component.
# credit to countwordsfree.com/stopwords. Words added: "gonna", "gimme", "em", "wanna", "gotta"
extended_english_stopwords <- read_csv(file = "extended_stopwords.csv", trim_ws = TRUE) %>% pull()  


clouds <- list()

for (w in 1:length(FLL)) {

      lyrics_flattened <- str_flatten(FLL[[w]]$Lyrics)                           # merge all song lyrics into one character entry
      lyrics_corpus <- VCorpus(VectorSource(lyrics_flattened))                   # create a corpus (required for further functions)
      lyrics_cleaned <- tm_map(lyrics_corpus, removeWords, extended_english_stopwords) #remove stopwords
      #lyrics_stemmed <- tm_map(lyrics_cleaned, stemDocument, language ="english") # doesn't work properly (cuts off last letters and converts, for some words, plurals into singular incorrectly)
      
      word_matrix <- as.matrix(TermDocumentMatrix(lyrics_cleaned))               # create matrix with frequencies of every word
      word_matrix_sorted <- sort(rowSums(word_matrix), decreasing = T)           # sort words by their frequencies
      word_matrix_df <- data.frame(row.names = 1:length(word_matrix_sorted),     # create data frame of words and their frequencies
                                   word = names(word_matrix_sorted), 
                                   freq = word_matrix_sorted)
                        
      
      #word_matrix_df$word <- str_replace_all(word_matrix_df$word, "babi", "baby")  #replace "babi" with "baby", stemming does not work here correctly
      
      clouds[[w]] <- word_matrix_df                                                 # save each word_matrix_df in list "clouds"
      names(clouds)[w] <- names(FLL)[w]
      print(paste0(names(clouds)[w], " done ... "))
      }
