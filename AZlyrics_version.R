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

# Import all Song-URLs + Bandname, Title, Duration and Album FROM AZLYRICS.com ----------------------------------------------------

# FLL <- list() # Final list with all downloaded Bands  # Only OVERRIDE if bands content requires new download!!!

# Lists of chosen bands (web-scraping required the "A-Z" display of each band to correctly scape its content)

start_time <- Sys.time()

band_pages <- as.character(
  c(
    "https://www.azlyrics.com/m/manowar.html",                               # 1 MANOWAR     
    "https://www.lyrics.com/artist.php?name=ABBA&aid=3492&o=1",              # 2 ABBA
    "https://www.lyrics.com/artist.php?name=Iron-Maiden&aid=4560&o=1",       # 3 IRON MAIDEN
    "https://www.lyrics.com/artist.php?name=AC%2FDC&aid=3496&o=1",           # 4 AC/DC
    "https://www.lyrics.com/artist.php?name=Michael-Jackson&aid=4576&o=1",   # 5 MICHAEL JACKSON
    "https://www.azlyrics.com/b/beatles.html",                               # 6 Beatles
    "https://www.lyrics.com/artist.php?name=The-Rolling-Stones&aid=5298&o=1",# 7 Rolling Stones
    "https://www.lyrics.com/artist.php?name=Ramones&aid=5223&o=1",           # 8 Ramones
    "https://www.lyrics.com/artist.php?name=Eminem&aid=347307&o=1",          # 9 Eminem
    "https://www.azlyrics.com/e/elvis.html",                                 # 10 Elvis
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
for (p in 10:10) {
  
  start_loop <- Sys.time()
  
  page <- read_html(band_pages[p]) # scrape page
  Band <- html_node(page, "title") %>% html_text() %>% str_remove(" Lyrics")# extract band name
  
  links <- html_nodes(page, "a") # extracting ALL LINKs of the band page
  link_name <- html_text(links) # extracting link names
  url <- html_attr(links, "href")  %>% as.character() # extracting urls
  
  link_table <- tibble(link_name, url) # combine names and urls into a tibble
  
  filter_indices <- which(str_detect(link_table$link_name, "sort by song") | 
                          str_detect(link_table$link_name, "Submit Lyrics")) # filter non-song links
  
  link_table2 <- link_table[filter_indices[1]: filter_indices[2], ]                      # remove non-song links by using filter_indices
  song_table_raw <- link_table2[-c(1, nrow(link_table2)), ] %>%                          # removing first and last entry
                    mutate(url = str_replace(url, "..", "https://www.azlyrics.com"),
                           link_name = str_to_lower(link_name))             %>% # complete url (first part was missing) 
                    filter( !str_detect(link_name, "\\*|dmca notice|version|live|remix|dvd|radio edit|remastered|medley|intro|outro")) %>%               # songs marked with * do not have lyrics and link to google)
                    group_by(link_name) %>%                                              # groups duplicates together...
                    arrange(link_name) %>%                                               # ...arrange by this name...
                    filter(row_number(link_name) == 1) %>%                               # ... and keep only the first entry (ensures having oldest version)
                    ungroup() %>%
                    rename(song = link_name)
  
  table(duplicated(song_table_raw$url))
  
  
  # DOWNLOADING lyrics for the full discography 
  
  raw_lyrics <- character() # create character vector (being used for the downloaded lyrics)
  
  for (i in 1:nrow(song_table_raw)) {
    
    lyrics_page <- read_html(song_table_raw$url[i])
    
    album_info <- html_node(lyrics_page, "div.songinalbum_title") %>% html_text()
    
    divs <- tibble(html_text(html_nodes(lyrics_page, "div"))) %>% 
                   rename(entry = `html_text(html_nodes(lyrics_page, "div"))`) %>% 
                   mutate(entry = str_squish(entry),
                          count = str_count(entry)) %>% arrange(desc(count))
    
     ## appears that div entry 4 is the right one, but why????
    raw_lyrics[i] <- divs[4,1] %>% pull()
    
    print(paste0(Band, " songs downloaded: ", i, " of ", nrow(song_table_raw))) 
    
  }
  song_table_raw %>% mutate(lyrics = raw_lyrics)
  # until hERE recoded 
  


  
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
  
  FLL[[p]] <- band_table                                ## move band_table df to FLL list (according to iterative indices)
  names(FLL)[p] <- Band
  
  end_loop <- Sys.time()
  print(paste0("------------------ [ ", Band, " done after ", ms(round((end_loop - start_loop), 2)), " ]------------------"))
  
}
