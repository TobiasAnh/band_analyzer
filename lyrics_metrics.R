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
library(shiny)
library(ggridges)
library(ggpubr)

#### Estimating band metrics including median song duration, song count and ...
#### ... type/token ratio (standardized for the first 10k words, otherwise it's biased!)

# define variables used in loop 
word_type <- integer()       # number of unique words
TTR_10k <- double()          # type / token ratio (standardized for the first 10k words)
song_count <- integer()      # number of songs 
median_duration <- integer() # median song duration of each song 
sd_duration <- integer()     # standard deviation of song duration

for (t in 1:length(FLL)) {    # band iteration
  
  song_count[t] <- nrow(FLL[[t]])
  
  word_bucket <- str_flatten(FLL[[t]]$Lyrics) %>% str_split(" ", simplify = TRUE)
  word_type[t] <- length(unique(word_bucket[1,1:10000]))
  TTR_10k[t] <- round(word_type[t] / 10000, 4)
  
  median_duration[t] <- median(period_to_seconds(FLL[[t]]$Duration), na.rm = TRUE)  # median song duration
  sd_duration[t] <- sd(period_to_seconds(FLL[[t]]$Duration), na.rm = TRUE)          # standard deviation of song duration
  
}

# Metrics overiew tibble
metrics <- tibble(Band = names(clouds), song_count, median_duration, sd_duration, word_type, TTR_10k) %>% 
           mutate(median_duration = round(seconds_to_period(median_duration)),
                  sd_duration = round(seconds_to_period(sd_duration))) %>%
           arrange(desc(TTR_10k))


# Plot song counts #############
plot_song_count <- metrics %>% ggplot(aes(fct_reorder(Band, song_count), song_count, label = song_count)) + 
                                geom_col(fill = "darkgreen", alpha = 0.5, width = 0.7) +
                                geom_text(nudge_y = -32, nudge_x = 0.05) + 
                                labs(title = "Total number of songs",
                                     y = element_blank(),
                                     x = element_blank()) +
                                coord_flip() + 
                                theme_classic() +
                                theme(panel.grid = element_blank(),
                                      axis.line.y = element_blank(),
                                      axis.line.x = element_blank(),
                                      axis.ticks.y = element_blank(),
                                      axis.ticks.x = element_blank(),
                                      axis.text.y = element_text(size = 12),
                                      axis.text.x = element_blank()) 





##############

# TTR_10k Plot 

plot_ttr <- metrics %>% ggplot(aes(fct_reorder(Band,TTR_10k), TTR_10k, label = round(TTR_10k, 2))) + 
                        geom_col(fill = "steelblue", alpha = 0.5, width = 0.5) +
                        geom_text(nudge_y = 0.011) + 
                        labs(title = "Type/Token ratio (TTR) of each band",
                             subtitle = "Number of unique words within the total amount of all words",
                             y = "TTR (standardized to the first 10,000 tokens)",
                             x = element_blank()) +
                        coord_flip() + 
                        theme_classic() +
                        theme(panel.grid = element_blank(),
                              axis.line.y = element_blank(),
                              axis.line.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              axis.ticks.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.text.x = element_blank()) + 
                        geom_label(aes(fct_reorder(Band,TTR_10k), TTR_10k, label = Band), alpha = 0.95, nudge_y = -0.035)


#### Creating data.frame with song durations 

all_durations <- list()   # create empty list() for all song durations of every band

for (u in 1:length(FLL)) {

      durations_band <- period_to_seconds(FLL[[u]]$Duration)[!is.na(FLL[[u]]$Duration)]  # convert periods -> seconds, exclude NAs
      length(durations_band) <- max(song_count)                                          # extend length to maximum song count (to assure consistend data frame)
      all_durations[[u]] <- as.integer(durations_band)                                   # append song durations to 'all_durations' list
      
      } 
names(all_durations) <- names(FLL)                                                 # name 'all durations' list accordingly
all_durations_tbl <- as_tibble(all_durations)                                      # convert list to tibble

arranged_bands <- metrics %>% arrange(desc(median_duration))                       # arrange bands by median_duration
durations_arranged <- all_durations_tbl %>%                                 
                      gather(Band, Duration) %>%                            # gather tibble for density ridge plot
                      mutate(Band = fct_relevel(Band, arranged_bands$Band)) # factorise and relevel "Band" according to median duration
  
  
plot_duration <- ggplot(durations_arranged, aes(Duration, Band)) +                           # plot song durations vs. Band
                         geom_density_ridges(na.rm = TRUE, alpha = 0.4, fill = "red") +      # density ridges with alpha
                         theme_classic() +
                         labs(title = "Distribution of song durations of each band",
                              subtitle = "displayed as ridged density plots in seconds",
                              y = element_blank(),
                              x = "Durations in seconds") +
                         theme(panel.grid = element_blank(),
                               axis.line.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.text.y = element_text(vjust = -0.2, size = 12)) +
                         scale_x_continuous(limits = c(0,1200), breaks = seq(from = 0, to = 60*20, by = 120))


plot_list <- list(plot_song_count, plot_ttr, plot_duration)
names(plot_list) <- c("count", "ttr", "duration")
