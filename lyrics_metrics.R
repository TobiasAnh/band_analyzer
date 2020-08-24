glimpse(clouds)

word_total <- integer()
word_type <- integer()
TTR_10k <- double()
song_count <- integer()
median_duration <- integer()
sd_duration <- integer()
all_durations <- list()

for (t in 1:length(FLL)) {    # band iteration
  
  song_count[t] <- nrow(FLL[[t]])
  
  word_bucket <- str_flatten(FLL[[t]]$Lyrics) %>% str_split(" ", simplify = TRUE)
  word_type[t] <- length(unique(word_bucket[1,1:10000]))   #normalized on the first 10000 words (TTR is affected by tokens [total word count])
  TTR_10k[t] <- round(word_type[t] / 10000, 4)

  all_durations[[names(FLL[t])]] <- FLL[[t]]$Duration 
  median_duration[t] <- median(period_to_seconds(FLL[[t]]$Duration), na.rm = TRUE)  # median song duration
  sd_duration[t] <- sd(period_to_seconds(FLL[[t]]$Duration), na.rm = TRUE)          # standard deviation of song duration
  
  }

metrics <- tibble(names(clouds), song_count, median_duration, sd_duration, word_type, TTR_10k) %>% 
           mutate(median_duration = round(seconds_to_period(median_duration)),
                  sd_duration = round(seconds_to_period(sd_duration))) %>%
           arrange(desc(TTR_10k))

metrics %>% ggplot(aes(TTR_10k, song_count)) + geom_point()


