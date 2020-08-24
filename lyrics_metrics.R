glimpse(clouds)

song_bucket <- character()
song_total <- integer()
song_type <- integer()
song_TTR <- double()
avg_song_TTR <- double()
avg_song_total <- double()

word_total <- integer()
word_type <- integer()
TTR <- double()
song_count <- integer()
mean_duration <- integer()
sd_duration <- integer()
h <- integer()
t <- integer()

for (t in 1:length(FLL)) {    # band iteration
  
  song_TTR <- NULL
  song_total <- NULL 
  
  song_count[t] <- nrow(FLL[[t]])
  
  for (h in 1:song_count[t]) {  # song itereation
  song_bucket <- str_split(string = FLL[[t]]$Lyrics[h], pattern = " ", simplify = TRUE)
  song_total[h] <- length(song_bucket[1, ])
  song_type <- length(unique(song_bucket[1,]))
  song_TTR[h] <- song_type / song_total[h]
  
  }
  
  avg_song_TTR[t] <- mean(song_TTR)
  avg_song_total[t] <- mean(song_total)
    
  word_bucket <- str_flatten(FLL[[t]]$Lyrics) %>% str_split(" ", simplify = TRUE)
  word_total[t] <- length(word_bucket)
  word_type[t] <- length(unique(word_bucket[1,]))
  TTR[t] <- round(word_type[t] / word_total[t], 4)
  
  
  
  mean_duration[t] <- mean(period_to_seconds(FLL[[t]]$Duration), na.rm = TRUE)
  sd_duration[t] <- sd(period_to_seconds(FLL[[t]]$Duration), na.rm = TRUE)
  
  }

metrics <- tibble(names(clouds), word_total, word_type, TTR, song_count, mean_duration, sd_duration, avg_song_TTR, avg_song_total) %>% 
           mutate(mean_duration = round(seconds_to_period(mean_duration)),
                  sd_duration = round(seconds_to_period(sd_duration))) %>%
           arrange(desc(avg_song_TTR))

metrics %>% ggplot(aes(avg_song_TTR, avg_song_total)) + geom_point()
metrics %>% ggplot(aes(TTR, word_total)) + geom_point()

#### Counting ALL used words #### 
FLL


word_test <- clouds[["Manowar"]] %>% top_n(10)

word_test %>% mutate(mean = mean(word_test$freq),
                     deviation_to_mean = abs(freq - mean))

