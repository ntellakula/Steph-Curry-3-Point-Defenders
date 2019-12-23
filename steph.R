library(readr)
library(tidyverse)

#Importing the data
source <- read_csv("C:/Users/NTellaku/Documents/R/Steph Curry Infographic/leagueseasonmatchups.csv")
labels <- source %>% 
  select(resultSets__headers) %>% 
  slice(1:34)

#Because this is a one-column csv from json,
#these lines are to wrangle the actual dataset
source_info <- source %>% select(resultSets__rowSet)
source_split <- as_tibble(matrix(, nrow = 34, ncol = 350))

i <- 1
value <- i + 33
j <- 1
while (value <= 11900) {
  source_split[, j] <- source_info %>% 
    slice(i:value)
  i <- i + 34
  value <- i + 33
  j <- j + 1
}


analysis <- as_tibble(t(source_split))
colnames(analysis) <- t(labels)
analysis2 <- analysis %>% 
  mutate(name_poss = paste(analysis$DEF_PLAYER_NAME, " (", analysis$POSS, ")",
                           sep = "")) %>% 
  slice(1:26) %>% 
  select(name_poss, POSS, FG3M, FG3A, FG3_PCT) %>% 
  transform(POSS = as.numeric(POSS), FG3M = as.numeric(FG3M),
            FG3A = as.numeric(FG3A), FG3_PCT = as.numeric(FG3_PCT))
#Analysis dataset complete

#making the graphic
analysis2 %>%
  ggplot(aes(x = reorder(name_poss, +FG3_PCT),
             y = FG3_PCT,
             size = FG3A,
             fill = FG3_PCT)) +
  geom_point(alpha = 0.75,
             shape = 21,
             color = "black") +
  coord_flip() +
  labs(size = "3-Point Field\nGoals Attempted",
       title = "Steph Curry's 3-Point Field Goal Percentage when Guarded by:",
       subtitle = "Among players that guarded Steph at least 40 possessions in 2018-2019 Regular Season",
       y = "3-Point Field Goal Percentage",
       x = "Player Name (Possessions)") +
  scale_y_continuous(limits = c(0.1, 0.9),
                     breaks = seq(0.1, 0.9, 0.1)) +
  scale_size_continuous(range = c(1, 10)) +
  theme(legend.position = c(0.15, 0.85),
        panel.border = element_rect(colour = "black", fill = NA),
        plot.background = element_rect(fill = "floralwhite"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(size = 0.5, linetype = "solid",
                                        colour = "#ececec"),
        panel.grid.minor = element_line(size = 0.5, linetype = "solid",
                                        colour = "#ececec"),
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 9, hjust = 0.5)) +
  geom_hline(yintercept = 0.423, linetype = 2, color = "gray55") +
  annotate(geom = "label",
           x = 3, y = .49,
           label = "Season Average\n42.3%",
           fontface = "bold") +
  scale_fill_gradient2(low = ("#0571b0"),
                       mid = "white",
                       high = ("#ca0020"),
                       midpoint = 0.423,
                       guide = FALSE)
