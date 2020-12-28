#' ---
#' title: "Chess"
#' author: "Jas Kainth"
#' date: "28/12/2020"
#' output: html_document
#' editor_options: 
#'   chunk_output_type: console
#' ---
#' 
## ----setup, include=FALSE----------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(ggsci)
library(tidytext)
library(scales)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

#' 
#' ## Getting the data ready  
#' 
## ----data--------------------------------------------------------------------------
# Load in the data
chess_raw <- read_csv("games.csv") %>% 
  # Make the id more readable
  mutate(id = row_number())
# Let's categorize the most common increments
rapid <- c("10+0", "15+0", "5+5", "5+8", "8+0", "10+5", "15+10", "20+0",
           "10+10", "15+5", "7+2", "10+2", "5+10", "10+3", "10+8", "10+15",
           "15+2")
classic <- c("15+15", "30+0", "30+30", "25+0", "20+10")
chess_df <- chess_raw %>% 
  # Create a column for the formats
  # We only include the ones that appear 100 times or more
  mutate(format = case_when(increment_code %in% rapid ~ "Rapid",
                            increment_code %in% classic ~ "Classic")) %>%
  # Create a column for opening, take the opening_name and remove everything
  # after the colon as that indicates the variation and we might only want to 
  # look at the opening
  mutate(opening = str_remove(opening_name, ":.*")) %>% 
  # We also have some '|', so we will remove those too
  mutate(opening = str_remove(opening, "\\|.*")) %>%
  # Create a column for the difference between the white and black elo
  mutate(elo_difference = abs(white_rating - black_rating),
         higher = case_when(black_rating > white_rating ~ "Black",
                            white_rating > black_rating ~ "White",
                            TRUE ~ "Equal")) %>%
  # Create a column for the level of play, we will classify them as
  # Beginner: Less than 1200
  # Intermediate: Less than 1800
  # Expert: More than 1800
  # For this we will also make sure the difference between the players is 
  # less than 300 because otherwise it may be a total mismatch
  # We will consider the higher player for the levels
  # Idealy there would be another level, we could do 1800 to 2400 and then
  # greater than 2400 but we do not have a lot of games with an elo rating of
  # that high
  mutate(highest_elo = case_when(white_rating > black_rating ~ white_rating,
                                 TRUE ~ black_rating),
         level = case_when(highest_elo < 1200 ~ "Beginner",
                           highest_elo < 1800 ~ "Intermediate",
                           TRUE ~ "Expert"),
         level = case_when(elo_difference < 300 ~ level)) %>%
  # Filter out the games that have fewer than 10 turns, these are most likely
  # games where the result was decide prior to the game
  filter(turns >= 10) %>%
  # Let's clean up the victory_status column
  mutate(victory_status = case_when(victory_status == "draw" ~ "Draw",
                                    victory_status == "mate" ~ "Checkmate",
                                    victory_status == "resign" ~ "Resign",
                                    TRUE ~ "Out of Time"),
         # And the Winner column
         winner = str_to_title(winner))

# Let's make a final dataframe of the relevant columns
chess <- chess_df %>%
  select(id, rated, white_id, white_rating, black_id, black_rating,
         elo_difference, opening_name, opening, opening_ply, level, format, 
         increment_code, higher, victory_status, turns, moves, winner)


#' 
#' ## Let's take a look at the openings 
#' 
## ----opening, fig.height=6, fig.width=10, fig.align='center'-----------------------
# What are the most popular openings?
chess %>% 
  count(opening, sort = TRUE) %>% 
  head(15) %>%
  mutate(opening = fct_reorder(opening, n)) %>%
  ggplot(aes(x = n, y = opening)) + 
  geom_col() + 
  theme_minimal() + 
  theme(text = element_text("Avenir Next Condensed")) +
  labs(title = "What openings are played the most often?",
       x = " ", 
       y = " ") + 
  scale_x_continuous(expand = c(0, 0))

#' 
## ----openingLevel, fig.height=6, fig.width=10, fig.align='center'------------------
# How does this change with the different levels?
chess %>% 
  filter(!is.na(level)) %>%
  add_count(level, name = "total_games") %>% 
  group_by(level, opening) %>% 
  summarise(number_of_games = n(),
            total_games = total_games) %>%
  ungroup() %>%
  distinct() %>%
  group_by(level) %>%
  slice_max(order_by = number_of_games, n = 10, with_ties = FALSE) %>%
  mutate(prop = number_of_games / total_games) %>%
  ungroup() %>%
  mutate(level = glue::glue("{ level } (Total Games: { total_games })")) %>%
  mutate(opening = reorder_within(opening, by = prop, within = level)) %>%
  ggplot(aes(x = prop, y = opening, fill = level)) +
  geom_col() + 
  theme_minimal() + 
  theme(legend.position = "none",
        text = element_text("Avenir Next Condensed")) +
  facet_wrap(~ level, scales = "free_y") + 
  scale_y_reordered() + 
  scale_x_continuous(labels = percent) +
  scale_fill_futurama() +
  labs(title = "Openings played based on level",
       x = "Proportion of games at each level playing specific openings",
       y = " ")

#' 
## ----outcome, fig.height=6, fig.width=10, fig.align='center'-----------------------
# How do the games tend to end based on the opening (we won't look at this based
# on level since there won't be a lot of data in each group)
chess %>% 
  add_count(opening, name = "total_games") %>%
  group_by(opening) %>% 
  add_count(victory_status, name = "outcome") %>% 
  ungroup() %>%
  select(opening, victory_status, total_games, outcome) %>%
  distinct() %>%
  arrange(desc(total_games)) %>% 
  head(40) %>%
  mutate(pct = outcome / total_games) %>%
  # We will create a column with the percent of checkmates 
  # We do this so we can later reorder the openings based on the highest values
  # of percent of games ending with checkmate
  group_by(opening) %>%
  mutate(pct_checkmate = pct[victory_status == "Checkmate"]) %>%
  ungroup() %>%
  mutate(opening = glue::glue("{ opening } (Total Games: { total_games })")) %>%
  mutate(victory_status = fct_relevel(victory_status, "Draw", "Out of Time",
                                      "Resign"),
         opening = fct_reorder(opening, pct_checkmate)) %>%
  ggplot(aes(x = pct, y = opening, fill = victory_status)) + 
  geom_col() +
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_minimal() +
  theme(text = element_text("Avenir Next Condensed"),
        panel.grid.major = element_line("white", size = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.ontop = TRUE) + 
  scale_x_continuous(labels = percent,
                     expand = c(0, 0)) + 
  scale_fill_futurama() +
  labs(title = "Outcomes based on Openings",
       x = "Percent of Games",
       y = " ",
       fill = "Outcome")


#' 
## ----theory, fig.height=6, fig.width=10, fig.align='center'------------------------
# Which openings have the most amount of theory moves
chess %>% 
  arrange(desc(opening_ply)) %>% 
  head(25) %>% 
  select(opening_ply, opening, id) %>%
  mutate(for_axis = glue::glue("{ opening } - Match ID: { id }")) %>%
  mutate(for_axis = fct_reorder(for_axis, opening_ply)) %>%
  ggplot(aes(x = opening_ply, y = for_axis, fill = opening)) + 
  geom_col() + 
  labs(title = "Which games contained the most opening moves based on theory?",
       x = "# of Moves",
       y = " ") + 
  theme_minimal() + 
  theme(text = element_text("Avenir Next Condensed"),
        legend.position = "none") +
  scale_fill_lancet() + 
  scale_x_continuous(expand = c(0, 0))

#' 
## ----theoryLevel, fig.height=6, fig.width=10, fig.align='center'-------------------
# What are the most moves based on theory per level?
chess %>% 
  filter(!is.na(level)) %>%
  mutate(opening = str_remove(opening, "\\|.*")) %>%
  group_by(level, opening) %>%
  summarise(theory_moves = mean(opening_ply)) %>%
  ungroup() %>%
  group_by(level) %>%
  slice_max(order_by = theory_moves, n = 10, with_ties = FALSE) %>%
  mutate(opening = reorder_within(opening, by = theory_moves, within = level)) %>%
  ggplot(aes(x = theory_moves, y = opening, fill = level)) + 
  geom_col() + 
  facet_wrap(~ level, scales = "free_y") + 
  scale_y_reordered() + 
  theme_minimal() +
  theme(text = element_text("Avenir Next Condensed"),
        legend.position = "none") + 
  scale_fill_futurama() + 
  labs(title = "# of beginning moves played on theory", 
       x = " ",
       y = " ")

#' 
## ----rated, fig.height=6, fig.width=10, fig.align='center'-------------------------
# Is there a difference in which openings are played if the games are rated or 
# not? Some openings may be a bit more fun but also more difficult to play so we
# may see those played more often with unrated games
chess %>% 
  group_by(rated) %>% 
  count(opening) %>% 
  summarise(opening = opening,
            scenario = n,
            total_games = sum(n)) %>%
  ungroup() %>%
  mutate(pct = scenario / total_games) %>% 
  group_by(rated) %>% 
  slice_max(order_by = pct, n = 15, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(rated = case_when(rated ~ "Rated",
                           TRUE ~ "Unrated"),
         opening = reorder_within(opening, by = pct, within = rated)) %>%
  ggplot(aes(x = pct, y = opening, fill = rated)) + 
  geom_col() + 
  theme_minimal() +
  theme(text = element_text("Avenir Next Condensed"),
        legend.position = "none") +
  facet_wrap(~ rated, scales = "free_y") + 
  scale_y_reordered() + 
  scale_x_continuous(labels = percent) +
  scale_fill_lancet() + 
  labs(title = "What openings are most common among rated and unrated games?",
       x = "% of Games",
       y = " ")


#' 
#' ## Let's take a look at elo ratings
#' 
## ----higherElo, fig.height=6, fig.width=10, fig.align='center'---------------------
# How often does the higher rated player win?
chess %>% 
  count(higher, winner) %>% 
  group_by(higher) %>% 
  summarise(total_games = sum(n),
            scenario = n,
            winner = winner) %>% 
  ungroup() %>% 
  mutate(pct = scenario / total_games,
         higher = glue::glue("{ higher } ({ total_games } Games)")) %>%
  ggplot(aes(x = pct, y = higher, fill = winner)) + 
  geom_col() +
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_minimal() + 
  theme(text = element_text("Avenir Next Condensed")) + 
  scale_fill_futurama() + 
  scale_x_continuous(labels = percent,
                     expand = c(0, 0)) + 
  labs(title = "How often does the higher rated player win?",
       x = "% of Games",
       y = "Higher rated player",
       fill = "Winner")



#' 
## ----higherEloLevel, fig.height=12, fig.width=10, fig.align='center'---------------
# How does the plot above change by the different levels?
chess %>% 
  filter(!is.na(level)) %>%
  count(higher, winner, level) %>% 
  group_by(higher, level) %>% 
  summarise(total_games = sum(n),
            scenario = n,
            winner = winner) %>% 
  ungroup() %>% 
  mutate(pct = scenario / total_games,
         higher = glue::glue("{ higher } ({ total_games } Games)")) %>% 
  ggplot(aes(x = pct, y = higher, fill = winner)) + 
  geom_col() +
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_minimal() + 
  theme(text = element_text("Avenir Next Condensed")) + 
  scale_fill_futurama() + 
  scale_x_continuous(labels = percent,
                     expand = c(0, 0)) + 
  labs(title = "How often does the higher rated player win within each level?",
       x = "% of Games",
       y = "Higher rated player",
       fill = "Winner") + 
  facet_wrap(~ level, scales = "free_y", ncol = 1)
  

#' 
## ----eloDifference, fig.height=8, fig.width=10, fig.align='center'-----------------
# Let's take a look at the elo difference column 
# For this we won't split it up into groups with the level 
# Let's filter out the games with an elo difference of greater than 900
chess %>% 
  filter(elo_difference < 900) %>% 
  filter(higher != "Equal") %>%
  # Let's make ranges for the Elo difference 
  # We will go up by 100
  mutate(elo_range = paste0("Elo Difference <=", 
                            floor(elo_difference / 100) * 100)) %>% 
  group_by(elo_range) %>% 
  count(winner, higher) %>% 
  ungroup() %>%
  group_by(elo_range, higher) %>%
  summarise(winner = winner,
            scenario = n,
            total_games = sum(n)) %>%
  ungroup() %>% 
  mutate(pct = scenario / total_games) %>%
  ggplot(aes(x = pct, y = higher, fill = winner)) +
  geom_col() +
  facet_wrap(~ elo_range) + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_minimal() + 
  theme(text = element_text("Avenir Next Condensed")) + 
  scale_fill_futurama() + 
  scale_x_continuous(labels = percent,
                     expand = c(0, 0)) + 
  labs(title = "How often does the higher rated player win?",
       x = "% of Games",
       y = "Higher rated player",
       fill = "Winner")


