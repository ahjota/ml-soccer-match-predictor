library(readr)
allSpi <- read_csv("/Users/aj/workspace/ml-spi-demo/spi_matches.csv")
spec(allSpi)
is(allSpi)
is(allSpi$date)
# we want to predict the 2020 season, so let's split those out
spi2020raw <- allSpi[ which(allSpi$season > "2019"), ]
# we will use 2019 and before for training
spi2019raw <- allSpi[ which(allSpi$season < "2020"), ]
library(dplyr)
spi2019clean <- spi2019raw %>%
  select(!c(xg1, xg2, nsxg1, nsxg2,
            adj_score1, adj_score2,
            prob1, prob2, probtie,
            proj_score1, proj_score2,
            adj_score1, adj_score2,
            league_id,
            importance1, importance2))
head(spi2019clean)
library(purrr)
# now create a tibble with the dataset
spi2019train <- tibble(
  season = double(),
  date = integer(),
  league = character(),
  team1 = character(),
  team2 = character(),
  spi1 = double(),
  spi2 = double(),
  score1 = double()
  )
class(spi2019train$date) <- "Date"
# duplicate rows so that we are predicting the score1 of team1 when it plays team2
spi2019train <- 1:length(spi2019clean$season) %>%
  spi2019clean[.,] %>%
  select(season, date, league, team1, team2, spi1, spi2, score1) %>%
  add_row(spi2019train, .)

spi2019train <- 1:length(spi2019clean$season) %>%
  spi2019clean[.,] %>%
  select(season, date, league, team2, team1, spi2, spi1, score2) %>%
  rename(team1 = team2, team2 = team1, spi1 = spi2, spi2 = spi1, score1 = score2) %>%
  add_row(spi2019train, .)

spi2019train %<>%
  add_column(aid = paste(spi2019train$date, spi2019train$league, spi2019train$team1, spi2019train$team2)) %>%
  arrange(aid)

spi2019train

if(count(spi2019train) == 2 * count(spi2019clean)) {
  write_csv(spi2019train, "/Users/aj/workspace/ml-spi-demo/spi-2019-training-set.csv")
}

spi2020clean <- spi2020raw %>%
  select(season, date, league, team1, team2, spi1, spi2, score1, score2)
spi2020actual <- tibble(
  season = double(),
  date = integer(),
  league = character(),
  team1 = character(),
  team2 = character(),
  spi1 = double(),
  spi2 = double(),
  score1 = double()
)
class(spi2020actual$date) <- "Date"
spi2020actual <- 1:length(spi2020clean$season) %>%
  spi2020clean[.,] %>%
  select(season, date, league, team1, team2, spi1, spi2, score1) %>%
  add_row(spi2020actual, .)
spi2020actual <- 1:length(spi2020clean$season) %>%
  spi2020clean[.,] %>%
  select(season, date, league, team2, team1, spi2, spi1, score2) %>%
  rename(team1 = team2, team2 = team1, spi1 = spi2, spi2 = spi1, score1 = score2) %>%
  add_row(spi2020actual, .)
if(count(spi2020actual) == 2 * count(spi2020clean)) {
  write_csv(spi2020actual, "/Users/aj/workspace/ml-spi-demo/spi-2020-actuals.csv")
}
library(tibble)
spi2020actual %>%
  add_column(aid = paste(spi2020actual$date, spi2020actual$league, spi2020actual$team1, spi2020actual$team2)) %>%
  arrange(aid)
spi2020externaltest <- spi2020actual %>%select(!score1)
spi2020externaltest
write_csv(spi2020externaltest, "/Users/aj/workspace/ml-spi-demo/spi-2020-external-test.csv")
