file_list <- c("data_2014.txt", "data_2015.txt", "data_2016.txt", "data_2017.txt", 
               "data_2018.txt", "data_2019.txt", "data_2020.txt", "data_2021.txt")
years <- c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)  #years vector
modified_data_list <- list()
for (i in 1:length(file_list)) {
  data <- read.csv(file_list[i], header = FALSE)
  
  if(ncol(data) != 2) { # in case data is missing values
    print("Error detected in file" + file_list[i] + "!")
  }
  
  data$Year <- years[i]
  colnames(data) <- c("Result", "Name", "Year")
  modified_data_list[[i]] <- data
}
combined_data <- do.call(rbind, modified_data_list)
combined_data


library(dplyr)
win_list <- c("wins_2014.txt", "wins_2015.txt", "wins_2016.txt", "wins_2017.txt", 
              "wins_2018.txt", "wins_2019.txt", "wins_2020.txt", "wins_2021.txt")
years <- c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
modified_win_list <- list()
for (i in 1:length(win_list)) {
  windata <- read.csv(win_list[i], header = FALSE)
  if (ncol(windata) == 2) {
    windata$Year <- years[i]
    colnames(windata) <- c("Name", "Wins", "Year")
    modified_win_list[[i]] <- windata
  }
}
combined_wins <- do.call(rbind, modified_win_list)
combined_wins <- combined_wins |>
  filter(grepl("\\*", Name)) |>
  mutate(Name = gsub("\\*$", "", Name))
print(combined_wins)

team_lookup <- data.frame(
  Team = c("San Antonio Spurs", "Oklahoma City Thunder", "Miami Heat", "Indiana Pacers",
                "Los Angeles Clippers", "Portland Trail Blazers", "Brooklyn Nets", "Washington Wizards",
                "Golden State Warriors", "Dallas Mavericks", "Memphis Grizzlies", "Toronto Raptors",
                "Houston Rockets", "Atlanta Hawks", "Chicago Bulls", "Charlotte Bobcats",
                "Cleveland Cavaliers", "Boston Celtics", "Detroit Pistons", "Milwaukee Bucks",
                "New Orleans Pelicans", "Boston Celtics", "Los Angeles Lakers", "Philadelphia 76ers",
                "Minnesota Timberwolves", "Utah Jazz", "Denver Nuggets", "New York Knicks", "Phoenix Suns", "Orlando Magic"),
  abbreviation = c("SAS", "OKC", "MIA", "IND", "LAC", "POR", "BKN", "WAS", "GSW", "DAL", "MEM",
                   "TOR", "HOU", "ATL", "CHI", "CHA", "CLE", "BOS", "DET", "MIL", "NOP", "BOS",
                   "LAL", "PHI", "MIN", "UTA", "DEN", "NYK", "PHO", "ORL")
)

combined_wins <- combined_wins |>
  left_join(team_lookup, by = c("Name" = "Team")) |>
  mutate(Name = abbreviation) |>
  select(-abbreviation)
print(combined_wins)

combined_data <- combined_data |>
  left_join(team_lookup, by = c("Name" = "Team")) |>
  mutate(Name = abbreviation) |>
  select(-abbreviation)
print(combined_data)

combined_data <- combined_data |>
  transform(Name_Year = paste(Name, Year, sep = "_")) |>
  select(-Name, -Year)  # Remove the Name and Year columns
combined_data

combined_wins <- combined_wins |>
  transform(Name_Year = paste(Name, Year, sep = "_")) |>
  select(-Name, -Year)  # Remove the Name and Year columns
combined_wins

WAR <- read.csv("C:/Users/swara/Downloads/WAR.csv")
WAR <- subset(WAR, war_reg_season != 0)
combined_WAR <- WAR |>
  transform(Name_Year = paste(team, year, sep = "_")) |>
  select(-team, -year)  # Remove the Name and Year columns
combined_WAR
combined_WAR <- combined_WAR |>
  group_by(Name_Year) |>
  summarise(Average_WAR = mean(war_reg_season, na.rm = TRUE))
combined_WAR

combined_result <- merge(combined_wins, combined_data, by = "Name_Year")
combined_result <- combined_result[, c("Name_Year", "Result", "Wins")]
finaldata <- merge(combined_result, combined_WAR, by = "Name_Year")
finaldata
