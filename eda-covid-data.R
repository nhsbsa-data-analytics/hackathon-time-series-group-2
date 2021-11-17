library(dplyr)
library(ggplot2)

# Read in data from GitHub with correct types
df <- readr::read_csv(
  file = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv",
  col_types = "cccDdddddddddddddddddddddddddddddcddddddddddddddddddddddddddddddddd"
)

# Aggregate to worldwide
worldwide_df <- df %>%
  group_by(date) %>%
  summarise(
    total_cases = sum(total_cases, na.rm = TRUE),
    new_cases = sum(new_cases, na.rm = TRUE)
  ) %>%
  ungroup()

# Chart it
worldwide_df %>%
  ggplot(aes(x = date, y = total_cases)) +
  geom_col()
worldwide_df %>%
  ggplot(aes(x = date, y = new_cases)) +
  geom_col()

# # Aggregate to continent
continent_df <- df %>%
  group_by(date, continent) %>%
  summarise(
    total_cases = sum(total_cases, na.rm = TRUE),
    new_cases = sum(new_cases, na.rm = TRUE)
  ) %>%
  ungroup()

# Chart it
continent_df %>%
  ggplot(aes(x = date, y = total_cases, col = continent)) +
  geom_line()
continent_df %>%
  ggplot(aes(x = date, y = new_cases, col = continent)) +
  geom_line()

# Mix of types so lets try again
continent_df <- df %>%
  dplyr::filter(
    location %in% unique(df$continent),
    !is.na(location)
  )

# Chart it
continent_df %>%
  ggplot(aes(x = date, y = total_cases, col = location)) +
  geom_line()
continent_df %>%
  ggplot(aes(x = date, y = new_cases, col = location)) +
  geom_line()

# What about countries in Europe
europe_df <- df %>%
  filter(continent == "Europe")

# Chart it
europe_df %>%
  ggplot(aes(x = date, y = total_cases, col = location)) +
  geom_line()
europe_df %>%
  ggplot(aes(x = date, y = new_cases, col = location)) +
  geom_line()