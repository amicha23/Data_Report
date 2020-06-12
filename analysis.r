# analysis file
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(leaflet)
library(plotly)
library(knitr)
library(styler)
library(lintr)

shootings_df <- read.csv("data/shootings-2018.csv", stringsAsFactors = F)

# Summary Information

# Number of shootings
shootings_count <- nrow(shootings_df)

# Lives lost
lives_lost <- shootings_df %>%
  select(num_killed) %>%
  sum()

# City impacted most
most_impacted_city <- shootings_df %>%
  group_by(city) %>%
  summarize(shooting_count = n()) %>%
  arrange(-shooting_count) %>%
  filter(shooting_count == max(shooting_count)) %>%
  pull(city)

most_impacted_city_count <- shootings_df %>%
  group_by(city) %>%
  summarize(shooting_count = n()) %>%
  arrange(-shooting_count) %>%
  filter(shooting_count == max(shooting_count)) %>%
  pull(shooting_count)

# First insight
most_impacted_state <- shootings_df %>%
  group_by(state) %>%
  summarize(count = n()) %>%
  arrange(-count) %>%
  filter(count == max(count)) %>%
  pull(state) %>%
  paste(collapse = " and ")

most_impacted_state_count <- shootings_df %>%
  group_by(state) %>%
  summarize(count = n()) %>%
  arrange(-count) %>%
  filter(count == max(count)) %>%
  select(count) %>%
  pull(1) %>%
  head(1)

# Second insight
get_month <- shootings_df %>%
  select(date) %>%
  pull(date) %>%
  as.Date("%B %d, %Y") %>%
  format("%B")

most_impacted_month_count <- shootings_df %>%
  mutate(month = get_month) %>%
  group_by(month) %>%
  summarize(shooting_count = n()) %>%
  arrange(-shooting_count) %>%
  filter(shooting_count == max(shooting_count, na.rm = T)) %>%
  pull(shooting_count)

most_impacted_month <- shootings_df %>%
  mutate(month = get_month) %>%
  group_by(month) %>%
  summarize(shooting_count = n()) %>%
  arrange(-shooting_count) %>%
  filter(shooting_count == max(shooting_count, na.rm = T)) %>%
  pull(month)

# Summary Table

aggregate_table <- shootings_df %>%
  mutate(month = get_month) %>%
  group_by(month) %>%
  summarize(
    Total_Shootings = n(),
    Total_Killed = sum(num_killed, na.rm = T),
    Total_Injured = sum(num_injured, na.rm = T),
    Avg_Lat = mean(lat, na.rm = T),
    Avg_Lng = mean(long, na.rm = T)
  ) %>%
  arrange(-Total_Shootings)

aggregate_info <- kable(aggregate_table,
  col.names = c(
    "Month", "Total Shootings", "Total Killed",
    "Total Injured", "Average Latitude", "Average Longitude"
  ),
  digits = 2,
  caption = "Aggregate Table based on Months"
)

# Particular Incident
particular_incident <- shootings_df %>%
  filter(num_killed == max(num_killed))

incident_date <- particular_incident %>%
  pull(date)

incident_state <- particular_incident %>%
  pull(state)

incident_city <- particular_incident %>%
  pull(city)

incident_address <- particular_incident %>%
  pull(address)

incident_injured <- particular_incident %>%
  pull(num_injured)

incident_killed <- particular_incident %>%
  pull(num_killed)


# Interactive Map
description_table <- shootings_df %>%
  mutate(description = paste0(
    city, ", ", state, ": ", "<br>",
    "Date: ", date, "<br>",
    "Address: ", address, "<br>",
    "Total Injured: ", num_injured, "<br>",
    "Total Killed: ", num_killed
  ))

interactive_map <- leaflet(data = description_table) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = -95, lat = 37.0902, zoom = 3.7) %>%
  addCircles(
    lat = ~lat,
    lng = ~long,
    popup = ~description,
    radius = ~ (num_killed * 5000),
    stroke = F,
  )

interactive_map

# Plot
# Which states had the most shootings (encode months)?
# Based on the the top 3 states with the most shootings
# where total deaths were at least one, which month had the most deaths?
plot_table <- shootings_df %>%
  mutate(month = get_month) %>%
  group_by(month, state) %>%
  summarize(
    Total_Shootings = n(),
    Total_Killed = sum(num_killed, na.rm = T),
    Total_Injured = sum(num_injured, na.rm = T)
  ) %>%
  filter(state == "California" | state == "Illinois" | state == "Florida") %>%
  filter(Total_Killed > 0)

top_3 <- shootings_df %>%
  mutate(month = get_month) %>%
  group_by(state) %>%
  summarise(Total_Shootings = n()) %>%
  arrange(-Total_Shootings) %>%
  head(3) %>%
  pull(state)

deadliest_months <- ggplot(
  data = plot_table,
  aes(Month = month, Killed = Total_Killed)
) +
  geom_col(mapping = aes(
    x = reorder(month, Total_Killed),
    y = Total_Killed,
    fill = state
  )) +
  coord_flip() +
  scale_color_brewer(palette = "Set3") +
  labs(
    x = "Months (Deaths >= 1)",
    y = "Total Deaths",
    title = paste0(
      "Based on the the top 3 states with the most shootings, ",
      "which month had the most deaths?"
    )
  )


plotly_graph <- ggplotly(deadliest_months,
  tooltip = c("Month", "Killed", "state")
)
plotly_graph
