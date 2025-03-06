# 259 Homework - exploratory data analysis + integrating skills
# For full credit, answer at least 8/10 questions
# List students working with below:

library(tidyverse)
library(lubridate)
library(DataExplorer)

#> These data are drawn from the fivethirtyeight article:
#> http://fivethirtyeight.com/features/what-12-months-of-record-setting-temperatures-looks-like-across-the-u-s/
#> The directory us-weather-history contains a data file for each of 10 cities, labelled by their station name
#> Each data file contains:
#> `date` | The date of the weather record, formatted YYYY-M-D
#> `actual_mean_temp` | The measured average temperature for that day
#> `actual_min_temp` | The measured minimum temperature for that day
#> `actual_max_temp` | The measured maximum temperature for that day
#> `average_min_temp` | The average minimum temperature on that day since 1880
#> `average_max_temp` | The average maximum temperature on that day since 1880
#> `record_min_temp` | The lowest ever temperature on that day since 1880
#> `record_max_temp` | The highest ever temperature on that day since 1880
#> `record_min_temp_year` | The year that the lowest ever temperature occurred
#> `record_max_temp_year` | The year that the highest ever temperature occurred
#> `actual_precipitation` | The measured amount of rain or snow for that day
#> `average_precipitation` | The average amount of rain or snow on that day since 1880
#> `record_precipitation` | The highest amount of rain or snow on that day since 1880

stations <- c("KCLT", "KCQT", "KHOU", "KIND", "KJAX", "KMDW", "KNYC", "KPHL", "KPHX", "KSEA")
cities <- c("Charlotte", "Los Angeles", "Houston", "Indianapolis", "Jacksonville", 
            "Chicago", "New York City", "Philadelphia", "Phoenix", "Seattle")


# QUESTION 1
#> The data files are in the directory 'us-weather-history'
#> Write a function that takes each station abbreviation and reads
#> the data file and adds the station name in a column
#> Make sure the date column is a date
#> The function should return a tibble
#> Call the function "read_weather" 
#> Check by reading/glimpsing a single station's file

library(tidyverse)
library(lubridate)

read_weather <- function(station) {
  file_path <- paste0("us-weather-history/", station, ".csv")
  weather_data <- read_csv(file_path) %>%
    mutate(
      station = station,  
      date = ymd(date)   
    )
  return(weather_data)
}
weather <- read_weather("KCLT")
glimpse(weather)



# QUESTION 2
#> Use map() and your new function to read in all 10 stations
#> Note that because map_dfr() has been superseded, and map() does not automatically bind rows, you will need to do so in the code.
#> Save the resulting dataset to "ds"

ds <- map(stations, read_weather) %>%
  bind_rows()
glimpse(ds)


# QUESTION 3
#> Make a factor called "city" based on the station variable
#> (station should be the level and city should be the label)
#> Use fct_count to check that there are 365 days of data for each city 

library(tidyverse)
library(forcats)
ds <- ds %>%
  mutate(city = factor(station, levels = stations, labels = cities))
fct_count(ds$city)


# QUESTION 4
#> Since we're scientists, let's convert all the temperatures to C
#> Write a function to convert F to C, and then use mutate across to 
#> convert all of the temperatures, rounded to a tenth of a degree

conversion <- function(f) {
  round((f - 32) * 5 / 9, 1)
}
ds <- ds %>%
  mutate(across(contains("_temp"), conversion))
glimpse(ds)


### CHECK YOUR WORK
#> At this point, your data should look like the "compiled_data.csv" file
#> in data-clean. If it isn't, read in that file to use for the remaining
#> questions so that you have the right data to work with.

compiled_data <- read_csv("data-clean/compiled_data.csv")
glimpse(ds)
glimpse(compiled_data)
ds <- compiled_data


# QUESTION 5
#> Write a function that counts the number of extreme temperature days,
#> where the actual min or max was equal to the (i.e., set the) record min/max
#> A piped function starting with '.' is a good strategy here.
#> Group the dataset by city to see how many extreme days each city experienced,
#> and sort in descending order to show which city had the most:
#> (Seattle, 20, Charlotte 12, Phoenix 12, etc...)
#> Don't save this summary over the original dataset!


extreme_temperature_days <- function(df) {
  df %>%
    filter(actual_min_temp == record_min_temp | actual_max_temp == record_max_temp) %>%
    count(city, name = "extreme_days") %>%
    arrange(desc(extreme_days))
}
extreme_temperature_days <- extreme_temperature_days(ds)
print(extreme_temperature_days)


# QUESTION 6
#> Pull out the month from the date and make "month" a factor
#> Split the tibble by month into a list of tibbles 

ds <- ds %>%
  mutate(month = factor(month(date), labels = month.name))
month_data <- split(ds, ds$month)
str(month_data)


# QUESTION 7
#> For each month, determine the correlation between the actual_precipitation
#> and the average_precipitation (across all cities), and between the actual and average mins/maxes
#> Use a for loop, and print the month along with the resulting correlation
#> Look at the documentation for the ?cor function if you've never used it before

for (month_name in names(month_data)) {
  month_current_data <- month_data[[month_name]]
  cor_precipitation <- cor(month_current_data$actual_precipitation, month_current_data$average_precipitation, use = "complete.obs")
  cor_min_temp <- cor(month_current_data$actual_min_temp, month_current_data$average_min_temp, use = "complete.obs")
  cor_max_temp <- cor(month_current_data$actual_max_temp, month_current_data$average_max_temp, use = "complete.obs")
  cat("Month:", month_name, "\n")
  cat("Correlation between actual and average precipitation:", cor_precipitation, "\n")
  cat("Correlation between actual and average min temp:", cor_min_temp, "\n")
  cat("Correlation between actual and average max temp:", cor_max_temp, "\n\n")
}

# QUESTION 8
#> Use the Data Explorer package to plot boxplots of all of the numeric variables in the dataset
#> grouped by city, then do the same thing grouped by month. 
#> Finally, use plot_correlation to investigate correlations between the continuous variables only
#> Check the documentation for plot_correlation for an easy way to do this

library(DataExplorer)
plot_boxplot(ds, by = "city")
plot_boxplot(ds, by = "month")
plot_correlation(ds, type = "continuous")



# QUESTION 9
#> Create a scatterplot of actual_mean_temp (y axis) by date (x axis)
#> Use facet_wrap to make a separate plot for each city (3 columns)
#> Make the points different colors according to month

library(ggplot2)
ggplot(ds, aes(x = date, y = actual_mean_temp, color = month)) +
  geom_point() +
  facet_wrap(~ city, ncol = 3) +
  labs(
    title = "Scatterplot of Actual Mean Temperature by Date",
    x = "Date",
    y = "Actual Mean Temperature",
    color = "Month"
  ) +
  theme_minimal()


# QUESTION 10
#> Write a function that takes the dataset and the abbreviate month as arguments
#> and creates a scatter and line plot of actual temperature (y axis) by date (x axis)
#> Note, just add geom_line() to your ggplot call to get the lines
#> use the ggtitle() function to add the month as a title
#> The function should save the plot as "eda/month_name.png"
#> The eda folder has an example of what each plot should look like
#> Call the function in a map or loop to generate graphs for each month

createplot <- function(dataset, month_abbr) {
  month_data <- dataset %>% filter(str_to_lower(month(date)) == str_to_lower(month_abbr))

  plot <- ggplot(month_data, aes(x = date, y = actual_mean_temp)) +
    geom_point() +               # Scatter plot
    geom_line() +                # Line plot
    ggtitle(paste("Temperature in", month_abbr)) +  
    labs(x = "Date", y = "Actual Mean Temperature") +
    theme_minimal()
  ggsave(paste0("eda/", month_abbr, ".png"), plot)
}
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
for (month_abbr in months) {
  createplot(ds, month_abbr)
}


