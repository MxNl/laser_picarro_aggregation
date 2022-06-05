library(tidyverse)
library(vroom)
library(janitor)
library(lubridate)

read_log_files <- function(filepath) {
  filepath |> 
    data.table::fread() |> 
    as_tibble() |> 
    select(DATE, TIME, H2O, Delta_18_16, Delta_D_H)
}

files <- list.files(
  "//bgr.local/gzh/Tmp/Berlin/Koeniger.P/2020-05", 
  pattern = "*.dat",
  full.names = TRUE
  )

# data_raw <- files[1:10] |> 
#   map_df(
#     read_table,
#     col_types = cols_only('DATE' = col_date(),
#                           'TIME' = col_character(),
#                           'H2O' = col_guess(),
#                           'Delta_18_16' = col_guess(),
#                           'Delta_D_H' = col_guess()
#       )
#   )

data_raw <- files[1:100] |> 
  map_df(read_log_files) |> 
  clean_names()

data_10mins <- data_raw |> 
  mutate(date_time = str_c(date, time, sep = "_"), .before = 1) |> 
  mutate(date_time = ymd_hms(date_time)) |> 
  select(-date, -time) |> 
  mutate(date_time_10min = floor_date(date_time, "10 min"), .after = 1) |> 
  group_by(date_time_10min) |> 
  summarise(across(where(is.numeric), mean))


data_10mins |> 
  pivot_longer(cols = -date_time_10min) |> 
  ggplot(aes(date_time_10min, value, colour = name)) +
  geom_line(show.legend = FALSE) +
  theme_minimal() +
  facet_wrap(~name, scales = "free_y", ncol = 1)
