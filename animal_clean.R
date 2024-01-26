## cleaning data files extracted from trove

library(readxl)
library(tidyverse)
library(openxlsx)
library(janitor)


elephant <- read_excel("all_animals_original_downloads.xlsx", sheet="elephant")
lion <- read_excel("all_animals_original_downloads.xlsx", sheet="lion")
tiger <- read_excel("all_animals_original_downloads.xlsx", sheet="tiger")
leopard <- read_excel("all_animals_original_downloads.xlsx", sheet="leopard")

##removing all records with no mention of escape and elephant


include_words <- c("escape |escaped |escapes ")

elephant_exclude_words <- c("baromet's| baronets")

# View(elephant1)
elephant1 <- elephant |>
  mutate(animal="elephant") |>
  mutate(heading = str_to_lower(heading),
         abstract = str_to_lower(abstract)) |>
  filter(str_detect(heading, " elephant | elephants")) |>
  filter(str_detect(heading, include_words)) |>
  mutate(exclude = str_count(heading, elephant_exclude_words)) |>
  filter(exclude ==0) |>
  select(-exclude) |>
  mutate(date_fix = as_date(
    ifelse(
      is.na(as.numeric(date)),
      ymd(date),
      dmy("01-Jan-1900") + days(as.numeric(date)-2)
    )
  ))


lion_exclude_words <- c("sea-lion|sea lion|explorer|british|marquis|explorer's")

# View(lion1)

lion1 <- lion |>
  mutate(animal="lion") |>
  mutate(heading = str_to_lower(heading),
         abstract = str_to_lower(abstract)) |>
  filter(str_detect(heading, " lion | lions ")) |>
  filter(str_detect(heading, include_words)) |>
  mutate(exclude = str_count(heading, lion_exclude_words)) |>
  filter(exclude ==0) |>
  select(-exclude) |>
  mutate(date_fix = as_date(
    ifelse(
      is.na(as.numeric(date)),
      ymd(date),
      dmy("01-Jan-1900") + days(as.numeric(date)-2)
    )
  ))




# View(tiger1)

tiger_exclude_words <- c("snake|moth|shark|tiger woman")


tiger1 <- tiger |>
  mutate(animal="tiger") |>
  mutate(heading = str_to_lower(heading),
         abstract = str_to_lower(abstract)) |>
  filter(str_detect(heading, " tiger | tigers ")) |>
  filter(str_detect(heading, include_words)) |>
  mutate(exclude = str_count(heading, tiger_exclude_words)) |>
  filter(exclude ==0) |>
  select(-exclude) |>
  mutate(date_fix = as_date(
    ifelse(
      is.na(as.numeric(date)),
      ymd(date),
      dmy("01-Jan-1900") + days(as.numeric(date)-2)
    )
  ))


# View(tiger1)

# View(leopard1)

leopard1 <- leopard |>
  mutate(animal="leopard") |>
  mutate(heading = str_to_lower(heading),
         abstract = str_to_lower(abstract)) |>
  filter(str_detect(heading, " leopard | leopards ")) |>
  filter(str_detect(heading, include_words)) |>
  mutate(date_fix = as_date(
  ifelse(
    is.na(as.numeric(date)),
    ymd(date),
    dmy("01-Jan-1900") + days(as.numeric(date)-2)
  )
))

sheets <- list(elephant = elephant1,
               lion = lion1,
               leopard = leopard1,
               tiger = tiger1)

write.xlsx(sheets, "animal_escape_clean.xlsx")


