library(tidyverse)



all_animals <- rbind(elephant1, tiger1, lion1, leopard1)




View(data)

data <- all_animals |>
  relocate(heading, abstract,title, spatial,animal, date, date_fix) |>
  mutate(spatial = case_when(spatial == "New South Wales" ~ "NSW",
                             spatial == "South Australia" ~ "SA",
                             spatial == "Tasmania" ~ "TAS",
                             spatial == "Western Australia" ~ "WA",
                             spatial == "Victoria" ~ "VIC",
                             spatial == "Queensland" ~ "QLD",
                             spatial == "Northern Territory" ~ "NT",
                             spatial == "National" ~ "ACT",
                             TRUE ~ as.character(spatial))) |>
  mutate(spatial = case_when(title=="The Australian Women's Weekly (1933 - 1982)" ~ "NSW",
                             TRUE ~ as.character(spatial))) |>
  rename(trove_full_title = title) |>
  mutate(title = str_remove_all(trove_full_title, "\\s*\\([^\\)]+\\)")) |>
  mutate(merge_title = str_c(title, spatial, sep = " ")) |>

  left_join(cleaned_location_list, by="merge_title") |>
  filter(!trove_full_title %in% foreign)  |>
  distinct()

# View(check)
#
# check <- data |>
#   filter(is.na(latitude))

write.xlsx(data, "all_animals_data.xlsx")

