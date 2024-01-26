library(tidyverse)

main_list_title_import <- read_csv("trove-newspaper-titles-locations.csv") |>
  rename(title = newspaper_title) |>
  clean_names()


main_list_title <- main_list_title_import|>
  select(title, state) |>
  distinct()


elephant_location_list <- elephant1 |>
  select(title, spatial) |>
  distinct()


lion_location_list <- lion1 |>
  select(title, spatial) |>
  distinct()

tiger_location_list <- tiger1 |>
  select(title, spatial) |>
  distinct()

leopard_location_list <- leopard1 |>
  select(title, spatial) |>
  distinct()



all_titles_main <- rbind(elephant_location_list, tiger_location_list, leopard_location_list, lion_location_list) |>
  distinct() |>
  mutate(spatial = case_when(spatial == "New South Wales" ~ "NSW",
                             spatial == "South Australia" ~ "SA",
                             spatial == "Tasmania" ~ "TAS",
                             spatial == "Western Australia" ~ "WA",
                             spatial == "Victoria" ~ "VIC",
                             spatial == "Queensland" ~ "QLD",
                             spatial == "Northern Territory" ~ "NT",
                             spatial == "National" ~ "ACT",
                             TRUE ~ as.character(spatial)))


city_location <- main_list_title_import |>
  select(title, place, state, longitude, latitude) |>
  group_by(title) |>
  mutate(row_count = row_number()) |>
  filter(row_count == 1) |>
  ungroup() |>
  select(-row_count)


# View(city_location)

foreign <- c("The South Australian Colonist and Settlers' Weekly Record of British, Foreign and Colonial Intelligence (London, England : 1840)",
             "The Tribune (Philippines : 1932 - 1945)",
             "Guinea Gold (Papua New Guinea : 1942 - 1945)")

# View(city_merge)

city_merge <- all_titles_main |>
  full_join(city_location, by="title") |>
  filter(!title %in% foreign) |>
  mutate(place = case_when(str_detect(title, "Kalgoorlie") ~ "Kalgoorlie",
                           str_detect(title, "Melbourne") ~ "Melbourne",
                           str_detect(title, "Echuca") ~ "Echuca",
                           str_detect(title, "Launceston") ~ "Launceston",
                           str_detect(title, "Albury") ~ "Albury",
                           str_detect(title, "Roma") ~ "Toowoomba",
                           str_detect(title, "Perth") ~ "Perth",
                           str_detect(title, "Orange") ~ "Orange",
                           str_detect(title, "Mildura") ~ "Mildura",
                           str_detect(title, "Sydney") ~ "Sydney",
                           str_detect(title, "Ballarat") ~ "Ballarat",
                           str_detect(title, "Hobart") ~ "Hobart",
                           str_detect(title, "Gundagai") ~ "Gundagai",
                           str_detect(title, "Inverell") ~ "Inverell",
                           str_detect(title, "Albany") ~ "Albany",
                           str_detect(title, "Brisbane") ~ "Brisbane",
                           str_detect(title, "Broome") ~ "Broome",
                           str_detect(title, "Adelaide") ~ "Adelaide",
                           str_detect(title, "Braidwood") ~ "Braidwood",
                           str_detect(title, "Wagga Wagga") ~ "Wagga Wagga",
                           str_detect(title, "Camperdown") ~ "Camperdown",
                           str_detect(title, "Tingha") ~ "Tingha",
                           str_detect(title, "Mitta") ~ "Tallangatta",
                           str_detect(title, "Northam") ~ "Northam",
                           str_detect(title, "Gnowangerup") ~ "Gnowangerup",
                           str_detect(title, "Benalla") ~ "Benalla",
                           str_detect(title, "Woodend") ~ "Woodend",
                           str_detect(title, "Benalla") ~ "Benalla",
                           str_detect(title, "Ipswich") ~ "Ipswich",
                           str_detect(title, "Coolgardie") ~ "Coolgardie",
                           str_detect(title, "Geraldton") ~ "Geraldton",
                           str_detect(title, "Cobram") ~ "Cobram",
                           str_detect(title, "Menzies") ~ "Menzies",
                           str_detect(title, "Richmond Guardian") ~ "Richmond",
                           str_detect(title, "Menzies") ~ "Menzies",
                           str_detect(title, "Maldon") ~ "Maldon",
                           str_detect(title, "Warrnambool") ~ "Warrnambool",
                           str_detect(title, "Bairnsdale") ~ "Bairnsdale",
                           str_detect(title, "Warrnambool") ~ "Warrnambool",
                           str_detect(title, "Dunolly") ~ "Dunolly",
                           str_detect(title, "Norseman") ~ "Norseman",
                           str_detect(title, "Bathurst") ~ "Bathurst",
                           str_detect(title, "Beechworth") ~ "Beechworth",
                           str_detect(title, "Narracan") ~ "Narracan",
                           str_detect(title, "Ararat") ~ "Ararat",
                           str_detect(title, "Elmore") ~ "Elmore",
                           str_detect(title, "Lake Rowan") ~ "Lake Rowan",
                           str_detect(title, "Coonabarabran") ~ "Coonabarabran",
                           str_detect(title, "Chiltern Valley") ~ "Chiltern Valley",
                           str_detect(title, "Kyabram") ~ "Kyabram",
                           str_detect(title, "Omeo Standard and Mining Gazette") ~ "Omeo",
                           str_detect(title, "Tatura") ~ "Tatura",
                           str_detect(title, "Sale") ~ "Sale",
                           str_detect(title, "Shepparton") ~ "Shepparton",
                           str_detect(title, "Yarrawonga") ~ "Yarrawonga",
                           str_detect(title, "Corryong") ~ "Corryong",
                           str_detect(title, "Korumburra") ~ "Korumburra",
                           str_detect(title, "Southern Cross News") ~ "Perth",
                           str_detect(title, "Korumburra") ~ "Korumburra",
                           str_detect(title, "National : 1980 - 2021") ~ "Canberra",
                           str_detect(title, "Essendon") ~ "Melbourne",
                           TRUE ~ as.character(place))) |>
  mutate(spatial = case_when(title=="The Australian Women's Weekly" ~ "NSW",
                             TRUE ~ as.character(spatial))) |>
  mutate(state = case_when(is.na(state) ~ spatial,
                           TRUE ~ as.character(state))) |>
  mutate(spatial = case_when(is.na(spatial) ~ state,
                           TRUE ~ as.character(spatial)))





# View(long_lat_current_list)

long_lat_current_list <- city_merge |>
  select(place, state, longitude, latitude) |>
  distinct() |>
  na.omit() |>
  group_by(place, state) |>
  mutate(number = row_number()) |>
  filter(number ==1)

View(cleaned_location_list)
cleaned_location_list <- city_merge |>
  select(-longitude, -latitude) |>
  left_join(long_lat_current_list, by = c("place", "state")) |>
  mutate(longitude = case_when(place == "Coonabarabran" ~ 149.2772,
                               place == "Yarrawonga" ~ 146.0003,
                               place == "Cobram" & state=="NSW" ~ 145.8117,
                               place == "Tatura" ~ 145.2260,
                               TRUE ~ as.numeric(longitude))) |>
  mutate(latitude = case_when(place == "Coonabarabran" ~ -31.2334,
                              place == "Yarrawonga" ~ -36.0261,
                              place == "Cobram" & state=="NSW" ~ -35.6574,
                              place == "Tatura" ~ 36.4401,
                               TRUE ~ as.numeric(latitude))) |>
  rename(trove_full_title = title) |>
  mutate(title = str_remove_all(trove_full_title, "\\s*\\([^\\)]+\\)")) |>
  mutate(merge_title = str_c(title, state, sep = " ")) |>
  group_by(merge_title) |>
  mutate(row_num = row_number()) |>
  filter(row_num==1) |>
  select(merge_title, place, longitude, latitude)


  check <- cleaned_location_list |>
  filter(is.na(longitude))

