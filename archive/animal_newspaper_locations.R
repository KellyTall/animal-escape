library(tidyverse)
library(janitor)
View(locations)

locations <- read_csv("trove-newspaper-titles-locations.csv") |>
  clean_names() |>
  rename(title = newspaper_title)

location_geo <- locations |>
  select(place, longitude, latitude) |>
  distinct()

View(location_geo)

locations_clean <- locations |>
  group_by(title_id) |>
  mutate(row_count = row_number()) |>
  filter(row_count==1) |>
  mutate(title = case_when(
    title == "Camperdown Chronicle (Vic. : 1877 - 1954)" ~ "Camperdown Chronicle (Vic. : 1875 - 1954)",
    title == "Western Star and Roma Advertiser (Toowoomba, Qld. : 1875 - 1948)" ~ "Western Star and Roma Advertiser (Qld. : 1875 - 1948)",
    title == "Upper Murray and Mitta Herald (Vic. : 1914 - 1918)" ~ "Upper Murray and Mitta Herald (Vic. : 1885 - 1955)",
    title == "Kalgoorlie Miner (WA : 1895 - 1950)" ~ "Kalgoorlie Miner (WA : 1895 - 1954)",
    title == "Queensland Times (Ipswich) (Qld. : 1909 - 1954)" ~ "Queensland Times (Ipswich, Qld. : 1909 - 1954)",
    title == "The Inverell Times (NSW : 1899 - 1954)" ~ "The Inverell Times (NSW : 1899 - 1907, 1909 - 1954)",
    title == "The Herald (Melbourne, Vic. : 1949 Supplement)" ~ "The Herald (Melbourne, Vic. : 1861 - 1954)",
    title == "Sunraysia Daily (Mildura, Vic. : 1920 - 1926)" ~ "Sunraysia Daily (Mildura, Vic. : 1920 - 1949)",
    title == "Benalla Standard (Vic. : 1901 - 1925)" ~ "Benalla Standard (Vic. : 1901 - 1940)",
    title == "The Daily News (Perth, WA : 1882 - 1950)" ~ "The Daily News (Perth, WA : 1882 - 1955)",
    title == "Bundarra and Tingha Advocate (NSW : 1900 - 1906)" ~ "Bundarra and Tingha Advocate (NSW : 1900 - 1907; 1910 - 1911; 1915 - 1916; 1918 - 1924; 1926 - 1929)",
    title == "Gnowangerup Star and Tambellup-Ongerup Gazette (WA : 1915 - 1942)" ~ "Gnowangerup Star and Tambellup-Ongerup Gazette (WA : 1915 - 1944)",
    title == "Leader (Orange, NSW : 1912 - 1922)" ~ "Leader (Orange, NSW : 1899 - 1945)",
    title == "Woodend Star (Vic. : 1915 - 1918)" ~"The Woodend Star (Vic. : 1888 - 1942)",
    title == "The Northam Advertiser (WA : 1895 - 1918; 1948 - 1954)" ~"The Northam Advertiser (WA : 1895 - 1955)",
    title == "Albany Advertiser (WA : 1897 - 1950)" ~ "The Albany Advertiser (WA : 1897 - 1954)",
    title == "Leader (Melbourne, Vic. : 1862 - 1918)" ~ "Leader (Melbourne, Vic. : 1862 - 1918, 1935)",
    title == "Richmond Guardian (Vic. : 1917 - 1918)" ~ "Richmond Guardian (Vic. : 1884 - 1885; 1894 - 1897; 1900 - 1930)",
    title == "The Daily Telegraph (Sydney, NSW : 1883 - 1923)" ~ "The Daily Telegraph (Sydney, NSW : 1883 - 1930)",
    title == "The Ballarat Courier (Vic. : 1869 - 1880; 1914 - 1918)" ~ "The Ballarat Courier (Vic. : 1869 - 1886; 1914 - 1918)",
    title == "Coolgardie Miner (WA : 1913 - 1917)" ~ "Coolgardie Miner (WA : 1894 - 1911)",
    TRUE ~ as.character(title)))


# location_geo <- locations |>
#   select(state, place, longitude, latitude) |>
#   distinct() |>
#   group_by(place) |>
#   mutate(row_count = row_number())
#
# View(location_geo)

merge_elephant <- elephant1 |>
  left_join(locations_clean, by="title") |>
  select(id, heading, title, trove_url,  date, spatial, place, latitude, longitude) |>
  mutate(place = case_when(title == "Daily Mirror (Sydney, NSW : 1941 - 1955)" ~ "Sydney",
                           title == "Sunday Mail (Brisbane, Qld. : 1926 - 1954)"~ "Brisbane",
                           title == "The Catholic Advocate (Brisbane, Qld. : 1911 - 1934; 1936 - 1938)"~ "Brisbane",
                           title == "The Sun News-Pictorial (Melbourne, Vic. : 1922 - 1954; 1956)"~ "Melbourne",
                           title == "The Daily Mail (Brisbane, Qld. : 1903; 1916 - 1926)"~ "Brisbane",
                           title == "The Australian Advertiser (Albany, WA : 1888 - 1897)"~ "Albany",
                           title == "The Albury Banner and Wodonga Express (NSW : 1860 - 1927; 1929 - 1931; 1933 - 1938)" ~ "Albury",
                           title == "Saturday Evening Express (Launceston, Tas. : 1924 - 1954)" ~ "Launceston",
                           title == "The Braidwood Express and People's Advocate (NSW : 1904 - 1907)" ~ "Braidwood",
                           title == "The Riverine Herald (Echuca, Vic. : Moama, NSW : 1869 - 1954; 1998 - 2002)" ~ "Echuca",
                           title == "The Groper (Perth, WA : 1930 - 1937)" ~ "Perth",
                           TRUE ~ as.character(place))) |>
  mutate(latitude = case_when(place == "Sydney" ~ -33.8732,
                              place == "Melbourne" ~ -37.82430,
                              place == "Brisbane" ~ -27.46785,
                              place == "Albany" ~ -35.01740,
                              place == "Albury" ~ -36.0735,
                              place == "Launceston" ~ -36.0735,
                              place == "Braidwood" ~ -35.4485,
                              place == "Echuca" ~ -36.15503,
                              place == "Perth" ~ -31.95184,
                               TRUE ~ as.numeric(latitude))) |>
  mutate(longitude = case_when(place == "Sydney" ~ 151.2096,
                               place == "Melbourne" ~ 144.9740,
                               place == "Brisbane" ~ 153.028,
                               place == "Albany" ~ -117.8838,
                               place == "Albury" ~ 146.9145,
                               place == "Launceston" ~ -36.0735,
                               place == "Braidwood" ~ 149.8012,
                               place == "Echuca" ~ 144.7618,
                               place == "Perth" ~ 115.8587,

                              TRUE ~ as.numeric(longitude)))





View(title_check)



merge_tiger <- tiger1 |>
  left_join(locations_clean, by="title") |>
  select(id, heading, title, trove_url,  date, spatial, place, latitude, longitude) |>
  mutate(place = case_when(title == "Daily Mirror (Sydney, NSW : 1941 - 1955)" ~ "Sydney",
                           title == "Sunday Mail (Brisbane, Qld. : 1926 - 1954)"~ "Brisbane",
                           title == "The Catholic Advocate (Brisbane, Qld. : 1911 - 1934; 1936 - 1938)"~ "Brisbane",
                           title == "The Sun News-Pictorial (Melbourne, Vic. : 1922 - 1954; 1956)"~ "Melbourne",
                           title == "The Daily Mail (Brisbane, Qld. : 1903; 1916 - 1926)"~ "Brisbane",
                           title == "The Australian Advertiser (Albany, WA : 1888 - 1897)"~ "Albany",
                           title == "The Albury Banner and Wodonga Express (NSW : 1860 - 1927; 1929 - 1931; 1933 - 1938)" ~ "Albury",
                           title == "Saturday Evening Express (Launceston, Tas. : 1924 - 1954)" ~ "Launceston",
                           title == "The Braidwood Express and People's Advocate (NSW : 1904 - 1907)" ~ "Braidwood",
                           title == "The Riverine Herald (Echuca, Vic. : Moama, NSW : 1869 - 1954; 1998 - 2002)" ~ "Echuca",
                           TRUE ~ as.character(place))) |>
  mutate(latitude = case_when(place == "Sydney" ~ -33.8732,
                              place == "Melbourne" ~ -37.82430,
                              place == "Brisbane" ~ -27.46785,
                              place == "Albany" ~ -35.01740,
                              place == "Albury" ~ -36.0735,
                              place == "Launceston" ~ -36.0735,
                              place == "Braidwood" ~ -35.4485,
                              place == "Echuca" ~ -36.15503,
                              TRUE ~ as.numeric(latitude))) |>
  mutate(longitude = case_when(place == "Sydney" ~ 151.2096,
                               place == "Melbourne" ~ 144.9740,
                               place == "Brisbane" ~ 153.028,
                               place == "Albany" ~ -117.8838,
                               place == "Albury" ~ 146.9145,
                               place == "Launceston" ~ -36.0735,
                               place == "Braidwood" ~ 149.8012,
                               place == "Echuca" ~ 144.7618,

                               TRUE ~ as.numeric(longitude)))

title_check <- merge_tiger |>
  filter(is.na(latitude))

View(title_check)

View(merge_tiger)

merge_lion <- lion1 |>
  right_join(locations, by=title)


merge_leopard <- leopard1 |>
  right_join(locations, by=title)
