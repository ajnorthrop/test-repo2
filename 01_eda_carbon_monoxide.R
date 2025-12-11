###########***********************
#### Code Description ####
# Author: Alex
# Date Created: 8/12/25
# Updated: 11/11/25
# Goal: Open data, clean, and condense as necessary
####**********************


###### Part 1: Cleaning and preparation #####

# Packages 

rm(list=ls())

source(paste0(here("code","00_setup", "0_read_libraries.R")))

# Load data 

pa_co <- read_xlsx(here("data","raw","pcc","Pediatric CO exposures 2020 to 2025.xlsx"), sheet = "Case Listing") %>% 
  janitor::clean_names() %>% 
  filter(year <2025) %>% # restrict 2025 cases so it's strictly 2020-2024 data
  filter(medical_outcome != "Confirmed nonexposure") # exclude non-cases

# Load Maps 
pa_map <- tigris::counties(state = "PA", class = "sf", cb = TRUE)
de_map <- tigris::counties(state = "DE", class = "sf", cb = TRUE)

zctas_pa <- tigris::zctas(state = c("PA"), class = "sf", year = 2010)
zctas_de <- tigris::zctas(state = c("DE"), class = "sf", year = 2010)

# Load census data 
# Obtained at https://data2.nhgis.org/main

census <- read_csv(here("data","raw","nhgis","pade_census_2019_2023.csv")) %>% 
  janitor::clean_names() %>% 
  mutate(pop_less_than_20 = (asnqe003 + asnqe004 + asnqe005 + asnqe006 + asnqe007 + 
                    asnqe027 + asnqe028 + asnqe029 + asnqe030 + asnqe031)) %>% 
  mutate(age_under_five = asnqe003 + asnqe027,
         age_five_to_nine = asnqe004 + asnqe028,
         age_ten_to_fourteen = asnqe005 + asnqe029,
         age_fifteen_to_nineteen = (asnqe006 + asnqe031 + asnqe007 + asnqe030),
         female = asnqe027 + asnqe028 + asnqe029 + asnqe030 + asnqe031,
         male = asnqe003 + asnqe004 + asnqe005 + asnqe006 + asnqe007) %>% 
  select(county, pop_less_than_20, age_under_five, age_five_to_nine, 
         age_ten_to_fourteen, age_fifteen_to_nineteen, male, female)

# Look at totals

sum_census <- census %>% 
  summarize(pop_less_than_20 = sum(pop_less_than_20), 
            age_under_five = sum(age_under_five), 
            age_five_to_nine = sum(age_five_to_nine), 
            age_ten_to_fourteen = sum(age_ten_to_fourteen), 
            age_fifteen_to_nineteen = sum(age_fifteen_to_nineteen),
            male = sum(male),
            female = sum(female))

# Fix New Castle error in county name convention d/t space in name 

census$county <- sub(" .*", "", census$county)

census <- census %>% 
  mutate(county = ifelse(county == "New", "New Castle", county)) 

# Combine maps 

fips_region <- rbind(pa_map, de_map) %>% 
  janitor::clean_names() %>% 
  select(name, stusps, geometry) 

zctas_region <- rbind(zctas_pa, zctas_de) %>% 
  janitor::clean_names() %>% 
  select(statefp10, zcta5ce10, zcta_geometry = geometry)

# Visualize missing data 

vis_miss(pa_co)

# Remove unnecessary columns and NA row 

pa_co <- pa_co %>% 
  select(-case_subcategory, -primary_center, -weight) %>% 
  filter(!is.na(state))

# Convert months to years for stratification 

pa_co <- pa_co %>% 
  mutate(age = case_when(
      age_unit == "Months" ~ age / 12,
      age_unit == "Years" ~ age,
      age_unit == "Days" ~ age / 365,
      age_unit == "Teen 13-19 yrs" ~ -999, # Age is excluded given the unspecified data 
      age_unit == "Unknown child (<=19 yrs)" ~ -999,
      age_unit == "<=5 yrs" ~ -999))

pa_co <- pa_co %>% 
  mutate(age_yr = case_when(
    age < 0 ~ NA,
    age < 1 & age > 0 ~ 0,
    age >= 1 & age < 2 ~ 1,
    age >= 2 & age < 3 ~ 2,
    TRUE ~ age)) %>% 
  mutate(age = case_when(
    age < 0 ~ "Unknown",
    age < 1 & age > 0 ~ "<1 year",
    age >= 1 & age < 5 ~ "1-4 years",
    age >= 4 & age < 10 ~ "5-9 years",
    age >= 10 & age < 15 ~ "10-14 years",
    age >= 15 & age < 20 ~ "15-19 years"))


###### Part 2: Data summarization #####

# Get a number of counts and incidence ratios for all subgroups 

pa_co %>% 
  mutate(age = factor(age, 
                   levels = c("<1 year", "1-4 years", "5-9 years", "10-14 years", "15-19 years"),
                   ordered = TRUE)) %>%
  group_by(age) %>% 
  summarise(n = n()) %>% 
  mutate(total = sum(n)) %>%
  mutate(percentage = round((n / total) * 100)) %>% 
  select(-total) %>% 
  mutate(population = case_when(
    age == "<1 year" ~ 739924/5, # Estimate for <1 year population based on 5 year-old data, supported by CDC WONDER data
    age == "1-4 years" ~ 591939,
    age == "5-9 years" ~ 787859,
    age == "10-14 years" ~ 840303,
    age == "15-19 years" ~ 902508
  )) %>%
  mutate(ir = (n / (population*5)) * 100000) %>% 
  mutate(ir = round(ir, digits = 1))

# And the same for the entire population
# 4.8 per 100,000 person-years

(nrow(pa_co) / (sum_census$pop_less_than_20 *5)) *  100000 

# Obtain breakdown by gender 

pa_co %>% 
  group_by(gender) %>% 
  summarise(n = n()) %>% 
  mutate(total = sum(n)) %>%
  mutate(percentage = round((n / total) * 100)) 

# Obtain breakdown by caller site (broadly) 

pa_co %>% 
  group_by(caller_site) %>% 
  summarise(n = n()) %>% 
  mutate(total = sum(n)) %>%
  mutate(percentage = round((n / total) * 100)) 

# Get a summary of medical outcomes
# 364 cases with no/minor effects, 
# 63 cases with moderate/major effects, and eight cases that were fatal.  

pa_co %>% 
  mutate(medical_outcome = case_when(
    medical_outcome == "Death" ~ "Death",
    medical_outcome == "Death, indirect report" ~ "Death",
    medical_outcome == "Not followed, minimal clinical effects possible (no more than minor effect possible) " ~ "Not followed",
    medical_outcome == "Not followed, judged as nontoxic exposure (clinical effects not expected)" ~ "Not followed",
    medical_outcome == "Unable to follow, judged as a potentially toxic exposure" ~ "Not followed",
    medical_outcome == "Not followed, minimal clinical effects possible (no more than minor effect possible)" ~ "Not followed",
    medical_outcome == "Unrelated effect, the exposure was probably not responsible for the effect(s)" ~ "Not followed",
    TRUE ~ medical_outcome
  )) %>%
  group_by(medical_outcome) %>% 
  summarise(n = n()) %>% 
  mutate(total = sum(n)) %>%
  mutate(percentage = round((n / total) * 100)) %>% 
  select(-total)

# Get the ages of death cases

pa_co %>% 
  filter(medical_outcome == "Death" | medical_outcome == "Death, indirect report") %>% 
  group_by(age_yr) %>% 
  summarise(n = n())

# Get residential data % 
# NB: Residential includes "Own residence" and "Other residence"

pa_co %>% 
  mutate(exposure_site = case_when(
    exposure_site == "Other" ~ "Other/Unknown",
    exposure_site == "Unknown" ~ "Other/Unknown",
     exposure_site == "Own residence" ~ "Residential",
     exposure_site == "Other residence" ~ "Residential",
     TRUE ~ exposure_site
   )) %>% 
  group_by(exposure_site) %>% 
  summarise(n = n()) %>% 
  mutate(total = sum(n)) %>%
  mutate(percentage = round((n / total) * 100)) %>% 
  select(-total)

# Get the year breakdown of cases 

pa_co %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  mutate(total = sum(n)) %>%
  mutate(percentage = round((n / total) * 100)) 

# Get the seasonal breakdown of cases 

pa_co %>% 
  mutate(month = lubridate::month(as.Date(start_date, format = "%m/%d/%Y"))) %>%
  mutate(season = case_when(
    month %in% c(12, 1, 2) ~ "Winter",
    month %in% c(3, 4, 5) ~ "Spring",
    month %in% c(6, 7, 8) ~ "Summer",
    month %in% c(9, 10, 11) ~ "Fall"
  )) %>%
  group_by(season) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n)) %>%
  mutate(percentage = round((n / total) * 100))

# Get the cases with hyperbaric therapy

pa_co %>% 
  mutate(has_hyperbaric = if_else(grepl("hyperbaric", therapy, ignore.case = TRUE), "yes", "no")) %>% 
  group_by(has_hyperbaric) %>% 
  summarise(n = n()) %>% 
  mutate(total = sum(n)) %>%
  mutate(percentage = round((n / total) * 100)) 

# Look at fire exposure among classes 

pa_co %>% 
  mutate(fire_exposure = if_else(grepl("House Fire", substance, ignore.case = TRUE), "yes", "no")) %>% 
  mutate(fire_exposure = if_else(grepl("Cyanides", substance, ignore.case = TRUE), "yes", fire_exposure)) %>% 
  group_by(fire_exposure, medical_outcome) %>% 
  summarise(n = n()) %>%
  ungroup() %>% 
  group_by(medical_outcome) %>% mutate(total = sum(n)) 

# Combine spatial data 

pa_co <- pa_co %>% 
  left_join(fips_region, by = c("county" = "name")) %>%
  left_join(zctas_region, by = c("zip_code_5" = "zcta5ce10"))

# Review missingness 
vis_miss(pa_co)

# Check for county and ZCTA hotspots for CO poisoning 

county_co_cases <- pa_co %>% 
  group_by(county, geometry) %>%
  summarise(n = n()) %>% 
  left_join(census, by = c("county")) %>% 
  mutate(rate = (n / (pop_less_than_20 * 5)) * 100000) %>% 
  mutate(rate = ifelse(is.na(rate), 0, rate)) %>%
  mutate(rate = case_when(
    rate == 0 ~ "0",
    rate > 0 & rate <= 2 ~ "0.1 - 2.0",
    rate > 2.0 & rate <= 4.0 ~ "2.1 - 4.0",
    rate > 4.0 & rate <= 6.0 ~ "4.1 - 6.0",
    rate > 6.0 ~ "+6.1")) %>% 
  right_join(fips_region, by = c("county" = "name", "geometry")) %>% 
  mutate(rate = as.character(rate)) %>% 
  mutate(rate = ifelse(is.na(rate), "0", rate)) %>% 
  mutate(rate = factor(rate, levels = c("0", "0.1 - 2.0", "2.1 - 4.0", "4.1 - 6.0", "+6.1"))) 
             
zcta_co_cases <- pa_co %>% 
  group_by(zip_code_5, zcta_geometry) %>%
  summarise(n = n()) 

###### Part 3: Create visualizations ###### 

a <- ggplot() +
  geom_sf(data = fips_region, color = "black", aes(geometry = geometry)) +
  geom_sf(data = county_co_cases, aes(fill = n, geometry = geometry), color = "black", alpha = 0.7) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "CO Poisoning Cases by County in PA and DE",
       fill = "Number of Cases") +
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  coord_sf() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(), 
        panel.grid = element_blank(),
        panel.background = element_blank()) + 
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) 

b <- ggplot() +
  geom_sf(data = fips_region, color = "black", aes(geometry = geometry)) +
  geom_sf(data = county_co_cases, aes(fill = rate, geometry = geometry), color = "black", alpha = 0.7) +
  scale_fill_viridis_d(option = "C") +
  labs(title = "Reported Carbon Monoxide Cases by County in PA and DE",
       fill = "Cases per 100,000 person-years (0-19 year old)",
       caption = "Data Source: Pennsylvania Poison Control Centers | US ACS 2019-2023") +
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  coord_sf() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(), 
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        text = element_text(family = "Times New Roman"),
        plot.caption = element_text(size = 8, hjust = -0.5, family = "Times New Roman"))

# Create a time series plot 

pa_co_time <- pa_co %>% 
  mutate(start_date = as.Date(start_date, format = "%m/%d/%Y")) %>%   mutate(
    month_year = format(floor_date(start_date, "month"), "%b %Y")
  ) %>% 
  group_by(month_year) %>%
  summarize(cases = n()) %>% 
  mutate(
    month_date = my(month_year)  # converts "Aug 2025" → 2025-08-01
  ) %>% 
  separate_wider_delim(month_year, delim = " ", names = c("month", "year")) %>% 
  mutate(month = factor(month, 
                       levels = month.abb)) 

# Add to demonstrate heating season in PA/DE 

shade_ranges <- data.frame(
  xmin = c("Jan", "Oct", "Jan", "Oct"),
  xmax = c("Apr", "Dec", "Apr", "Dec"),
  year = c(2022, 2022, 2023, 2023)
) %>%
  mutate(
    xmin = factor(xmin, levels = month.abb, ordered = TRUE),
    xmax = factor(xmax, levels = month.abb, ordered = TRUE)
  )

# Monthly variation graph 

monthly_variation <- pa_co_time %>% 
  ggplot(aes(x = month, y = cases, group = year, color = year)) +
  geom_line() +  # points for each month
  geom_rect(
    data = shade_ranges,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf,
        fill = "October - April"),
    inherit.aes = FALSE,
    alpha = 0.1,
    linetype = "dotted", color = "coral4"
  ) +
  scale_color_viridis_d(option = "C", begin = 0.2, end = 0.8, name = "Year") +  # color palette for years+ 
  theme_minimal() + 
  labs(title = "Monthly Carbon Monoxide Poisoning Cases in PA and DE",
       x = "Month",
       y = "Number of Cases",
       ) + 
  scale_fill_manual(
    name = "Heating Season",  # Legend title for shaded area (blank here)
    values = c("October - April" = "goldenrod")
  ) + 
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# Create age histogram

pa_co %>% 
  filter(!is.na(age_yr)) %>% 
  ggplot() +
  geom_histogram(aes(x = age_yr), binwidth = 1, fill = "darkblue", alpha = 0.4, color = "black"
                 ) + 
  theme_minimal() + 
  labs(title = "Carbon Monoxide Poisoning Cases by Age in PA and DE",
       x = "Age",
       y = "Number of Cases",
  ) +
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5, family = "Times New Roman"),
        axis.title.x = element_text(size = 12, face = "bold", family = "Times New Roman"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) 

# What are the temporal trends of CO poisoning in PA and DE?

pa_co_series <- pa_co %>% 
  group_by(start_date) %>%
  summarize(cases = n()) %>% 
  mutate(date = as.Date(start_date, format = "%m/%d/%Y")) %>% 
  select(date, cases) 

date_range <- seq(as.Date("2020-01-01"), as.Date("2024-12-31"), by = "day")
yr_num <- length(unique(lubridate::year(pa_co_series$date)))


pa_co_series <- pa_co_series %>%
  complete(date = date_range) %>% 
  replace_na(list(cases = 0)) 

# Plot time 

ggplot(pa_co_series, aes(date, cases))+ 
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(color = "mediumpurple2", method = lm, formula = y ~ ns(x, df= 4*yr_num)) +
  geom_smooth(color = "green", method = lm, formula = y ~ ns(x, df= 1*yr_num)) + 
  geom_smooth(color = "orange", method = lm, formula = y ~ ns(x))



