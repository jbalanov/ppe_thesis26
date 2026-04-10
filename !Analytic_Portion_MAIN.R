library(tidyverse)
library(tidycensus)
#built in to tidycensus package
data(fips_codes)
library(fixest)
library(iplots)
library(car)
library(modelsummary)
library(flextable)


#directory list
files <- list.files(getwd(), pattern = "\\.csv$", full.names = TRUE)

#read and bind csvs for all states
df_all <- do.call(rbind, lapply(files, read.csv, stringsAsFactors = FALSE)) %>%
  filter(!str_detect(county, "total|Total")) %>%
  mutate(month = str_remove(month, "2023")) 

df_all$county <- as.character(df_all$county)
fips_codes$county <- as.character(fips_codes$county)

states_clean <- df_all %>%
  #there were some weird whitespaces in some counties/parishes
  mutate(county = trimws(str_to_title(str_remove_all(county, "[0-9]")), which = "both"),
         county = case_when(state == "LA" ~ str_replace(county, "Par", "Parish"),
                            state == "VA" & str_detect(county, "County") ~ trimws(str_remove(county, "Locality:"), which = "both"),
                            state == "VA" & str_detect(county, "City") ~ 
                              paste0(str_remove(trimws(str_remove(county, "Locality:"), which = "both"), "City"), "city"),
                            .default = paste(county, "County")),
         county = trimws(gsub("\u00A0$", "", county), which = "right"),
         county = gsub("\\.\\s+", ". ", county),
         county = gsub("\u00A0", " ", county),
         county = trimws(str_remove(county, "^\\s*-\\s*"), which = "left"),
         county = str_remove(county, "\\*"),
         month = str_remove(month, "-23")) %>%
  #fix all the one's that are just written differently 
  mutate(
    county = case_when(
      county == "Dekalb County" ~ "DeKalb County",
      county == "St_clair County" ~ "St. Clair County",
      county == "Desoto County" ~ "DeSoto County",
      county == "O'brien County" ~ "O'Brien County",
      county == "Mcpherson County" ~ "McPherson County",
      county == "Mccracken County" ~ "McCracken County",
      county == "Mccreary County" ~ "McCreary County",
      county == "Mclean County" ~ "McLean County",
      county == "Lasalle Parish" ~ "La Salle Parish",
      county == "St. John The Baptist Parish" ~ "St. John the Baptist Parish",
      county == "Baltimore City County" ~ "Baltimore city",
      county == "Baltimore Co. County" ~ "Baltimore County",
      county == "Pr. George's County" ~ "Prince George's County",
      county == "Jeff Davis County" ~ "Jefferson Davis County",
      county == "Mcclain County" ~ "McClain County",
      county == "Mccurtain County" ~ "McCurtain County",
      county == "Mcintosh County" ~ "McIntosh County",
      county == "Mccook County" ~ "McCook County",
      county == "Mcdowell County" ~ "McDowell County",
      county == "King & Queen County" ~ "King and Queen County",
      county == "Isle Of Wight County" ~ "Isle of Wight County",
      .default = county),
    county = if_else(state == "OK" & county == "Leflore County",
                     "Le Flore County", county)) %>%
  #join to built in fips code dataset
  left_join(fips_codes, by = join_by("county" == "county", "state" == "state")) %>%
  #get full code
  mutate(fips_code = paste0(state_code, county_code)) %>%
  select(county, state, count, month, year, fips_code) %>%
  filter(!county %in% c("Grand Tota County", "Totals County", "County County",
                        "West Virginia County", "Parish  Fir F")) %>%
  filter(!is.na(state)) %>%
  #extra florida months
  filter(!is.na(count))

#this list allows for quick recoding of different month formats
month_lookup <- c(Jan = 1, January = 1, Feb = 2, February = 2, Mar = 3, March = 3,
                  Apr = 4, April = 4, May = 5, Jun = 6, June = 6, Jul = 7, July = 7,
                  Aug = 8, August = 8, Sep = 9, Sept = 9, September = 9,
                  Oct = 10, October = 10, Nov = 11, November = 11, Dec = 12, December = 12) 

states_clean <- states_clean %>%
  mutate(month_clean = ifelse(month %in% names(month_lookup),month_lookup[month],
                              as.integer(month)),  #fix numeric months
         date = lubridate::make_date(year = as.integer(year), month = month_clean, day = 1),
         count = as.numeric(str_remove(count, ",")))

#checks for self
nonmatch <- states_clean %>% filter(fips_code == "NANA") %>% select(county, state) %>% unique
nas <- states_clean %>% filter(is.na(count))
#states_clean %>% write.csv("C:\\Users\\Jackie\\Dropbox\\PPE Thesis Jackie\\full_clean_new.csv")

###combine all###
ccc_all <- read.csv("ccc_all.csv")
#not aggregated by month-state
ccc_nonagg <- read.csv("ccc_nonagg.csv")
census <- read.csv("censuspops.csv") 

ccc_all <- ccc_all %>% mutate(date = ym(paste0(year, "-", month)))
ccc_nonagg <- ccc_nonagg %>% mutate(date = ym(paste0(year, "-", month)),
                                    fips_code = as.factor(fips_code)) %>% select(-year, -month)

#join dataframes
full_data <- states_clean  %>%
  mutate(fips_code = as.numeric(fips_code)) %>%
  left_join(ccc_all, by = c("fips_code", "date")) %>%
  left_join(census, by = join_by("fips_code" == "GEOID")) %>%
  mutate(protests = ifelse(is.na(n), 0, n)) %>%
  select(-n)
 
#make new variables
full_data <- full_data %>%
  mutate(fips_code = as.factor(fips_code)) %>%
  group_by(fips_code) %>%         
  arrange(date, .by_group = TRUE) %>%  
  mutate(delta_regs = count - dplyr::lag(count),
         pct_change = (count - dplyr::lag(count))/dplyr::lag(count) * 100,
         delta_pct = delta_regs/total_pop * 1000,
         mo_days = days_in_month(date),
         protest_daily = protests/mo_days)

full_data_nonagg <- full_data %>%
  left_join(ccc_nonagg, by = c("fips_code", "date"))

####
#NOTE: I remove Idaho even though I have it in the dataset 
#as it has really inconsistent monthly reporting

#mian model
fe_leads_lags2 <- feols(
  delta_pct ~ l(protest_daily, -3:3) |
    fips_code + date^state,
  data = full_data %>% filter(state != "ID"),
  cluster = ~ fips_code,
  panel.id = ~ fips_code + date
)
summary(fe_leads_lags2)

#partial sample model
fe_leads_lags2nonvi <- feols(
  delta_pct ~ l(protest_daily, -3:3) |
    fips_code + date^state,
  data = full_data_nv %>% filter(state != "ID"),
  cluster = ~ fips_code,
  panel.id = ~ fips_code + date
)
summary(fe_leads_lags2nonvi)

####summary stats 

#Table 5

#bind samples
sumstats <- bind_rows(
  mutate(full_data, sample = "Full (1-6)"),
  mutate(full_data_nv, sample = "Only No Violence (1-3)"),
)

bothsum <- datasummary(
  sample * (delta_pct + protest_daily + protests + count + total_pop) ~
    Mean + SD + Min + Max + N,
  data = sumstats %>% filter(state != "ID"),  
  #title = "Descriptive Statistics",
  output = "flextable"
)

#save_as_docx(bothsum, path = "summarystatsmain.docx")


#Table 6

#bring samples together
#using nonaggregated versions of dataframes
sumstats_nonagg <- bind_rows(
  mutate(full_data_nonagg, sample = "Full (1-6)"),
  mutate(full_data_nonagg_nv, sample = "Only No Violence (1-3)"),
)


bothsum_nonagg <- datasummary(
  sample * score ~ Mean + SD + Min + Max + N,
  data = sumstats_nonagg %>% filter(state != "ID"),  
  #title = "Descriptive Statistics",
  output = "flextable"
)
#bothsum_nonagg

#save_as_docx(bothsum_nonagg, path = "summarystatsindprotest.docx")

#descriptive names
full_data_tbl <- full_data %>%
  rename(`Registration change per 1k residents` = delta_pct,
    `Protest Saliency (Monthly)` = protests,
    Registrations = count,
    `County Population`= total_pop,
    `Daily Protest Avg.`= protest_daily
  )

#state by state stats (appendix)
statessum <- datasummary(state * (`Registration change per 1k residents` + 
                                    Registrations + `Daily Protest Avg.` + 
                                    `Protest Saliency (Monthly)` +
                                    `County Population`) ~ Mean + SD + Min + Max + N,
  data = full_data_tbl %>% filter(state != "ID"),
  output = "flextable")

#save_as_docx(statessum, path = "statessum.docx")

#shortened labels for coef plot
dict_lags <- c("l(protest_daily, -3)" = "t+3",
               "l(protest_daily, -2)" = "t+2",
               "l(protest_daily, -1)" = "t+1",
               "l(protest_daily, 0)"  = "t",
               "l(protest_daily, 1)"  = "t-1",
               "l(protest_daily, 2)"  = "t-2",
               "l(protest_daily, 3)"  = "t-3")
#Figure 6
coefplot(fe_leads_lags2, dict = dict_lags, 
  xlab = "Months Relative to Protest Activities",
  ylab = "Effect on Change in Voter Registration (% of Population)",
  main = "Distributed-Lag Effects of Protests")

#Table 7 
sum <- modelsummary(list("All" = fe_leads_lags2, 
                         "No Violence Only" = fe_leads_lags2nonvi),  
  output = "flextable",
  stars = F,
  statistic = c("std.error"),
  gof_omit = "Adj|Within|FE|Errors|Obs|AIC|BIC"
)

save_as_docx(sum, path = "results_table.docx")
esttable(fe_leads_lags)


#Figure 5
county_month_saliency <- full_data_nonagg %>%
  filter( state != "ID") %>%
  group_by(fips_code, state, date) %>%
  summarise(avg_saliency = mean(score, na.rm = TRUE),
            delta_pct = first(delta_pct)) %>%
  ungroup()

saliency_bins <- county_month_saliency %>%
  filter(!is.na(avg_saliency)) %>%
  mutate(saliency_bin = round(avg_saliency)) %>%
  group_by(saliency_bin) %>%
  summarise(mean_delta_pct = mean(delta_pct, na.rm = TRUE),
            sd_delta_pct   = sd(delta_pct, na.rm = TRUE),
            n_county_months = n())


ggplot(saliency_bins, aes(x = as.factor(saliency_bin), y = mean_delta_pct)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_delta_pct - sd_delta_pct / sqrt(n_county_months),
      ymax = mean_delta_pct + sd_delta_pct / sqrt(n_county_months)), 
      width = 0.2) +
  labs(x = "Protest Saliency Score",
       y = "Mean Monthly Change in Voter Registration (%)",
       title = "Descriptive Relationship Between Protest Saliency and Registration Changes") +
  theme_light()

