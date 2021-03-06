# Data Import

# Justin Priest & Ryan Whitmore
# May 2022
# justin.priest@alaska.gov
# Ryan.Whitmore@dfo-mpo.gc.ca


### LIBRARIES ###
library(tidyverse)
library(lubridate)
library(here)
library(JTPfunc) # remotes::install_github("justinpriest/JTPfunc") (for function statweek()


### SET PARAMS ###
maxyear <- 2019
termareas <- c("101-85", "101-90", "101-95")



###### AK Troll CPUE ######

# Troll catches and CPUE for the Northern boundary area, districts 101/102 only, stat weeks 27-30 
troll_boundary <- read_csv(here::here("data/SEAK_Coho_TrollFPD_2000-2019.csv")) %>% 
  dplyr::select(Year:Chum) %>% 
  rename("Gear" = `Gear Code`,
         "SellDate" = `Sell Date`, 
         "StatWeek" = `Stat Week`, 
         "TrollArea" = `Troll Area`,
         "StatArea" = `Stat Area`,
         "DaysFished" = `FPD-Days Fished`,
         "HoursPerDay" = `Hours per Day`,
         "CohoCatch" = "Coho") %>% 
  filter(Gear == 15, Year <= maxyear) %>% # remove hand troll (power troll only)
  mutate(SellDate = as_date(as.POSIXct(SellDate, format = "%m/%d/%Y", tz = "US/Alaska")),
         District = as.factor(District),
         Effort_boatdays = DaysFished * HoursPerDay / 13,
         CohoCPUE = CohoCatch / Effort_boatdays) %>%
  dplyr::select(Year, SellDate, StatWeek, TrollArea, District, StatArea, CohoCatch, Effort_boatdays, CohoCPUE)



###### NBC Troll CPUE ######

nbctroll <- read_csv(here::here("data/_PRIVATE_NBC_Coho_TrollHarvest2001-2019.csv")) %>%
  janitor::clean_names() %>%
  rename("Can_week" = "stat_week",
         "Year" = "year") %>%
  mutate(harvestdate = as_date(as.POSIXct(fishing_date, format = "%m/%d/%Y", tz = "America/Vancouver")),
         StatWeek = statweek(harvestdate),
         cohototal = coho_kept + coho_reld) %>% 
  filter(mgmt_area == 101 | mgmt_area == 1) %>%
  dplyr::select(Year, mgmt_area, harvestdate, StatWeek, subarea, 
                fe_effort, fe_hrs_fished, fe_set_no, cohototal) %>%
  mutate(fe_hrs_fished = replace_na(fe_hrs_fished, 10)) %>% # assume blanks are a standard 10-hr day
  group_by(Year, StatWeek) %>%
  summarise(NBCtrollcount = sum(cohototal),
            NBCtrolleffort = sum(fe_hrs_fished)) %>%
  mutate(NBCtrollCPUE = NBCtrollcount / NBCtrolleffort) %>%
  dplyr::select(Year, StatWeek, NBCtrollcount, NBCtrollCPUE)




###### Tyee Test Fishery ######

# Old data thru 2019 only
# tyeecpue <- read_csv(here::here("data/NBC_Coho_TyeeTestFisheryDaily_1956-2019.csv")) %>%
#   pivot_longer(-DATE, "Year") %>%
#   rename("CohoCPUE" = "value") %>%
#   mutate(Date = as.Date(paste0(DATE, "-", Year), format = "%d-%B-%Y"),
#          Std_date = as.Date(paste0(DATE, "-", 2022), format = "%d-%B-%Y"),
#          Year = as.numeric(Year)) %>% 
#   dplyr::select(Year, Date, Std_date, CohoCPUE) %>%
#   arrange(Year) %>%
#   filter(Year <= maxyear)


tyeecpue <- read_csv(here::here("data/NBC_Coho_Tyee_Coho_1956-2021.csv"), skip = 1) %>%
  rename("canstatwk" = `STAT WEEK ('01)`) %>% 
  pivot_longer(-c("canstatwk", "MONTH", "DAY"),
               values_to = "CohoCPUE", names_to = "Year") %>%
  arrange(Year) %>%
  mutate(Year = as.integer(Year),
         Date = as.Date(paste0(Year, "-", MONTH, "-", DAY), 
                        format = "%Y-%m-%d"),
         Std_date = as.Date(paste0(2022, "-", MONTH, "-", DAY), 
                            format = "%Y-%m-%d")) %>%
  dplyr::select(Year, Date, Std_date, CohoCPUE) %>%
  arrange(Year) %>%
  filter(Year <= maxyear)



###### Nass River FW Catch #####

NassFW <- read_csv(here::here("data/NBC_Coho_NassRiverFWCatch_2000-2019.csv")) %>%
  dplyr::select(Date:`2019`) %>%
  pivot_longer(-Date, "Year") %>%
  rename("CohoCumCatch" = "value") %>%
  mutate(Std_date = as.Date(Date, format = "%d-%B-%Y") + years(2000), #add 2000 yrs to acct for two digit year
         Year = as.numeric(Year),
         Date = ymd(paste0(Year, "-",month(Std_date), "-", day(Std_date))),
         week = statweek(Date)) %>%
  arrange(Date) %>% group_by(Year) %>%
  mutate(CohoCatchDaily = CohoCumCatch - lag(CohoCumCatch, 1),
         CohoCatchDaily = replace(CohoCatchDaily, Date=="2019-06-16", 30), # manually replace value
         CohoCatchDaily = replace_na(CohoCatchDaily, 0)) %>%
  dplyr::select(Year, Date, Std_date, week, CohoCatchDaily, CohoCumCatch) %>%
  ungroup()



###### Nass River FW  CPUE #####

Nasscpue <- read_csv(here::here("data/NBC_Coho_NassRiverFWCPUE_1992-2021.csv"), skip = 1) %>% 
  pivot_longer(-Date, names_to = "Year", values_to = "nasscpue") %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         Year = as.numeric(Year),
         Date = as.Date(paste0(Year, "-", month(Date), "-", day(Date)), format = "%Y-%m-%d"),
         Std_date = as.Date(paste0(2022, "-", month(Date), "-", day(Date)), format = "%Y-%m-%d"),
         statweek = statweek(Date)) %>%
  dplyr::select(Year, Date, Std_date, statweek, nasscpue) %>% 
  filter(Year <= maxyear)





###### Toboggan Escapement #####

toboggan <- read_csv(here::here("data/NBC_Coho_Tobogganescapement.csv")) %>% 
  rename("Year" = `Return Year`,
         "wild_esc" = `Wild Escapement`) %>%
  dplyr::select(Year, wild_esc) %>%
  filter(Year <= maxyear)
# Note that there are issues with using this as the "wild" because
#  these could be F2 non-natural origin fish. 
# JTP May 2022: Deciding not to use Toboggan. Code is vestigial. 






# This is the 1981-2019 data which will be used to show the relationship between CPUE early vs late
# If time, replace this / rewrite code to only use post 2000 data. 
UStroll_cpue <- read_csv(here::here("data/SEAK_Coho_TrollFPD_1981-2019.csv"), 
                         guess_max = 84000) %>% #increased guess b/c of many blanks 
  rename("Gear" = `Gear Code`,
         "TripNum" = `Trip No`,
         "SellDate" = `Sell Date`, 
         "StatWeek" = `Stat Week`, 
         "TrollArea" = `Troll Area`,
         "StatArea" = `Stat Area`,
         "DaysFished" = `FPD-Days Fished`,
         "HoursPerDay" = `Hours per Day`,
         "CohoCatch" = "Coho") %>% 
  filter(Gear == 15, Year <= maxyear) %>% # remove hand troll (power troll only)
  mutate(SellDate = as_date(as.POSIXct(SellDate, format = "%m/%d/%Y", tz = "US/Alaska")),
         District = as.factor(District),
         Effort_boatdays = DaysFished * HoursPerDay / 13, # Effort is standardized to a 13 hour boat day
         CohoCPUE = CohoCatch / Effort_boatdays,
         TripNumber = paste0(Year, "-", TripNum)) %>% 
  dplyr::select(Year, TripNumber, SellDate, StatWeek, TrollArea, District, 
                StatArea, CohoCatch, Effort_boatdays, CohoCPUE)


usboundarytroll <- UStroll_cpue %>%
  filter(between(StatWeek, 27, 29),
         District == "101" | District == "102",
         !StatArea %in% termareas) # exclude terminal harvest areas

usboundtrollann <- usboundarytroll %>%
  group_by(Year) %>%
  summarise(USboundarytrollCPUE = mean(CohoCPUE)) 



wildabundance <- read_csv(here::here("data/SEAK_Coho_wildabundance1982-2021.csv"))
# To see how this file was created, run script "0_wildabundancecalc.R"






##### DATA WEEKLY #####
tyee_weekly <- tyeecpue %>%
  mutate(week = statweek(Date)) %>%
  group_by(Year, week) %>%
  summarise(Tyee_cpue = mean(CohoCPUE)) %>% ungroup()

NassFW_weekly <- NassFW %>% 
  group_by(Year, week) %>% 
  summarise(Nass_coho = sum(CohoCatchDaily)) %>% ungroup()

troll_USboundary <- troll_boundary %>%
  filter(between(StatWeek, 27, 30),
         !StatArea %in% termareas) %>% # exclude terminal harvest areas
  group_by(Year, StatWeek) %>%
  summarise(UStrollCPUE = mean(CohoCPUE))
#101-40, 101-41, 101-45,






### Create the overall index to compare ###   
indices <- tyee_weekly %>% 
  left_join(NassFW_weekly) %>% 
  left_join(troll_USboundary, by = c("Year" = "Year", "week" = "StatWeek")) %>%
  left_join(nbctroll, by = c("Year" = "Year", "week" = "StatWeek"))


indices_2000 <- indices %>% 
  filter(Year >= 2000, between(week, 27, 29)) %>% 
  group_by(Year) %>% # Need this so that NAs appear when you lead (can't use next week's info this week!)
  mutate(Tyee_cpue_lead1 = lead(Tyee_cpue, 1),
         Nass_coho_lead1 = lead(Nass_coho, 1),
         NBCtrollCPUElead1 = lead(NBCtrollCPUE, 1)) %>% 
  ungroup() 



