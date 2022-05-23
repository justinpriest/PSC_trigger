# Calculation & creation of the wild abundance index which takes a lot of parts
# This script creates a csv file at the end and can be re-updated with more data


### LIBRARIES ###
library(tidyverse)
library(lubridate)
library(here)
library(JTPfunc) # remotes::install_github("justinpriest/JTPfunc") (for function statweek()


SEAK_escape <- read_csv(here::here("../coho_data/data/SEAK_Coho_Escapement_1972-2021.csv")) 


indic_harvest <- read_csv(here::here("../coho_data/data/SEAK_Coho_HarvestSourcesForIndicatorStocks1980-2021.csv")) %>%
  filter(Year >= 1980) %>% # theres a few early estimates for Berners, exclude these
  pivot_wider(names_from= Fishery_Type, values_from = Coho_Harvest_Count) %>%
  rename("AK_Gillnet" = `AK_Gillnet (Drift & Set)`) %>% 
  mutate(Other = AK_Gillnet + AK_Other + AK_Seine + AK_Sport + CAN_AllFisheries) %>%
  dplyr::select(-c(AK_Gillnet, AK_Other, AK_Seine, AK_Sport, CAN_AllFisheries)) 

indic_totalrun <- indic_harvest %>%
  left_join(SEAK_escape %>% 
              filter(River %in% c("Auke Creek", "Berners River", "Ford Arm Lake", "Hugh Smith Lake")) %>%
              dplyr::select(c(Year, River, Escapement_Count)),
            by = c("Year" = "Year", "River" = "River")) %>%
  pivot_longer(cols = c(AK_Troll, Other, Escapement_Count), names_to = "Fishery", values_to = "Count") %>%
  #left_join(SEAK_escgoals, by = c("River" = "System")) %>%
  mutate(Fishery = factor(Fishery, levels = c("AK_Troll", "Other", "Escapement_Count")),
         Fishery = recode(Fishery, "AK_Troll" = "Alaska Troll", "Other" = "Other Harvest",
                          "Escapement_Count" = "Escapement"))



##### Exploitation Rate #####
trollindex <- indic_totalrun %>%
  filter( !(River =="Berners River" & Year < 1989)) %>% # These years are incorrect, exclude
  group_by(Year, River) %>%
  mutate(freq = Count / sum(Count)) %>%
  ungroup() %>%
  filter(Fishery == "Alaska Troll") %>%
  dplyr::select(-Count) %>%
  rename("index" = "freq")

trollindex_imputed <- impute_global(trollindex, Year_column = "Year", 
              StreamName_column = "River", count_column = "index")



trollindex <- trollindex_imputed %>%
  dplyr::select(-imputed) %>%
  pivot_wider(names_from = stream_name , values_from = total_count) %>%
  #calculate troll index from Leon's weighting (different weightings pre/post 1989)
  mutate(trollindex = ifelse(year < 1989, (`Auke Creek` * 0.4) + (`Ford Arm Lake` * 0.2) + 
                               (`Hugh Smith Lake` * 0.4), 
                             (`Berners River` * 0.2) + (`Auke Creek` * 0.2) + (`Ford Arm Lake` * 0.2) + 
                               (`Hugh Smith Lake` * 0.4))) %>%
  rename("Year" = "year") # to maintain consistency with other dataframes

rm(trollindex_imputed)


trollharvest <- read_csv(here::here("data/SEAK_Coho_TrollHarvest_Wildvshatchery.csv")) %>% 
  dplyr::select(Year:`Other hatchery`) %>%
  mutate(Hatchery = `Alaska hatchery` + `Other hatchery`) %>%
  dplyr::select(-`Alaska hatchery`, -`Other hatchery`) %>%
  pivot_longer(cols = c(`Wild contribution`, "Hatchery"), names_to = "Source", values_to = "Harvest")



##### Wild Abundance #####
wildabundance <- trollharvest %>% 
  filter(Source == "Wild contribution", Year >= 1982) %>%
  rename("Wildharvest" = "Harvest") %>%
  dplyr::select(-Fishery, -Source) %>%
  left_join(trollindex %>% dplyr::select(Year, trollindex)) %>%
  mutate(EstTotalWildAbund = Wildharvest / trollindex) 


#write_csv(wildabundance, "data/SEAK_Coho_wildabundance1982-2021.csv")
