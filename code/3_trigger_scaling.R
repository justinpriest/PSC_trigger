source(here::here("code/1_data_import.R"))



usboundarytroll <- UStroll_cpue %>%
  filter(between(StatWeek, 27, 30),
         District == "101" | District == "102",
         !StatArea %in% termareas) # exclude terminal harvest areas

usboundtrollann <- usboundarytroll %>%
  group_by(Year) %>%
  summarise(USboundarytrollCPUE = mean(CohoCPUE)) 


tyee_ann <- tyeecpue %>% 
  mutate(statweek = statweek(Date)) %>%
  filter(between(Year, 1988, maxyear),
         between(statweek, 27, 29)) %>%
  group_by(Year) %>%
  summarise(tyeecpue = sum(CohoCPUE)) 


# Set base troll CPUE to scale values to
# I used the 2000-2019 mean CPUE and scaled as a percent of this
# This was to keep it positive and meaningful
basetrollcpue <- usboundarytroll %>% filter(between(Year, 2000, 2019)) %>%
  summarise(mean(CohoCPUE)) %>%
  as.numeric()




basetyee <- tyee_ann %>% filter(between(Year, 2000, 2019)) %>%
  summarise(mean(tyeecpue)) %>%
  as.numeric()



usboundtrollann %>%
  mutate(scaletrollcpue = USboundarytrollCPUE / basetrollcpue) %>% 
  left_join(tyee_ann %>% 
              mutate(scaletyee = tyeecpue / basetyee)) %>%
  dplyr::select(Year, scaletrollcpue, scaletyee) %>%
  filter(Year >= 1990) %>% # start Tyee year
  mutate(crossindex_2575 = 2 * ((scaletrollcpue * 0.25) + (scaletyee * 0.75)),
         crossindex_equal = 2 * ((scaletrollcpue * 0.5) + (scaletyee * 0.5)),
         crossindex_7525 = 2 * ((scaletrollcpue * 0.75) + (scaletyee * 0.25))) %>%
  left_join(toboggan) %>%
  left_join(wildabundance %>% dplyr::select(Year, EstTotalWildAbund)) %>%
  cor()



# status quo (US troll only)   or   75% US troll, 25% Tyee
# 0.56 + 0.3 cor (0.86 tot)         0.46 + 0.21 (0.67 tot)






