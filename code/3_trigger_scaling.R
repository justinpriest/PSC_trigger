source(here::here("code/1_data_import.R"))
# test change





tyee_ann <- tyeecpue %>% 
  mutate(statweek = statweek(Date)) %>%
  filter(between(Year, 1988, maxyear),
         between(statweek, 27, 29)) %>%
  group_by(Year) %>%
  summarise(tyeecpue = sum(CohoCPUE)) 

# Create annual Nass index of CPUE
nass_ann <- Nasscpue %>% 
  group_by(Year) %>% 
  summarise(nasscpue_ann = sum(nasscpue, na.rm = TRUE)) 





# Set base troll CPUE to scale values to
# I used the 2000-2019 mean CPUE and scaled as a percent of this
# This was to keep it positive and meaningful
basetrollcpue <- usboundarytroll %>% filter(between(Year, 2000, 2019)) %>%
  summarise(mean(CohoCPUE)) %>%
  as.numeric()

basetyee <- tyee_ann %>% filter(between(Year, 2000, 2019)) %>%
  summarise(mean(tyeecpue)) %>%
  as.numeric()



replacements <- usboundtrollann %>%
  mutate(scaletrollcpue = USboundarytrollCPUE / basetrollcpue) %>% 
  left_join(tyee_ann %>% 
              mutate(scaletyee = tyeecpue / basetyee)) %>%
  dplyr::select(Year, scaletrollcpue, scaletyee) %>%
  filter(Year >= 1990) %>% # start Tyee year
  mutate(crossindex_2575 = 2 * ((scaletrollcpue * 0.25) + (scaletyee * 0.75)),
         crossindex_equal = 2 * ((scaletrollcpue * 0.5) + (scaletyee * 0.5)),
         crossindex_7525 = 2 * ((scaletrollcpue * 0.75) + (scaletyee * 0.25))) %>%
  left_join(nass_ann) %>%
  left_join(wildabundance %>% dplyr::select(Year, EstTotalWildAbund)) %>%
  filter(Year >= 1992) # No data for Nass in 1990 & 1992 = NAs. cor will fail

replacements %>%
  cor()



# status quo (US troll only)   or   75% US troll, 25% Tyee
# 0.26 + 0.28 cor (0.537 tot)         0.35 + 0.25 (0.65 tot)
#    mean=0.27                           mean=0.32




ggplot(replacements, aes(x = scaletrollcpue, y = scaletyee)) +
  geom_point()

ggplot(replacements, aes(x = scaletrollcpue, y = EstTotalWildAbund)) +
  geom_point()

ggplot(replacements, aes(x = scaletyee, y = nasscpue_ann, color = Year)) +
  geom_point()

ggplot(replacements, aes(x = scaletyee, y = EstTotalWildAbund, color = Year)) +
  geom_point()



lm(scaletrollcpue ~ scaletyee, data = replacements) %>% summary()
lm(nasscpue_ann ~ scaletyee, data = replacements) %>% summary()
lm(EstTotalWildAbund ~ scaletrollcpue, data = replacements) %>% summary()


usboundtrollann %>%
  mutate(scaletrollcpue = USboundarytrollCPUE / basetrollcpue) %>% 
  left_join(tyee_ann %>% 
              mutate(scaletyee = tyeecpue / basetyee)) 



plot(EstTotalWildAbund ~ scaletrollcpue, data = tempdf)



tempdf <- wildabundance %>% 
  left_join(usboundtrollann) 


lm(EstTotalWildAbund ~ USboundarytrollCPUE, 
   data = tempdf %>% filter(Year <= 1998)) %>% summary()



tempdf %>% 
  ggplot(aes(x = USboundarytrollCPUE, y = EstTotalWildAbund, color = Year, label = Year)) +
  geom_point() + 
  geom_text(color = "black") +
  scale_color_adfg(palette = "glacier", discrete = FALSE) +
  geom_vline(xintercept = 15) +
  geom_vline(xintercept = 22)
