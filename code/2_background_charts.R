# Background figures for memo

# Justin Priest
# July 2022
# justin.priest@alaska.gov




library(adfgcolors) # remotes::install_github("justinpriest/adfgcolors")
library(patchwork)
source(here::here("code/1_data_import.R"))



##### US TROLL CPUE VS WILD ABUND #####
trollwildplot <- ggplot(wildabundance %>%
         filter(Year <= maxyear) %>%
         left_join(usboundtrollann), 
       aes(x=USboundarytrollCPUE, y = EstTotalWildAbund, fill = Year)) +
  #geom_text(aes(label = Year)) +
  geom_smooth(method = "lm", color = "gray40") +
  geom_point(size = 3, pch = 21, color = "black") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_adfg("glacier", discrete = FALSE) +
  labs(x = "Mean US Troll CPUE (Weeks 27-29)", y = "Est. SEAK Wild Coho Abundance", 
       title = "Relationship between US troll CPUE and wild coho abundance")  +
  theme_crisp(base_family = "Arial")
trollwildplot

lm(EstTotalWildAbund ~ USboundarytrollCPUE, 
   data = wildabundance %>%
     filter(Year <= maxyear) %>%
     left_join(usboundtrollann)) %>% summary()
#ggsave(trollwildplot, filename = "output/wildtroll.png", width = 6.5, height = 4, units = "in")



##### TYEE & NASS CUMULATIVE #####

tyeecumchart <- tyee_weekly %>% 
  filter(Year >= 2000) %>%
  group_by(Year) %>%
  mutate(cummsum = cumsum(replace_na(Tyee_cpue, 0)),
         perctotal = cummsum / max(cummsum)) %>% 
  ggplot(aes(x = week, y = perctotal, color = Year, group = Year)) +
  geom_line() +
  geom_vline(xintercept = 31, color = "red4", size = 1.5) +
  geom_vline(xintercept = 33, color = "red3", size = 1.5) +
  geom_vline(xintercept = 34, color = "red", size = 1.5) +
  scale_color_adfg(palette = "blues", discrete = FALSE) +
  scale_x_continuous(limits = c(23, 42)) +
  labs(title = "Cumulative Tyee CPUE by stat week", x = "Stat Week", y = "Cumulative Proportion of CPUE") +
  theme_crisp(base_family = "Arial") +
  annotate(geom = "curve", x = 27, y = 0.38, xend = 30.9, yend = 0.35, 
           curvature = 0.2, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 26, y = 0.45, size = 3, 
           label = "By week 31, no years have \nhad >50% of run passage", hjust = "center") +
  annotate(geom = "curve", x = 38, y = 0.26, xend = 33.1, yend = 0.21, 
           curvature = -0.2, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 39.5, y = 0.35, size = 3, 
           label = "By week 33, 11 of 20 years \nhave had >50% of run passage \n(20-84% of run passed)", hjust = "center") +
  annotate(geom = "curve", x = 38, y = 0.02, xend = 34.1, yend = -0.03, 
           curvature = -0.2, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 39, y = 0.1, size = 3, 
           label = "By week 34, 17 of 20 years \nhave had >50% of run passage \n(27-100% of run passed)", hjust = "center")
tyeecumchart

# ggsave(tyeecumchart, filename = here::here("output/cumulativetyee.png"), 
#        dpi = 600, width = 6.5, height = 4, units = "in")


# To source text in plot
tyee_weekly %>% 
  filter(Year >= 2000) %>%
  group_by(Year) %>%
  mutate(cummsum = cumsum(replace_na(Tyee_cpue, 0)),
         perctotal = cummsum / max(cummsum)) %>% filter(week == 34) %>% View


###############################




nasscumchart <- NassFW_weekly %>%
  full_join(crossing(data.frame(week = 24:42), 
                     data.frame(Year = 2000:2019))) %>% # add blank years to expand chart
  arrange(Year, week) %>%
  group_by(Year) %>%
  mutate(cummsum = cumsum(replace_na(Nass_coho, 0)),
         perctotal = cummsum / max(cummsum)) %>% 
  ggplot(aes(x = week, y = perctotal, color = Year, group = Year)) +
  geom_line() +
  geom_vline(xintercept = 31, color = "red4", size = 1.5) +
  geom_vline(xintercept = 33, color = "red3", size = 1.5) +
  geom_vline(xintercept = 34, color = "red", size = 1.5) +
  scale_color_adfg(palette = "blues", discrete = FALSE) +
  scale_x_continuous(limits = c(23, 42)) +
  labs(title = "Cumulative Nass Count by stat week", x = "Stat Week", y = "Cumulative Proportion of Count") +
  theme_crisp(base_family = "Arial") +
  annotate(geom = "curve", x = 27, y = 0.38, xend = 30.9, yend = 0.35, 
           curvature = 0.2, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 26, y = 0.45, size = 3, 
           label = "By week 31, no years have \nhad >50% of run passage", hjust = "center") +
  annotate(geom = "curve", x = 37, y = 0.28, xend = 33.1, yend = 0.2, 
           curvature = -0.2, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 39, y = 0.37, size = 3, 
           label = "By week 33, 11 of 20 years \nhave had >50% of run passage \n(19-77% of run passed)", hjust = "center") +
  annotate(geom = "curve", x = 37, y = 0.03, xend = 34.1, yend = -0.02, 
           curvature = -0.2, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 39, y = 0.12, size = 3, 
           label = "By week 34, 18 of 20 years \nhave had >50% of run passage \n(43-92% of run passed)", hjust = "center") 
nasscumchart

# ggsave(nasscumchart, filename = here::here("output/cumulativenass.png"), 
#        dpi = 600, width = 6.5, height = 4, units = "in")

  
NassFW_weekly %>% 
  group_by(Year) %>%
  mutate(cummsum = cumsum(replace_na(Nass_coho, 0)),
         perctotal = cummsum / max(cummsum)) %>% filter(week == 34) %>% View



####################################
###### US CPUE vs Can stocks #######

## US Troll vs Tyee
tyeetroll <- indices_2000 %>%
  ggplot(aes(x = UStrollCPUE, y = Tyee_cpue, fill = Year)) +
  geom_smooth(method = "lm", color = "black") + 
  geom_point(pch = 21, color="black", size = 3) +
  scale_fill_adfg("glacier", discrete = FALSE) +
  labs(y = "Tyee CPUE") +
  theme_crisp(base_family = "Arial")
tyeetroll + labs(title = "US Troll CPUE vs Tyee CPUE, Stat Weeks 27–29, same week") 
#ggsave(tyeetroll, filename = here::here("output/troll_tyee.png"), width = 6.5, height = 4, units = "in")


## US Troll vs Nass
nasstroll <- indices_2000 %>%
  ggplot(aes(x = UStrollCPUE, y = Nass_coho, fill = Year)) +
  geom_smooth(method = "lm", color = "black") + 
  geom_point(pch = 21, color="black", size = 3) +
  scale_fill_adfg("glacier", discrete = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "US Troll FPD CPUE", y = "Nass Catch") +
  theme_crisp(base_family = "Arial")
nasstroll + labs(title = "US Troll CPUE vs Nass catch, Stat Weeks 27–29, same week")
#ggsave(nasstroll, filename = here::here("output/troll_nass.png"), width = 6.5, height = 4, units = "in")


trollvsnasstyee <- (tyeetroll  + labs(x = "")) / (nasstroll + labs(x = "US Troll FPD CPUE") ) +
  plot_annotation(tag_levels = "a", tag_suffix = ")") & 
  theme(plot.tag.position  = c(0.14, 0.9))
trollvsnasstyee
#ggsave(trollvsnasstyee, filename = here::here("output/troll_nasstyee.png"), width = 6.5, height = 7, units = "in")




## US troll vs Tyee, lag
tyeelead <- indices_2000 %>%
  ggplot(aes(x = UStrollCPUE, y = Tyee_cpue_lead1, fill = Year)) +
  geom_smooth(method = "lm", color = "black") + 
  geom_point(pch = 21, color="black", size = 3) +
  scale_fill_adfg("glacier", discrete = FALSE) +
  labs(x = "US Troll FPD CPUE (Districts 101/102)", y = "Tyee CPUE, one week lead") +
  theme_crisp(base_family = "Arial") 
tyeelead + labs(title = "US Troll CPUE vs following week Tyee CPUE, Stat Weeks 27–29")
#ggsave(tyeelead, filename = here::here("output/troll_tyee_lead.png"), width = 6.5, height = 4, units = "in")

## US troll vs Nass, lag
nasslead <- indices_2000 %>%
  ggplot(aes(x = UStrollCPUE, y = Nass_coho_lead1, fill = Year)) +
  geom_smooth(method = "lm", color = "black") + 
  geom_point(pch = 21, color="black", size = 3) +
  scale_fill_adfg("glacier", discrete = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "US Troll FPD CPUE", y = "Nass Catch, one week lead") +
  theme_crisp(base_family = "Arial") 
nasslead + labs(title = "US Troll CPUE vs following week Nass catch, Stat Weeks 27–29")
#ggsave(nasslead, filename = here::here("output/troll_nass_lead.png"), width = 6.5, height = 4, units = "in")


trollvsnasstyeelead <- (tyeelead  + labs(x = "")) / (nasslead + labs(x = "US Troll FPD CPUE") ) +
  plot_annotation(tag_levels = "a", tag_suffix = ")") & 
  theme(plot.tag.position  = c(0.14, 0.9))
trollvsnasstyeelead
#ggsave(trollvsnasstyeelead, filename = here::here("output/troll_nasstyee_lead.png"), width = 6.5, height = 7, units = "in")



## US troll vs NBC troll
us_nbctroll <- indices_2000 %>%
  ggplot(aes(x = UStrollCPUE, y = NBCtrollCPUE , fill = Year)) +
  geom_smooth(method = "lm", color = "black") + 
  geom_point(pch = 21, color="black", size = 3) +
  scale_fill_adfg("glacier", discrete = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "US Troll FPD CPUE", y = "NBC Troll CPUE, same weeks", 
       title = "US Troll CPUE vs NBC Troll CPUE, Stat Weeks 27–29") +
  theme_crisp(base_family = "Arial")
us_nbctroll
#ggsave(us_nbctroll, filename = here::here("output/troll_uscan.png"), width = 6.5, height = 4, units = "in")

