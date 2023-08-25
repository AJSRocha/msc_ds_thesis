library(ggplot2)
library(dplyr)
library(tidyr)
library(wesanderson)


# só de OCC
global_landings = read.csv('data/global_production_quantity.csv')

global_landings = global_landings[,names(global_landings)[!grepl('Flag', names(global_landings))]]

gl =
global_landings %>% 
  pivot_longer(c(-Country.Name.En, -Continent.Name.En, -FAO.major.fishing.area.Name.En, -Ocean.Name.En, -Unit.Name),
               names_to = 'Year',
               values_to = 'tons')

fig =
gl %>% 
  group_by(Continent.Name.En, Year) %>% 
  summarise(landings = sum(tons, na.rm = T)) %>% 
  mutate(Year = substr(Year,2,5) %>% as.numeric) %>%  
  ggplot() +
  # geom_line(aes(x = Year,
  #               y = landings,
  #               color = Continent.Name.En,
  #               group = Continent.Name.En)) + 
  geom_bar(aes(x = Year,
               y = landings,
               fill = Continent.Name.En),
           stat = 'identity',
           color = 'black')+
  scale_fill_manual(values = wes_palette('GrandBudapest2')) + 
  scale_x_continuous(breaks = seq(1950, 2021, 5)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none') + 
  facet_wrap(Continent.Name.En ~., ncol = 2, scales = 'free_y') + 
  labs(fill = '',
       y = 'tons')


ggsave(fig, dpi = 300, width = 20, height = 20, units = 'cm', filename = 'plots/global_landings.png')


# todos os polvos
global_landings_all = read.csv('data/global_production_quantity_comb.csv')

global_landings_all = global_landings_all[,names(global_landings_all)[!grepl('Flag', names(global_landings_all))]]

gl_all =
  global_landings_all %>% 
  pivot_longer(c(-Country.Name.En, -Continent.Name.En, -FAO.major.fishing.area.Name.En, -Ocean.Name.En, -Unit.Name),
               names_to = 'Year',
               values_to = 'tons') %>% 
  mutate(Continent.Name.En = case_when(Continent.Name.En == '' ~ 'USSR + others',
                                      T ~ Continent.Name.En))

unique(gl_all$Continent.Name.En)

fig_all =
  gl_all %>% 
  group_by(Continent.Name.En, Year) %>% 
  summarise(landings = sum(tons, na.rm = T)) %>% 
  mutate(Year = substr(Year,2,5) %>% as.numeric) %>%  
  ggplot() +
  # geom_line(aes(x = Year,
  #               y = landings,
  #               color = Continent.Name.En,
  #               group = Continent.Name.En)) + 
  geom_bar(aes(x = Year,
               y = landings,
               fill = Continent.Name.En),
           stat = 'identity',
           color = 'black')+
  scale_fill_manual(values = c(wes_palette('GrandBudapest2'), wes_palette('GrandBudapest1'))) + 
  scale_x_continuous(breaks = seq(1950, 2021, 5)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none') + 
  facet_wrap(Continent.Name.En ~., ncol = 2, scales = 'free_y') + 
  labs(fill = '',
       y = 'tons')


ggsave(fig_all, dpi = 300, width = 20, height = 20, units = 'cm', filename = 'plots/global_landings_all.png')



# desembarques nacionais




