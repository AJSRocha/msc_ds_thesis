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
land_pt = read.csv('data/pt_production.csv',
                   sep = ';')

fig =
gridExtra::grid.arrange(
land_pt %>% 
  filter(Especie == 'Polvos') %>%
  filter(Year > 2001) %>% 
  ggplot() + 
  geom_bar(aes(x = Year, 
               y = land_tot_ton),
           stat = 'identity',
           fill = wes_palette('FantasticFox1')[1]) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(y = 'tons', fill = '') 
  # # geom_line(aes(x = Year,
  #               y = ))
,
land_pt %>% 
  # filter(Especie == 'Polvos') %>%
  filter(Year > 2001) %>% 
  group_by(Year) %>% 
  summarise(land_perc = land_tot_ton[Especie == 'Polvos']/
              land_tot_ton[Especie == 'Total'] * 100,
            val_perc = thousands_euros_tot[Especie == 'Polvos']/
              thousands_euros_tot[Especie == 'Total'] * 100
            ) %>% 
  pivot_longer(cols = -Year,
               names_to = 'type',
               values_to = 'perc') %>%
  mutate(type = case_when(type == 'land_perc' ~ 'Weight landed',
                          T ~ 'Monetary value')) %>% 
  ggplot() + 
  geom_line(aes(x = Year, 
               y = perc,
               group = type,
               color = type),
            size = 2) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(angle = 0),
        legend.position = 'bottom') + 
  labs(y = '%', color = "") + 
  scale_color_manual(values = c(wes_palette('Zissou1')[1],wes_palette('Zissou1')[3]))
,ncol=1)

ggsave(fig, dpi = 300, width = 20, height = 20, units = 'cm', filename = 'plots/pt_landings_all.png')
