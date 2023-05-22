# load(paste0(dados,'//2022_occ_tmp//2020_2021_raw.RData'))

library(dplyr)
library(ggplot2)
library(forecast)

source('C://repos/path.R'); path('local')

source('scripts/vd_import.R')

df_ano = vd %>% 
  filter(EGRUPART == 'MIS_MIS') %>% 
  filter(zona %in% c('27.9.a.c.s', '27.9.a.c.n', '27.9.a.s.a')) %>% 
  mutate(area = case_when(zona == '27.9.a.s.a' ~ 'S',
                          T ~ 'W')) %>% 
  group_by(year_sale, area) %>% 
  summarise(QVENDA = sum(QVENDA))

df_a = ts(data = df_ano %>%
            filter(area == 'S') %>% 
            ungroup() %>% 
            select(QVENDA),
          start = 1995,
          end = 2021,
          frequency = 1)

effort_a = vd %>% 
  filter(EGRUPART == 'MIS_MIS') %>% 
  filter(zona %in% c('27.9.a.c.s', '27.9.a.c.n', '27.9.a.s.a')) %>% 
  group_by(year_sale) %>% 
  summarise(effort = length(unique(id_venda)),
            effort_2 = n_distinct(id_venda))

plot(df_a)

ggplot() + 
  geom_line(aes(x = factor(1995:2021),
                y = df_a[,'QVENDA'],
                group = 1)) + 
  geom_line(aes(x = factor(1995:2021),
                y = effort_a$effort*50,
                group = 1),
            color = 'red') + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90))


df_mes = vd %>% 
  filter(EGRUPART == 'MIS_MIS') %>% 
  filter(zona %in% c('27.9.a.c.s', '27.9.a.c.n', '27.9.a.s.a')) %>% 
  mutate(area = case_when(zona == '27.9.a.s.a' ~ 'S',
                          T ~ 'W')) %>% 
  group_by(year_sale, month_sale, area) %>% 
  summarise(QVENDA = sum(QVENDA))

df_m = ts(data = df_mes %>%
            filter(area == 'S') %>% 
            ungroup() %>% 
            select(QVENDA),
          start = 1995,
          end = 2021,
          frequency = 12)

plot(df_m)

decompose(df_m) %>%  plot

acf(df_m, lag.max = 120)
