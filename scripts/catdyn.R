


# com df do script main
# calculo de esforco

ts_w = df %>%
  filter(regiao %in% c('Costa Ocidental', 'Costa Sul')) %>% 
  filter(EGRUPART %in% c('MIS_MIS')) %>% 
  group_by(year_sale, semana_tot, regiao) %>% 
  summarise(QVENDA = sum(QVENDA),
            n_trip = n_distinct(id_venda))
# catch

p1 =
ts_w %>%   
ggplot() + 
  geom_line(aes(x = semana_tot,
                y = QVENDA,
                group = regiao,
                color = regiao)) +
  scale_color_manual(values = wes_palette('Zissou1',5)[c(1,3)]) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  facet_grid(regiao ~.)

# cpue

p2 =
ts_w %>%   
  ggplot() + 
  geom_line(aes(x = semana_tot,
                y = QVENDA/n_trip,
                group = regiao,
                color = regiao)) +
  scale_color_manual(values = wes_palette('Zissou1',5)[c(2,4)]) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  facet_grid(regiao ~.)

grid.arrange(p1,p2, ncol =1)


df_catdyn_s = ts_w %>%
  ungroup() %>% 
  filter(regiao == 'Costa Sul') %>%
  filter(year_sale %in% c(1995, 1996)) %>% 
  transmute(obscat = QVENDA,
            obseff = obscat/n_trip)
           
# mean body weight simulado
df_catdyn_s$obsmbm = rnorm(nrow(df_catdyn_s),0.800, 0.010)
  
  
occ_cat = as.CatDynData(x=df_catdyn_s,
                       step="week",
                       fleet.name="Artisanal-S",
                       coleff=2,
                       colcat=1,
                       colmbw=3,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="bill",
                       season.dates=c(as.Date("1995-01-08"),
                                      as.Date("1996-12-24")))




