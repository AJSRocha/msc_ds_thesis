library(CatDyn)


# com df do script main
# calculo de esforco

ts_w = df %>%
  filter(regiao %in% c('Costa Ocidental', 'Costa Sul')) %>% 
  filter(EGRUPART %in% c('MIS_MIS')) %>% 
  group_by(year_sale, month_sale, regiao) %>% 
  summarise(QVENDA = sum(QVENDA),
            n_trip = n_distinct(id_venda))
# catch

p1 =
ts_w %>%   
ggplot() + 
  geom_line(aes(x = year_sale,
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
  # filter(year_sale %in% c(1995, 1996)) %>% 
  transmute(obscat = QVENDA,
            obseff = obscat/n_trip)
           
# mean body weight simulado
df_catdyn_s$obsmbm = rnorm(nrow(df_catdyn_s),0.800, 0.010)
  
  
occ_cat = as.CatDynData(x=df_catdyn_s,
                       step="month",
                       fleet.name="Artisanal-S",
                       coleff=2,
                       colcat=1,
                       colmbw=3,
                       unitseff="trips",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="bill",
                       season.dates=c(as.Date("1995-01-01"),
                                      as.Date("2022-12-31")))

plot.CatDynData(occ_cat,
                mark = T,
                offset = c(0,1,10),
                hem = 'N')

M         <- 1/12 #1/Time step
N0.ini    <- 1 #billions
P1.ini    <- 0.5 #billions
k.ini     <- 0.02 #1/n of boats
alpha.ini <- 1.7 #adimensional
beta.ini  <- 0.6 #adimensional
P1 = 70
pars.ini  <- log(c(M,
                   N0.ini,
                   P1.ini,
                   k.ini,
                   alpha.ini,
                   beta.ini))

dates <- c(head(occ_cat$Data$`Artisanal-S`$time.step,1),
           P1,
           tail(occ_cat$Data$`Artisanal-S`$time.step,1))

lgahi.apln.1P.ini <- catdynexp(x=occ_cat,
                               p=1,
                               par=pars.ini,
                               dates=dates,
                               distr="aplnormal")


CatDynFit(x = occ_cat,
          p=1)
          
          
          par=pars.ini,
          dates=dates,
          distr="aplnormal",
          method="spg",
          itnmax=10)



