

temp = vd_2022[vd_2022$id_venda=="PRT000024950_2022-02-03",]


temp$EESPECIE %in% c('OCC', 'OCT')

temp %>% 
  summarise(QVENDA_pi = sum(QVENDA),
            OCC = sum(QVENDA[EESPECIE %in% c("OCC")]))
