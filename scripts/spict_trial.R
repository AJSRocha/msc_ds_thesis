
# effort_ns = 
# df %>% 
#   filter(regiao == 'Costa Sul') %>% 
#   filter(EGRUPART == 'MIS_MIS') %>% 
#   group_by(year_sale, month_sale, IEMBARCA) %>% 
#   summarise(effort = n_distinct(IDATVEND),
#             catch = sum(QVENDA)) %>% 
#   group_by(year_sale, month_sale) %>% 
#   summarise(effort = sum(effort),
#             catch = sum(catch)) %>% 
#   ungroup() %>% 
#   transmute(time = paste(year_sale,month_sale,sep = '_'),
#             effort = effort,
#             catch = catch)

effort_ns = 
  vd %>% 
  filter(zona == '27.9.a.s.a') %>% 
  filter(EGRUPART == 'MIS_MIS') %>% 
  group_by(year_sale, IEMBARCA) %>% 
  summarise(effort = n_distinct(IDATVEND),
            catch = sum(QVENDA)) %>% 
  group_by(year_sale) %>% 
  summarise(effort = sum(effort),
            catch = sum(catch))

effort_ns_correcto = 
  vd %>% 
  filter(zona == '27.9.a.s.a') %>% 
  filter(EGRUPART == 'MIS_MIS') %>% 
  group_by(IDATVEND, year_sale) %>% 
  summarise(effort = n_distinct(IEMBARCA),
            catch = sum(QVENDA)) %>% 
  group_by(year_sale) %>% 
  summarise(effort = sum(effort),
            catch = sum(catch))



save(effort_ns, file = 'app/data/spict.Rdata')

library(spict)

# effort_ns$time_nm = 1995 + (1:length(effort_ns$time)/12)
  
  
timeC = effort_ns_correcto$year_sale %>% as.character %>% as.numeric()
timeI = effort_ns_correcto$year_sale %>% as.character %>% as.numeric()
obsC = effort_ns_correcto$catch
obsI = effort_ns_correcto$effort

modelo_spict = list(timeC = timeC,
                    timeI = timeC,
                    obsC = obsC,
                    obsI = obsI)

modelo_spict$priors$logbkfrac <- c(log(0.8),0.5,1)
modelo_spict$ini$logn <- log(2) #adjust production curve to Shaefer
modelo_spict$phases$logn <- -1
modelo_spict$priors$logalpha <- c(1, 1, 0)
modelo_spict$priors$logbeta <- c(1, 1, 0)
# modelo_spict$dtc = 1/12
# modelo_spict$dteuler = 1/32

res_spict = fit.spict(modelo_spict)
retro_res = retro(res_spict)

# res_spict$opt$convergence
# res_spict$check.ini$resmat %>% print

plotspict.diagnostic(calc.osa.resid(res_spict))

# plot(effort_ns$effort ~ effort_ns$time_nm, type = 'l')
# plot(effort_ns$catch ~ effort_ns$time_nm, type = 'l')

plotspict.biomass(res_spict)
plotspict.bbmsy(res_spict)
plotspict.ffmsy(res_spict)
plotspict.fb(res_spict)

plotspict.production(res_spict)
par(mfrow = c(2,2))
plotspict.bbmsy(res_spict)
plotspict.ffmsy(res_spict)
plotspict.fb(res_spict)
plotspict.production(res_spict, n.plotyears = 40)
par(mfrow = c(1,1))
plotspict.diagnostic(calc.osa.resid(res_spict))
