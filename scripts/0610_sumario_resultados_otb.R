

load('tables/otb/jabba_tables_otb.Rdata')
load('tables/otb/spict_tables_otb_w.Rdata')
load('tables/otb/spict_tables_otb_s.Rdata')



res_spict_w = data.frame(res_spict_w)
res_spict_s = data.frame(res_spict_s)
bet_total_w$estimates
bet_total_s$estimates
res_spict_w



spm_sum = data.frame('Parameter' = c('r', 'K', 'q','MSY'),
                     'CMSY++ Ww' = c(
                                   NA,
                                   NA,
                                   NA,
                                   NA),
                     
                     'CMSY++ S' = c(
                                    NA,
                                    NA,
                                    NA,
                                    NA),
                     
                     'JABBA W' = c(
                                   bet_total_w$estimates['r','mu']%>% round(3),
                                   bet_total_w$estimates['K','mu']%>% trunc(0),
                                   NA,
                                   bet_total_w$estimates['MSY','mu']%>% round(3)),
                     
                     'JABBA S' = c(
                                   bet_total_s$estimates['r','mu']%>% round(3),
                                   bet_total_s$estimates['K','mu']%>% trunc(0),
                                   NA,
                                   bet_total_s$estimates['MSY','mu']%>% round(3)),
                     
                     'SPiCT W' = c(
                                   res_spict_w['r ', 'estimate']%>% round(3),
                                   res_spict_w['K', 'estimate']%>% trunc(0),
                                   res_spict_w['q', 'estimate'],
                                   res_spict_w['MSYs', 'estimate']%>% round(3)),
                     
                     'SPiCT S' = c(
                                   res_spict_s['r ', 'estimate']%>% round(3),
                                   res_spict_s['K', 'estimate']%>% trunc(0),
                                   res_spict_s['q', 'estimate'],
                                   res_spict_s['MSYs', 'estimate']%>% round(3)),
                     check.names = F)

xtable(spm_sum, digits = -1) %>% 
  print(.,
        type = "latex",
        file = "tables/otb/model_comps_otb.tex", 
        include.rownames = F,
        only.contents = T)


Results of CMSY analysis 
-------------------------
  r   = 0.751 , 95% CL = 0.51 - 0.939 , k = 13 , 95% CL = 10.6 - 18.8 
MSY = 2.45 , 95% CL = 2.13 - 2.73 
Relative biomass in last year = 0.342 k, 2.5th perc = 0.17 , 97.5th perc = 0.523 
Exploitation F/(r/2) in last year = 0.988 , 2.5th perc = 0.561 , 97.5th perc = 2.92 

Results from Bayesian Schaefer model (BSM) using catch & CPUE 
------------------------------------------------------------
  q   = 0.418 , lcl = 0.29 , ucl = 0.602 (derived from catch and CPUE) 
r   = 0.615 , 95% CL = 0.419 - 0.905 , k = 14.5 , 95% CL = 9.93 - 21.6 , r-k log correlation = -0.956 
MSY = 2.23 , 95% CL = 2.02 - 2.54 
Relative biomass in last year = 0.333 k, 2.5th perc = 0.222 , 97.5th perc = 0.449 
Exploitation F/(r/2) in last year = 1.08 , 2.5th perc = 0.689 , 97.5th perc = 1.89 

Results for Management (based on BSM analysis) 
-------------------------------------------------------------
  Fmsy = 0.307 , 95% CL = 0.21 - 0.452 (if B > 1/2 Bmsy then Fmsy = 0.5 r)
Fmsy = 0.307 , 95% CL = 0.21 - 0.452 (r and Fmsy are linearly reduced if B < 1/2 Bmsy)
MSY  = 2.23 , 95% CL = 2.02 - 2.54 
Bmsy = 7.25 , 95% CL = 4.96 - 10.8 
Biomass in last year = 4.83 , 2.5th perc = 3.02 , 97.5 perc = 7.29 
B/Bmsy in last year  = 0.667 , 2.5th perc = 0.443 , 97.5 perc = 0.898 
Fishing mortality in last year = 0.33 , 2.5th perc = 0.202 , 97.5 perc = 0.567 
Exploitation F/Fmsy  = 1.08 , 2.5th perc = 0.689 , 97.5 perc = 1.89 
Comment:  