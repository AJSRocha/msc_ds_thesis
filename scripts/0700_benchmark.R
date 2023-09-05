
library(xtable)
library(ggplot2)

load("C:/repos/msc_ds_thesis/benchmarks/cmsy_mis_s_y.Rdata")
load("C:/repos/msc_ds_thesis/benchmarks/cmsy_mis_w_y.Rdata")
load("C:/repos/msc_ds_thesis/benchmarks/jabba.Rdata")
load("C:/repos/msc_ds_thesis/benchmarks/jabba_all.Rdata")

temp =
rbind(perf_cmsy_w_mis,perf_cmsy_s_mis) %>% 
  reshape::melt()
names(temp) = c('Framework', 'Phase', 'Time')


temp %>% 
  ggplot() +
  geom_bar(aes(x = variable, y = value, fill = Framework),
           stat = 'identity',
           position = 'dodge') + 
    theme_bw() + 
  theme(legend.position = 'bottom',
        legend.direction = 'vertical') + 
  labs(x = '', y = 'seconds', fill = '')


xtable(temp) %>% 
  print(.,
        type = "latex",
        file = "tables/cmsy_perf.tex", 
        include.rownames = F,
        only.contents = T)

temp_jabba =
  perf_jabba%>% 
  reshape::melt()
names(temp_jabba) = c('Framework', 'Phase', 'Time')


temp_jabba %>% 
  ggplot() +
  geom_bar(aes(x = variable, y = value, fill = Framework),
           stat = 'identity',
           position = 'dodge') + 
  theme_bw() + 
  theme(legend.position = 'bottom',
        legend.direction = 'vertical') + 
  labs(x = '', y = 'seconds', fill = '')

xtable(temp_jabba) %>% 
  print(.,
        type = "latex",
        file = "tables/jabba_perf.tex", 
        include.rownames = F,
        only.contents = T)

temp_jabba_all =
  perf_jabba_all%>% 
  reshape::melt()
names(temp_jabba_all) = c('Model', 'Framework', 'Phase', 'Time')

xtable(temp_jabba_all) %>% 
  print(.,
        type = "latex",
        file = "tables/jabba_all_perf.tex", 
        include.rownames = F,
        only.contents = T)


load("C:/repos/msc_ds_thesis/benchmarks/spict_mis_w_y.Rdata")
load("C:/repos/msc_ds_thesis/benchmarks/spict_mis_s_y.Rdata")

temp_spict =
  rbind(perf_spict_w_mis_y,perf_spict_s_mis_y) %>% 
  reshape::melt()
names(temp_spict) = c('Framework', 'Phase', 'Time')

xtable(temp_spict) %>% 
  print(.,
        type = "latex",
        file = "tables/spict_perf.tex", 
        include.rownames = F,
        only.contents = T)


temp_spict %>% 
  ggplot() +
  geom_bar(aes(x = variable, y = value, fill = Framework),
           stat = 'identity',
           position = 'dodge') + 
  theme_bw() + 
  theme(legend.position = 'bottom',
        legend.direction = 'vertical') + 
  labs(x = '', y = 'seconds', fill = '')


benchmark = rbind(temp,
                  temp_jabba_all %>%
                    transmute(Framework = paste(Framework, Model),
                              Phase = Phase,
                              Time = Time),
                  temp_spict)

benchmark$Framework =
case_when(benchmark$Framework == "JABBA - Polyvalent, Southern Coast" ~ "JABBA - Polyvalent, Southern Coast, Yearly data",
T ~ benchmark$Framework)


fig =
benchmark %>% 
  ggplot() +
  geom_bar(aes(x = Framework, y = Time, fill = Framework),
           stat = 'identity',
           position = 'dodge') + 
  theme_bw() + 
  theme(legend.position = 'bottom',
        legend.direction = 'vertical',
        axis.text.x = element_blank()) + 
  facet_wrap(Phase ~., scales = 'free_y') + 
  labs(x = '', y = 'time (s)', fill = '') + 
  scale_fill_manual(values=c(wes_palette('FantasticFox1'),wes_palette('AsteroidCity1')))


ggsave(fig, dpi = 300, width = 20, height = 20, units = 'cm', filename = 'plots/benchmark.png')
