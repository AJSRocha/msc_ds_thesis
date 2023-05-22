


Pella = function(Bt, r, p, k, Ct){
  res = Bt + (r*Bt * (1/p) * (1 - (Bt/k)^p))
  return(res)
}  


estimador =  function(B0, r, p, k, Ct) {
  res = c()
  res[1] = Pella(B0, r, p, k, Ct[1])
  # preenche resto das estimativas
  for(i in 2:length(Ct)){
    res[i] = Pella(res[i-1], r, p, k, Ct[i])
  }
  return(res)
}


teste = estimador(df_a[,'QVENDA'][1],
                  r =  0.10000000,
                  p =  1,
                  k =  0.57216185,
                  Ct = df_a[,'QVENDA'])

teste


  ggplot() + 
  geom_line(aes(x = factor(1995:2021),
                y = teste,
                group = 1)) +
    geom_line(aes(x = factor(1995:2021),
                  y = df_a[,'QVENDA'],
                  group = 1),
              color = 'red') + 
  theme_classic() + 
    theme(axis.text.x = element_text(angle = 90))
  
ans
  
    