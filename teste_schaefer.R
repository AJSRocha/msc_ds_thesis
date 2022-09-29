
library(MQMF)

cobaia = data.frame(year = 1995:2021,
                    catch = df_a[,'QVENDA'],
                    effort = effort_a$effort)

cobaia$cpue = cobaia$catch/cobaia$effort

cobaia = as.matrix(cobaia)

#primeiro teste

param <- log(c(r=0.1, K=1985316, Binit=1985316, sigma=0.5))

negatL <- negLL(param,simpspm,cobaia,logobs=log(cobaia[,"cpue"])) 

ans <- plotspmmod(inp=param,
                  indat=cobaia,
                  schaefer=TRUE,  
                  addrmse=TRUE,
                  plotprod=FALSE)  

### Optimizacao

pnams <- c("r","K","Binit","sigma")  
best <- optim(par=param,
              fn=negLL,
              funk=simpspm,
              indat=cobaia,  
              logobs=log(cobaia[,"cpue"]),method="BFGS")

outfit(best,digits=4,title="Optim",parnames = pnams) 

cat("\n")  
best2 <- nlm(negLL,best$par,funk=simpspm,indat=cobaia,  
             logobs=log(cobaia[,"cpue"]))

outfit(best2,digits=4,title="nlm",parnames = pnams)  


ans <- plotspmmod(inp=best2$estimate,
                  indat=cobaia,
                  schaefer=TRUE,  
                  addrmse=TRUE,
                  plotprod=FALSE) 

best2$estimate
ans
