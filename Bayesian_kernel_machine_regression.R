#load packages
install.packages("bkmr")
library("bkmr")
pacman::p_load(bkmr,readxl,ggplot2)

#load data
data <- read_dta("C:/Users/HP/Desktop/data_insomnia.dta")

#analysis
summary(data$BMI)
data$BMI_2cat <- cut(data$BMI, breaks = c(-Inf, 24.52, Inf), labels = c(1,2), right=FALSE)
data$BMI_2cat<-as.factor(data$BMI_2cat)
data$occupation<-as.factor(data$occupation)
data$education<-as.factor(data$education)

covar = data.matrix(data[, c('age','BMI_2cat','education','occupation')])
expos = data.matrix(data[, c('PFHxA_ln', 'PFHpA_ln', 'PFOA_ln', 'PFNA_ln',
                             'PFDA_ln','PFUdA_ln','PFHxS_ln',  'PFHpS_ln',
                             'PFOS_ln','F53B_9Cl_ln')])
Y = data$insom
scale_expos = scale(expos) 

set.seed(20000000)
fitkm0 = kmbayes(Y, Z=scale_expos,X=covar,
                 iter=1000,verbose = TRUE,varsel = TRUE,
                 family='binomial',est.h = TRUE)

risks.overall = OverallRiskSummaries(fit=fitkm0,qs=seq(0.25,0.75,by=0.05),q.fixed = 0.5)
risks.overall
ggplot(risks.overall,aes(quantile,est,ymin=est-1.96*sd,ymax=est+1.96*sd))+
  geom_hline(yintercept = 0,lty=2,col='brown')+
  geom_pointrange()

risks.sigvar = SingVarRiskSummaries(
  fit = fitkm0, qs.diff = c(0.25,0.75),
  q.fixed = c(0.25,0.5,0.75))
subset(risks.sigvar,variable %in% c('PFHxA_ln', 'PFHpA_ln', 'PFOA_ln', 'PFNA_ln',
                                    'PFDA_ln','PFUdA_ln','PFHxS_ln',  'PFHpS_ln',
                                    'PFOS_ln','F53B_9Cl_ln'))
ggplot(risks.sigvar,aes(variable,est,ymin=est-1.96*sd,ymax=est+1.96*sd,col=q.fixed))+
  geom_pointrange(position = position_dodge(width = 0.75))+
  coord_flip()+
  scale_color_manual(values = c("#e8490f","#3cb346","#00abf0"))

pred.resp.univar = PredictorResponseUnivar(fit = fitkm0)
ggplot(pred.resp.univar,aes(z,est,ymin=est-1.96*se,ymax=est+1.96*se))+
  geom_smooth(stat = 'identity', color="#3f60aa")+
  facet_wrap(~variable,ncol=5)+
  xlab('expos')+
  ylab('h(expos)')

expos.pairs = subset(data.frame(expand.grid(expos1=c(1,2,3,4,5,6,7,8,9,10),expos2=c(1,2,3,4,5,6,7,8,9,10))),expos1<expos2)
expos.pairs
pred.resp.bivar = PredictorResponseBivar(fit=fitkm0,min.plot.dist = 0.5,z.pairs = expos.pairs)
ggplot(pred.resp.bivar,aes(z1,z2,fill=est))+
  geom_raster()+
  facet_grid(variable2~variable1)+
  scale_fill_gradientn(colours = c('#0000FFFF','#FFFFFFFF','#FF0000FF'))+
  xlab('expos1')+
  ylab('expos2')+
  ggtitle('h(expos1,expos2)')

pred.resp.bivar.levels = PredictorResponseBivarLevels(
  pred.resp.bivar,scale_expos,qs=c(0.25,0.5,0.75))
ggplot(pred.resp.bivar.levels,aes(z1,est))+
  geom_smooth(aes(col=quantile),stat='identity')+
  facet_grid(variable2~variable1)+
  ggtitle('h(expos1|quantiles of expos2)')+
  xlab('expos1')+
  scale_color_manual(values = c("#e8490f","#3cb346","#00abf0"))