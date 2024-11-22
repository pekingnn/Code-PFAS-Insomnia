#load packages
install.packages("qgcomp")
library("qgcomp")
library("knitr")
library("ggplot2")
install.packages("RColorBrewer")
library(RColorBrewer)

#load data
rm(list = ls())
library(haven)
data <- read_dta("data_insomnia.dta")

#analysis
summary(data$BMI)
data$BMI_2cat <- cut(data$BMI, breaks = c(-Inf, 24.52, Inf), labels = c(1,2), right=FALSE)
data$BMI_2cat<-as.factor(data$BMI_2cat)
data$occupation<-as.factor(data$occupation)
data$education4<-as.factor(data$education)
data$insom<-as.factor(data$insom)

which(colnames(data) == 'PFHxA_ln')
which(colnames(data) == 'F53B_9Cl_ln')
com<-names(data)[18:27]

qc <- qgcomp.noboot(insom~PFHxA_ln+PFHpA_ln+PFOA_ln+PFNA_ln+PFDA_ln+PFUdA_ln+PFHxS_ln+PFHpS_ln+PFOS_ln+F53B_9Cl_ln, 
                    expnms=com, data = data, family=binomial(),q=4,bayes=TRUE)
plot(qc)
qc

qa <- qgcomp.noboot(insom~PFHxA_ln+PFHpA_ln+PFOA_ln+PFNA_ln+PFDA_ln+PFUdA_ln+PFHxS_ln+PFHpS_ln+PFOS_ln+F53B_9Cl_ln+
                      age+BMI_2cat+education+occupation,
                    expnms=com, data = data, family=binomial(),q=4,bayes=TRUE)
plot(qa)
qa

qa$pos.weights
qa$neg.weights
name<- c('PFOA_ln','PFHpA_ln','PFUdA_ln','PFHpS_ln','PFDA_ln',
         'F53B_9Cl_ln','PFNA_ln','PFHxA_ln','PFOS_ln','PFHxS_ln')
weight<-c(0.01289526, 0.06367164, 0.06639681, 0.10092053, -0.14239048, 
          0.22950306, 0.25799141, 0.28151655, -0.34696417, -0.49775009)

qc90_plot<-data.frame(weight,name)
qc90_plot$name<-factor(qc90_plot$name,
                       levels=c('PFOA_ln','PFHpA_ln','PFUdA_ln','PFHpS_ln','PFDA_ln',
                                'F53B_9Cl_ln','PFNA_ln','PFHxA_ln','PFOS_ln','PFHxS_ln'))

qgcom<-ggplot(qc90_plot)+ geom_bar(aes(x = name, y = weight, fill = weight),
                                   stat = "identity",color = "grey",lwd = 0.2) + 
  theme_bw() + coord_flip() +
  scale_fill_gradient(low = "#9ecae1",high = "#5D90BC")+
  scale_y_continuous(limits = c(-0.75, 0.75))+
  geom_hline(yintercept = 0, linetype = "solid", color = "black",size=0.8)+
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.y = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.line = element_line(size=0.8, colour = "black"),
        legend.position = "right",
        legend.text = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        legend.title = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())+labs(title="(A)Association between Quantile g-computation index and insomnia") 
qgcom