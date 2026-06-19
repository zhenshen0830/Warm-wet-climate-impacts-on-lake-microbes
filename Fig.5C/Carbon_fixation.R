
rm(list = ls())

library(tidyverse)
data1<-read.csv("log2Carbon_fixation.csv",sep=',',header=TRUE)
wilcox.test(data1$WD, data1$WW)

level <- data1[,1]    
data1$K_ID <- factor(data1$K_ID,level = level)

library(ggplot2)
p<-ggplot(data1,aes(x=K_ID,y=log2FC,fill=paythway_cycle))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c('#E0E79B','#F1865F','#99CB93','#397CAF'))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p+theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file="Carbon_fixation.pdf", width=9, height=3)
