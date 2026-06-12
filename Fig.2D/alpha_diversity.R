library(reshape2)
library(ggplot2)
library(ggpubr)
library(ggalluvial)
library(ggsignif)
library(gghalves)

rm(list = ls())
setwd("D:/Climate warm-wet impacts on lake microbes/Fig.2D")
data <- read.csv('Alpha-bacterial.csv')
observed_ASVs <- data[,c(2,3)]
shannon <- data[,c(2,4)]


p_raw <- wilcox.test(observed_ASVs ~ Type, 
                     data = observed_ASVs,
                     exact = FALSE)$p.value

p_fdr <- p.adjust(p_raw, method = "BH")

p_label <- paste0("q = ", format.pval(p_fdr, digits = 3, eps = 0.001))

p1 <- ggplot(observed_ASVs, aes(x=Type, y=observed_ASVs,  fill = Type))+
  geom_half_violin(data = observed_ASVs[observed_ASVs$Type=="Warm-dry",], 
                   side = "l",width=0.7,
                   position = position_nudge(x=-0.2))+
  geom_half_violin(data = observed_ASVs[observed_ASVs$Type=="Warm-wet",], 
                   side = "r",width=0.7,
                   position = position_nudge(x=0.2))+
  geom_boxplot(data = observed_ASVs[observed_ASVs$Type=="Warm-dry",],
               width=0.08,size=0.6,outlier.color ="black",
               position = position_nudge(x=-0.2))+
  geom_boxplot(data = observed_ASVs[observed_ASVs$Type=="Warm-wet",],
               width=0.08,size=0.6,outlier.color ="black",
               position = position_nudge(x=0.2))+
  geom_jitter(aes(fill=Type),shape=21,size=2.5,width=0.1)+
  stat_summary(data = observed_ASVs[observed_ASVs$Type=="Warm-wet",],
               aes(color = Type, fill= Type),
               position = position_nudge(x=-0.2),
               fun = mean,
               size = 1,
               linewidth = 1.2,
               geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = observed_ASVs[observed_ASVs$Type=="Warm-dry",],
               aes(color = Type,fill= Type),
               position = position_nudge(x=0.2),
               fun = mean,
               size = 1,
               linewidth = 1.2,
               geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = observed_ASVs[observed_ASVs$Type=="Warm-wet",],
               aes(color = Type),
               position = position_nudge(x=-0.2),
               geom = "errorbar",
               width = 0.1,
               linewidth = 1.2,
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = observed_ASVs[observed_ASVs$Type=="Warm-dry",],
               aes(color = Type),
               position = position_nudge(x=0.2),
               geom = "errorbar",
               width = 0.1,
               linewidth = 1.2,
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  annotate("text", x = 0.65, y = 680, label = p_label, size = 4)+
  scale_fill_manual(values = c("#E18040","#67AFDC"))+
  scale_color_manual(values = c("#E18040","#67AFDC"))+
  labs(x=NULL,y="observed ASVs")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),
        axis.text = element_text(color = 'black',size=10),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0),
        axis.title = element_text(size=15),
        strip.text = element_text(color = "black",size = 14, hjust = 0.5))
p1




ggsave("p1.pdf",device="pdf",width = 5, height = 4)
###################

p_raw <- wilcox.test(shannon ~ Type, 
                     data = shannon,
                     exact = FALSE)$p.value

p_fdr <- p.adjust(p_raw, method = "BH")

p_label <- paste0("q = ", format.pval(p_fdr, digits = 3, eps = 0.001))

p2 <- ggplot(shannon, aes(x=Type, y=shannon, fill = Type))+
  geom_half_violin(data = shannon[shannon$Type=="Warm-dry",], 
                   side = "l",width=0.7,
                   position = position_nudge(x=-0.2))+
  geom_half_violin(data = shannon[shannon$Type=="Warm-wet",], 
                   side = "r",width=0.7,
                   position = position_nudge(x=0.2))+
  geom_boxplot(data = shannon[shannon$Type=="Warm-dry",],
               width=0.08,size=0.6,outlier.color ="black",
               position = position_nudge(x=-0.2))+
  geom_boxplot(data = shannon[shannon$Type=="Warm-wet",],
               width=0.08,size=0.6,outlier.color ="black",
               position = position_nudge(x=0.2))+
  geom_jitter(aes(fill=Type),shape=21,size=2.5,width=0.1)+
  stat_summary(data = shannon[shannon$Type=="Warm-wet",],
               aes(color = Type, fill= Type),
               position = position_nudge(x=-0.2),
               fun = mean,
               size = 1,
               linewidth = 1.2,
               geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = shannon[shannon$Type=="Warm-dry",],
               aes(color = Type,fill= Type),
               position = position_nudge(x=0.2),
               fun = mean,
               size = 1,
               linewidth = 1.2,
               geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = shannon[shannon$Type=="Warm-wet",],
               aes(color = Type),
               position = position_nudge(x=-0.2),
               geom = "errorbar",
               width = 0.1,
               linewidth = 1.2,
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = shannon[shannon$Type=="Warm-dry",],
               aes(color = Type),
               position = position_nudge(x=0.2),
               geom = "errorbar",
               width = 0.1,
               linewidth = 1.2,
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  annotate("text", x = 0.65, y = 6.8, label = p_label, size = 4)+
  scale_fill_manual(values = c("#E18040","#67AFDC"))+
  scale_color_manual(values = c("#E18040","#67AFDC"))+
  labs(x=NULL,y="shannon")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),
        axis.text = element_text(color = 'black',size=10),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0),
        axis.title = element_text(size=15),
        strip.text = element_text(color = "black",size = 14, hjust = 0.5))
p2
ggsave("p2.pdf",device="pdf",width = 5, height = 4)
###################################


########################################################################################
########################################################################################
rm(list = ls())
data <- read.csv('Alpha-eukaryotic.csv')
observed_ASVs <- data[,c(2,3)]
shannon <- data[,c(2,4)]


p_raw <- wilcox.test(observed_ASVs ~ Type, 
                     data = observed_ASVs,
                     exact = FALSE)$p.value

p_fdr <- p.adjust(p_raw, method = "BH")

p_label <- paste0("q = ", format.pval(p_fdr, digits = 3, eps = 0.001))

p11 <- ggplot(observed_ASVs, aes(x=Type, y=observed_ASVs,  fill = Type))+
  geom_half_violin(data = observed_ASVs[observed_ASVs$Type=="Warm-dry",], 
                   side = "l",width=0.7,
                   position = position_nudge(x=-0.2))+
  geom_half_violin(data = observed_ASVs[observed_ASVs$Type=="Warm-wet",], 
                   side = "r",width=0.7,
                   position = position_nudge(x=0.2))+
  geom_boxplot(data = observed_ASVs[observed_ASVs$Type=="Warm-dry",],
               width=0.08,size=0.6,outlier.color ="black",
               position = position_nudge(x=-0.2))+
  geom_boxplot(data = observed_ASVs[observed_ASVs$Type=="Warm-wet",],
               width=0.08,size=0.6,outlier.color ="black",
               position = position_nudge(x=0.2))+
  geom_jitter(aes(fill=Type),shape=21,size=2.5,width=0.1)+
  stat_summary(data = observed_ASVs[observed_ASVs$Type=="Warm-wet",],
               aes(color = Type, fill= Type),
               position = position_nudge(x=-0.2),
               fun = mean,
               size = 1,
               linewidth = 1.2,
               geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = observed_ASVs[observed_ASVs$Type=="Warm-dry",],
               aes(color = Type,fill= Type),
               position = position_nudge(x=0.2),
               fun = mean,
               size = 1,
               linewidth = 1.2,
               geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = observed_ASVs[observed_ASVs$Type=="Warm-wet",],
               aes(color = Type),
               position = position_nudge(x=-0.2),
               geom = "errorbar",
               width = 0.1,
               linewidth = 1.2,
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = observed_ASVs[observed_ASVs$Type=="Warm-dry",],
               aes(color = Type),
               position = position_nudge(x=0.2),
               geom = "errorbar",
               width = 0.1,
               linewidth = 1.2,
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  annotate("text", x = 0.65, y = 680, label = p_label, size = 4)+
  scale_fill_manual(values = c("#E18040","#67AFDC"))+
  scale_color_manual(values = c("#E18040","#67AFDC"))+
  labs(x=NULL,y="observed ASVs")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),
        axis.text = element_text(color = 'black',size=10),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0),
        axis.title = element_text(size=15),
        strip.text = element_text(color = "black",size = 14, hjust = 0.5))
p11

ggsave("p11.pdf",device="pdf",width = 5, height = 4)
############################



p_raw <- wilcox.test(shannon ~ Type, 
                     data = shannon,
                     exact = FALSE)$p.value

p_fdr <- p.adjust(p_raw, method = "BH")

p_label <- paste0("q = ", format.pval(p_fdr, digits = 3, eps = 0.001))
p22 <- ggplot(shannon, aes(x=Type, y=shannon, fill = Type))+
  geom_half_violin(data = shannon[shannon$Type=="Warm-dry",], 
                   side = "l",width=0.7,
                   position = position_nudge(x=-0.2))+
  geom_half_violin(data = shannon[shannon$Type=="Warm-wet",], 
                   side = "r",width=0.7,
                   position = position_nudge(x=0.2))+
  geom_boxplot(data = shannon[shannon$Type=="Warm-dry",],
               width=0.08,size=0.6,outlier.color ="black",
               position = position_nudge(x=-0.2))+
  geom_boxplot(data = shannon[shannon$Type=="Warm-wet",],
               width=0.08,size=0.6,outlier.color ="black",
               position = position_nudge(x=0.2))+
  geom_jitter(aes(fill=Type),shape=21,size=2.5,width=0.1)+
  stat_summary(data = shannon[shannon$Type=="Warm-wet",],
               aes(color = Type, fill= Type),
               position = position_nudge(x=-0.2),
               fun = mean,
               size = 1,
               linewidth = 1.2,
               geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = shannon[shannon$Type=="Warm-dry",],
               aes(color = Type,fill= Type),
               position = position_nudge(x=0.2),
               fun = mean,
               size = 1,
               linewidth = 1.2,
               geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = shannon[shannon$Type=="Warm-wet",],
               aes(color = Type),
               position = position_nudge(x=-0.2),
               geom = "errorbar",
               width = 0.1,
               linewidth = 1.2,
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  stat_summary(data = shannon[shannon$Type=="Warm-dry",],
               aes(color = Type),
               position = position_nudge(x=0.2),
               geom = "errorbar",
               width = 0.1,
               linewidth = 1.2,
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x))+
  annotate("text", x = 0.65, y = 6.8, label = p_label, size = 4)+
  scale_fill_manual(values = c("#E18040","#67AFDC"))+
  scale_color_manual(values = c("#E18040","#67AFDC"))+
  scale_y_continuous(expand = c(0,0), limits = c(2,7.7), breaks = seq(0,7.5,2.5))+
  labs(x=NULL,y="shannon")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),
        axis.text = element_text(color = 'black',size=10),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0),
        axis.title = element_text(size=15),
        strip.text = element_text(color = "black",size = 14, hjust = 0.5))
p22

ggsave("p22.pdf",device="pdf",width = 5, height = 4)
#######################
