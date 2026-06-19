
library(ropls)
library(openxlsx)
library(ggplot2) 
df <- read.csv('COG_data.csv',header = T)
head(df)
df$ConcordGroup <- factor(df$ConcordGroup, levels = c('1','2','3','4','5'))


p <- ggplot(data = df,
            aes(x = log2Dry, y = log2Wet, 
                color = ConcordGroup)) + 
  geom_point(size = 3) 
p


mycol <- c('#397CAF','#A5D3A0','#F5F6BE','#EFAA63','#C62021') 

mytheme <- theme_classic() + 
  theme(axis.title = element_text(size = 15), 
        axis.text = element_text(size = 14), 
        legend.title = element_text(size = 15), 
        legend.text = element_text(size = 14), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

#图表美化：
p1 <- p +
  scale_colour_manual(name = "Rank differential\n(Dry vs. Wet)",
                      labels =c('>+0.5', 
                                ' 0.25 to 0.49', 
                                '-0.25 to +0.25', 
                                '-0.49 to -0.25', 
                                '>-0.5'),
                      values = alpha(mycol, 0.8)) + 
  labs(x = '-log2Dry_Abundance',
       y = '-log2Wet_Abundance') +
  guides(color = guide_legend(override.aes = list(size = 4.5))) + 
  scale_x_continuous(breaks = seq(8, 14, by = 1)) + 
  scale_y_continuous(limits = c(8, 14),
                     breaks = seq(0, 14, by = 1), 
                     expand = c(0,0)) + 
  mytheme
p1
ggsave(file="COG_volcano.pdf", width=6.2, height=4) 
