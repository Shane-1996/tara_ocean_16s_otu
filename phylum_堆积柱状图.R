setwd("~/Documents/北京大学/单室反应器抽平结果/群落组成")
phylum<-read.csv("phylum_bar.csv",header = T,row.names = 1,check.names = FALSE) 
phylum$sum <- rowSums(phylum)
phylum <- phylum[order(phylum$sum, decreasing = TRUE), ]
phylum_top10 <- phylum[1:10, -ncol(phylum)]
phylum_top10 <- phylum[which(rowMeans(phylum)>0.01),-ncol(phylum)]  #挑选均值大于1%的门
phylum_top10['Others', ] <- 1 - colSums(phylum_top10)
write.csv(phylum_top10, 'phylum_top10.csv')   #可忽略此步
#phylum_top10 <- phylum[which(colMeans(phylum)>0.001), -ncol(phylum)] #用于挑选属水平相对分度大于0.1%的种
#采用ggplot2做堆积柱状图
library(reshape2)    #用于排列数据
#调整数据布局
phylum_top10<-read.csv("phylum_top10.csv",header = T,row.names=1,check.names = FALSE) #可忽略此步
phylum_top10$Taxonomy <- factor(rownames(phylum_top10), levels = rev(rownames(phylum_top10)))
phylum_top10 <- melt(phylum_top10, id = 'Taxonomy')
#添加分组
group <- read.csv('phylum_group.csv', header = T,stringsAsFactors = FALSE)
group=as.data.frame(group[,c(1,3)])
names(group)[1] <- 'variable'
phylum_top10 <- merge(phylum_top10, group, by = 'variable')
library(ggplot2)
p <- ggplot(phylum_top10, aes(variable, 100 * value, fill = Taxonomy)) +
  labs(x = 'Samples', y = 'Relative Abundance(%)') +
  theme(axis.text = element_text(size = 15,face = "plain",color ='black'), axis.title = element_text(size = 16,color ='black')) +
  theme(legend.text = element_text(size = 11,color ='black'))+
  geom_col(position = 'stack', width = 0.6)

#scale_x_discrete(breaks =c("-0.05V_a", "-0.05V_b", "-0.05V_c", "-0.29V_a", "-0.29V_b", "-0.29V_c","open_a", "open_b", "open_c"))
#使用geom_col()绘制柱状图，position = 'stack'，堆叠柱状图，width = 0.6，柱子宽度；
#labs()设置坐标轴标签；
#theme()中，axis.text和axis.title分别指定坐标轴刻度字体大小及标签字体大小，
#legend.text指定图例字体大小
p
#使用scale_fill_manual()为各类群自定义颜色
p <- p +
  scale_fill_manual(values =  rev(c('cyan','blue', 'purple', 'hotpink', 'orange', 'red', 'green', 'tomato','burlywood1', 'salmon','skyblue', 'gray','magenta','yellow'))) 
# 13种颜色
p
p<-p+theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent')) +
  theme(legend.title = element_blank()) #去除背景框，legend.title = element_blank()用于去除图例
p
#分面
pdf("phylum_top10_组成图.pdf",height=7,width=12)
par(mar=c(5,7,2,2))
p1 <- p +
  facet_wrap(~note, scales = 'free_x', ncol = 5) +
  theme(strip.text = element_text(size = 16))
p1
dev.off()


