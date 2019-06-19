##seed_stat
seed_stat <- read.table("E:/HYE/本地磁盘/resistance_related_noncoding_RNA/data/seed_stat.txt",head=T,sep="\t",quote="",as.is=T)
rownames(seed_stat) <- seed_stat[,1]
seed_stat <- seed_stat[,-1]
seed_stat <- as.matrix(seed_stat)


	
##AUC compare
result <- read.table("E:/HYE/本地磁盘/resistance_related_noncoding_RNA/data/AUC_compare.txt",head=T,sep="\t",quote="",as.is=T)
ggplot(data=result,aes(x=Cancer,y=AUC,group=data_type)) + 
       geom_line(aes(colour=data_type)) + 
	   geom_point(size=4,aes(shape=data_type,colour=data_type)) + 
	   #geom_errorbar(aes(ymax=max,ymin=min),width=0.2,colour="grey")+
	   xlab("")+ylab("average_value")+theme_bw()+
	   theme(panel.grid.major=element_line(colour=NA))+
	   theme(panel.grid.minor=element_line(colour=NA))+ylim(0,1)

  

##===
MPR_compare <- read.table("E:/HYE/本地磁盘/resistance_related_noncoding_RNA/data/MPR_compare_1.txt",head=T,sep="\t",quote="",as.is=T) 
ggplot(data=MPR_compare,aes(x=Cancer,y=average_MPR,group=net_type)) + 
       geom_line(aes(colour=net_type)) + 
	   geom_point(size=4,aes(shape=net_type,colour=net_type)) + 
	   #geom_errorbar(aes(ymax=max,ymin=min),width=0.2,colour="grey")+
	   xlab("")+ylab("average_MPR")+theme_bw()+
	   theme(panel.grid.major=element_line(colour=NA))+
	   theme(panel.grid.minor=element_line(colour=NA))+ylim(0,1) 
##==
Recall_compare <- read.table("E:/HYE/本地磁盘/resistance_related_noncoding_RNA/data/Recall_compare_1.txt",head=T,sep="\t",quote="",as.is=T) 
ggplot(data=Recall_compare,aes(x=Cancer,y=average_Recall,group=net_type)) + 
       geom_line(aes(colour=net_type)) + 
	   geom_point(size=4,aes(shape=net_type,colour=net_type)) + 
	   geom_errorbar(aes(ymax=max,ymin=min),width=0.2,colour="grey")+
	   xlab("")+ylab("average_MPR")+theme_bw()+
	   theme(panel.grid.major=element_line(colour=NA))+
	   theme(panel.grid.minor=element_line(colour=NA)) +ylim(0,1) 
  
=========================================================
##technology compare
##==
Recall_tech_compare <- read.table("E:/HYE/本地磁盘/resistance_related_noncoding_RNA/data/Recall_tech_compare_1.txt",head=T,sep="\t",quote="",as.is=T)
ggplot(data=Recall_tech_compare,aes(x=Cancer,y=average_AUC,group=tech)) + 
       geom_line(aes(colour=tech)) + 
	   geom_point(size=4,aes(colour=tech)) + 
	   xlab("")+ylab("average_Recall")+theme_bw() +ylim(0,1) 


#=====
AUC_tech_compare <- read.table("E:/HYE/本地磁盘/resistance_related_noncoding_RNA/data/AUC_tech_compare_1.txt",head=T,sep="\t",quote="",as.is=T)
ggplot(data=AUC_tech_compare,aes(x=Cancer,y=average_AUC,group=tech)) + 
       geom_line(aes(colour=tech)) + 
	   geom_point(size=4,aes(colour=tech)) + 
	   xlab("")+ylab("average_AUC")+theme_bw() +ylim(0,1) 	
#==
MPR_tech_compare <- read.table("E:/HYE/本地磁盘/resistance_related_noncoding_RNA/data/MPR_tech_compare_1.txt",head=T,sep="\t",quote="",as.is=T)
ggplot(data=MPR_tech_compare,aes(x=Cancer,y=average_AUC,group=tech)) + 
       geom_line(aes(colour=tech)) + 
	   geom_point(size=4,aes(colour=tech)) + 
	   xlab("")+ylab("average_MPR")+theme_bw()  +ylim(0,1)  
##调参图
auc <- read.table("E:/HYE/本地磁盘/resistance_related_noncoding_RNA/data/r_stat.txt",head=T,sep="\t",quote="",as.is=T)
ggplot(data=auc, aes(x=r, y=AUC),group=Cancer) + geom_line(aes(colour=Cancer)) + geom_point(size=4, shape=20,aes(colour=Cancer))+
scale_x_continuous(breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))+ 
theme_bw()+theme(panel.grid.major=element_line(colour=NA))+theme(panel.grid.minor=element_line(colour=NA))+
ylim (0,1)

mpr <- read.table("E:/HYE/本地磁盘/resistance_related_noncoding_RNA/data/r_mpr_stat.txt",head=T,sep="\t",quote="",as.is=T)
ggplot(data=mpr, aes(x=r, y=MPR),group=Cancer) + geom_line(aes(colour=Cancer)) + geom_point(size=4, shape=20,aes(colour=Cancer))+
scale_x_continuous(breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))+
theme_bw()+theme(panel.grid.major=element_line(colour=NA))+theme(panel.grid.minor=element_line(colour=NA))+
ylim (0,1)

Recall <- read.table("E:/HYE/本地磁盘/resistance_related_noncoding_RNA/data/r_Recall_stat.txt",head=T,sep="\t",quote="",as.is=T)
ggplot(data=Recall, aes(x=r, y=Recall),group=Cancer) + geom_line(aes(colour=Cancer)) + geom_point(size=4, shape=20,aes(colour=Cancer))+
scale_x_continuous(breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))+
theme_bw()+theme(panel.grid.major=element_line(colour=NA))+theme(panel.grid.minor=element_line(colour=NA))+
ylim (0,1)
 
