

##@@@@----函数目的:随机游走的计算，并且用留一法得到预测的结果和计算相应的AUC，MPR,Recall,F_measure值
##@@  seed: 输入随机游走的种子，第一列是种子，第二列是condition
##@@  backgroundnetwork：输入背景网络，两列数据，两列都是节点
##@@  r：随机游走函数中的参数，默认是0.7，也可以换成别的
##@@　path: 随机游走结果的存储路径和文件，eg:"E:/a.txt"
##@@  AUC: 是否计算AUC，默认不计算
##@@  AUCPath: 如果AUC=T，要计算AUC值的情况下，要给一个AUC结果的存储路径，格式跟上面path一样
##@@  MPR：是否计算MPR，默认不计算
##@@  Recall：是否计算Recall，默认不计算
##@@  RecallPath: 如果Recall=T，要计算Recall值的情况下，要给一个Recall结果的存储路径，格式跟上面path一样
##@@  F_measure：是否计算F_measure，默认不计算
##@@  F_measurePath: 如果F_measure=T，要计算F_measure值的情况下，要给一个F_measure结果的存储路径，格式跟上面path一样
 

RandomWalk <- function(seed,
                       backgroundnetwork,
					   r,
					   path,
					   AUC = F,
					   AUCPath,
					   MPR = F,
					   Recall = F,
					   RecallPath)
            {
				
				library(ROCR)
				library(PRROC)
				
				##随机游走函数的主体部分，下面会调用这个函数
			    my_rand_walk<-function(W,p0,r)
				                {
                                pt<-p0
                                delta<-1
                                while(delta>1e-10){
                                pt1 = (1-r)*W%*%pt+r*p0
                                delta = sum(abs(pt1 - pt))
                                pt = pt1}
                                return(pt)
                                }
								
				##把背景网络换成数字表示
                interact <- as.matrix(backgroundnetwork)
                nodes <- unique(c(interact[,1],interact[,2]))
                nodes <- cbind(nodes,c(1:length(nodes)))
                rownames(nodes) <- nodes[,1]
                colnames(nodes) <- c("nodes","ID")
                Index1 <- match(interact[,1],nodes[,1])
                Index2 <- match(interact[,2],nodes[,1])
                Net_ID <- cbind(Index1,Index2)	

                ##创建权重矩阵，有互作的关系对是1，没有的是0
				m <- matrix(0,nrow(nodes),nrow(nodes))
                for(i in 1:nrow(Net_ID)) {
				    n1 <- Net_ID[i,1]
					n2 <- Net_ID[i,2]
					m[n1,n2] <- 1
					m[n2,n1] <- 1
				}
                dev <- colSums(m)
                W <- t(t(m) / dev)

                ##对每一个condition，计算相应的随机游走的概率，并用留一法交叉证实来验证预测结果的真实性
				allnode<-unique(seed)
                alld<-names(table(allnode[,2]))[table(allnode[,2])>5]
                for(j in alld)
                {
                    allseed<-allnode[allnode[,2]==j,1]
                    allseed <- unique(allseed)
                    for(out in allseed)
                    {
                        liuyi<-allseed[-which(allseed==out)]
                        overlap <- as.matrix(intersect(liuyi,nodes[,1]),ncol=1) # make sure all the seeds are vertexes in the network
                        colnames(overlap) <- "seeds"
                        seeds <- merge(overlap,nodes,by.x="seeds",by.y="nodes")
	                    seedsnames <- as.character(seeds[,1])
	                    seeds <- as.numeric(as.character(seeds[,2]))
						
						#seeds<-allseed[-which(allseed==out)]
                        #seeds <- intersect(seeds,rownames(nodes)) # make sure all the seeds are vertexes in the network
                        #seeds <- nodes[seeds,2]
						#seeds <- as.numeric(seeds)
						
                        p0=rep(0,nrow(nodes))
						p0[seeds] <- 1/length(seeds) #equal probability of seeds.
						
                        p_final <- my_rand_walk(W,p0,r)
                        p_final <- cbind(nodes[,1],p_final)
                        p_final <- cbind(j,p_final)
                        label<-rep(0,length(nodes[,1]))
                        label[nodes[,1]==out]<-1
                        p_final <- cbind(p_final,label) 
                        p_final <- p_final[-match(seedsnames,p_final[,2]),]##去掉真实的才是预测的
                        rank<-order(order(1-as.numeric(p_final[,3])))#the larger the value, the more reliable the relationship is
                        p_final<-cbind(p_final,rank)
                        write.table(p_final,path,sep="\t",row.names=F,quote=F,col.name=F,append=T)
                    }
                }
				result <- read.table(path,head=F,sep="\t",quote="",as.is=T)
				result[,6] <- dim(result)[1]-result[,5]
				##计算AUC
				if (AUC) {
				    auc<-c()
                    for(i in unique(result[,1]))
					{
                        each <- result[which(result[,1]==i),]
	                    pred<-prediction(as.numeric(each[,6]),as.numeric(each[,4]))
	                    aucPerf<-performance(pred,"auc")
                        AUCValue<-aucPerf@y.values[[1]]
	                    each_condition <- c(i,AUCValue)
	                    auc <- rbind(auc,each_condition)
                    }
                    mean_auc <- mean(as.numeric(auc[,2]))
					print("mean AUC:")
					print(mean_auc)
                write.table(auc,AUCPath,sep="\t",row.names=F,quote=F,col.name=F)
				}
				
				##计算MPR
				if (MPR) {
				    Di<-unique(result[,1])
                    MPR_result<-c()
                    for(i in Di)
                    {
	                    part<-result[result[,1]==i,]
	                    num_test<-length(which(part[,4]==1))
	                    num_pre<-nrow(part)/num_test            ##用每个condition的所有的行数除以留一法的种子数，得到每次随机游走的所有应该排名的种子数
	                    test_rank<-part[part[,4]==1,5]
	                    MPR<-mean(test_rank/num_pre)
	                    temp<-c(i,MPR)
	                    MPR_result<-rbind(MPR_result,temp)
                    }
                    mean_MPR <- mean(as.numeric(MPR_result[,2]))
					print("MPR:")
					print(mean_MPR)
				}
				
				##计算Recall
				if (Recall) {
				    Recall<-c()
                    for(i in unique(result[,1]))
					{
                        each <- result[which(result[,1]==i),]
	                    pred<-prediction(as.numeric(each[,6]),as.numeric(each[,4]))
	                    recPerf<-performance(pred,"rec")
						recValue <- unlist(recPerf@y.values)
	                    each_condition <- cbind(rep(i,length(recValue)),recValue)
	                    Recall <- rbind(Recall,each_condition)
                    }
                    mean_Recall <- mean(as.numeric(Recall[,2]))
					print("mean Recall:")
					print(mean_Recall)
                    write.table(Recall,RecallPath,sep="\t",row.names=F,quote=F,col.name=F)
				}
				

			}