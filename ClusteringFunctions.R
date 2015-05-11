checkpackages=function(package){
  if (!package %in% installed.packages())
    install.packages(package)
}

checkpackages("cluster")
checkpackages("fpc")
checkpackages("mclust")
checkpackages("ggplot2")
library("ggplot2")
library(cluster) 
library(fpc) 
library(mclust)

# setwd('Z:/2hat/project')
# load('GeneratedData.RData')


#extract the data features for each user
#aggregated on the entire history

ExtractFeatures =function(DataSet,Features=c('General Risk','Sexting','Bullying','Vulgar'),Time_scale=2){
  
#generate a list object for each player
getplayerID=unique(DataSet[,'playerID'])
getplayerID=getplayerID[which(!is.na(getplayerID))]
DataSet1=list()
for (i in (1:length(getplayerID))){
  DataSet1[[i]]=DataSet[which(DataSet[,'playerID']==getplayerID[i]),]
}

  
  
nOfFeatures     = 8*length(Features)
mat             = matrix(NA,length(DataSet1),nOfFeatures+1)
colnames(mat)   = c('playerID',rep(0:7, length(Features)))

for (i in (1:length(DataSet1))){
  
  #colnames(DataSet[[i]])=c('UserID','Trust_level','Smoothed GR','General Risk','Sexting','Bullying','Vulgar')  
  #print(i)
  
  if (length(Features)==1){

   
   if (is.vector(DataSet1[[i]])){
     
     getcounts          =  table(DataSet1[[i]][which(names(DataSet1[[i]]) %in% Features)])
     mat[i,]            =  c(DataSet1[[i]]['playerID'], rep(0,nOfFeatures))
     mat[i, which(colnames(mat) %in% names(getcounts)) ]=
                           getcounts/(length(DataSet1[[i]][which(names(DataSet1[[i]]) %in% Features)]))
  
     
   }else{
     
     getcounts          =  table(DataSet1[[i]][,which(colnames(DataSet1[[i]]) %in% Features)])
     mat[i,]            =  c(unique(DataSet1[[i]][,'playerID']), rep(0,nOfFeatures))
     mat[i, which(colnames(mat) %in% names(getcounts)) ]=
                           getcounts/(length(DataSet1[[i]][,which(colnames(DataSet1[[i]]) %in% Features)]))
     
   } 
    
      
    
    }else{
   
   getcounts=NULL
   
   for (l in (1:length(Features))){
     if (is.vector(DataSet1[[i]])){
       #when features are more than 1 you do not use table
       getcounts  =  table(DataSet1[[i]][which(names(DataSet1[[i]]) %in% Features[l])])
       mat[i,]    =  c(DataSet1[[i]]['playerID'], rep(0,nOfFeatures))
       mat[i, which(colnames(mat)[(1:8)+1+8*(l-1)] %in% names(getcounts)) + 1+8*(l-1)] = getcounts/(length(DataSet1[[i]][which(names(DataSet1[[i]]) %in% Features)]))
     }else{ 
       getcounts  =  table(DataSet1[[i]][,which(colnames(DataSet1[[i]]) %in% Features[l])])
       mat[i,]    =  c(unique(DataSet1[[i]][,'playerID']), rep(0,nOfFeatures))
       mat[i, which(colnames(mat)[(1:8)+1+8*(l-1)] %in% names(getcounts)) + 1+8*(l-1)] = getcounts/(nrow(DataSet1[[i]]))  
     }
   }
     
  }

}
  save(mat,file=paste('mat',ifelse(length(Features)==1,Features,'all'),'.RData',sep=''))
  return(mat)
}


runTheCluster=function(DataFile='mat_user_risk.RData',Features=c('topic_Bullying'),nUsers=150){
  
  load(DataFile)
  
  #mat.user_risk is the matrix containinng the data
  #which is loaded by load('mat_user_risk.RData')
  #extract the features
  #t1=proc.time()[1]
  #load(paste('mat',ifelse(length(Features)==1,Features,'all'),'.RData',sep=''))
  mat=ExtractFeatures(DataSet=mat.user_risk,Features=Features)
  #(t=proc.time()[1]-t1)/60
  #0.2053333 
  #mat1=as.matrix(mat)
  mat1=matrix(NA,dim(mat)[1],dim(mat)[2])
  colnames(mat1)=colnames(mat)
  #convert the string data into double
  for (i in (1:ncol(mat))){
    mat1[,i]=as.double(mat[,i])
  }
  colnames(mat1)=colnames(mat)
  #run kmeans with 1,2,,,10 clusters and then decide how many clusters
  #will be the best fit for the data
  #res.kmeans is a list object containing 10 list objects obtained
  #as an output from the kmeans
  #lapply takes a vector (in this case 1:10) and applies the function
  #to each of the elements of the provided vector
  res.kmeans = lapply(1:10, function(i) {
    kmeans(mat1, i)
  })
  
  #decide the number of clusters that are the best fit of the data
  #Calculate the within cluster sum of squares and make a plot
  ## SS for each cluster (1 cluster to 10 clusters)
  #lapply(res.kmeans, function(x) x$withinss)
  
  ## Sum up SS
  res.within.ss <- sapply(res.kmeans, function(x) sum(x$withinss))
  
  
  #generate a qgglot scree plot of the within cluster Sum of Squares
  #The scree plot is widely used technique in Principal Component and Factor Analysis
  #that is based on the percentage of variance explained for each of the principal components or factors
  
  #By looking at the scree plot we pick the number of clusters where the within ss stops 
  #changing abrubptly. 
  ggplot(data.frame(cluster = 1:10, within.ss = res.within.ss), aes(cluster, within.ss)) +
    geom_point() + geom_line() +
    scale_x_continuous(breaks = 0:10)
  
  ggsave(paste("ggplot_within_ss_", ifelse(length(Features)==1,Features,'All'),".png",sep=''), width=4, height=4, dpi=100)
  
  #another measure that determines the number of clusters is the percentage of 
  #variance explained which in case of kmeans is determined as a ratio between
  # the between cluster sum of squares (SS) and total SS (SS for all clusters)
  res.var.explained <- sapply(res.kmeans, function(x) (x$betweenss/x$totss))
  
  #pickup the number of clusters such that the difference between 
  #the variance explained for each of the different models (with 1, ..,10 clusters)
  #is bigger than 0.05
  nClusters=length(res.var.explained[c(1,which(diff(res.var.explained)>0.05)+1)])
  
  #again generate a scree plot with variance explained for each of the models
  #*(with 1,..,10 clusters)
  
  #generate a qgglot scree plot of the percentage of explained variance
  ggplot(data.frame(cluster = 1:10, variance.explained = res.var.explained), aes(cluster, variance.explained)) +
    geom_point() + geom_line() +
    scale_x_continuous(breaks = 0:10)
  
  ggsave(paste("ggplot_var_explained_", ifelse(length(Features)==1,Features,'all'),".png",sep=''), width=4, height=4, dpi=100)
  #dev.off()
  
  #generate plots of all the users into nClusters number of clusters

  #to generate plots it is very important to remove the vectors that contain all 0 or all 1 (less likely)
  allZeroesOrAllOnesAll=(apply(mat1[,2:ncol(mat1)],2,function(x) (all(x==0)==TRUE || all(x==1)==TRUE)))
  

  #to ensure that the covariance matrix is positive definite we take
  #out the vector columns that cause the covariance matrix to be non positive definite
    ColInd=ColInd1=c(1,2)
    for (m in (1:(length( which(allZeroesOrAllOnesAll==0)+1 )-1))){
    
    if (is.positive.definite(cov(mat1[,(which(allZeroesOrAllOnesAll==0)+1)[ColInd]]))){
      #print(TRUE)
      ColInd1=ColInd
    }
    ColInd=c(ColInd,ColInd[length(ColInd)]+1)
    }
  
  png(paste('clusterPlot',nrow(mat1),'_',ifelse(length(Features)==1,Features,'all'),'.png',sep=''))
  
  clusplot(mat1[,(which(allZeroesOrAllOnesAll==0)+1)[ColInd1]],res.kmeans[[nClusters]]$cluster,color=TRUE, shade=TRUE, 
           labels=2, lines=0, main=paste('Plot od all ',nrow(mat1),' users',sep=''))
  
  dev.off()
  #generate plots of  nUsers number of users into nClusters number of clusters
  #png keyword writes .png files in the working directory
  
  png(paste('clusterPlot',nUsers,'_',ifelse(length(Features)==1,Features,'all'),'.png',sep=''))
  
  #since we are sampling at random nUsers number of users, there might be cases
  #where all the elements in the one of the states (0..7) are 0.
  #to prevent that we keep resamling until we have a good data set for the plot
  #Note that this is for visualization only
  #while(any(apply(mat1[ind,2:9],2,function(x) (all(x==0)==TRUE || all(x==1)==TRUE)))){ 
  
  plotSelectedUsers=sample(mat1[,1],size=nUsers)
  ind=which(mat1[,1] %in% plotSelectedUsers)
  
  allZeroesOrAllOnes=(apply(mat1[ind,2:ncol(mat1)],2,function(x) (all(x==0)==TRUE || all(x==1)==TRUE)))
  #}
  
  clusplot(mat1[ind,which(allZeroesOrAllOnes==0)+1], res.kmeans[[nClusters]]$cluster[ind], color=TRUE, shade=TRUE, 
           labels=1, lines=0,main=paste('Plot of',nUsers, 'randomly selected users'))
  #close the png graphics device
  dev.off()
  
  
  mat_cluster=cbind(mat1,res.kmeans[[nClusters]]$cluster)
  save(mat_cluster,file='mat_cluster.RData')
  return(mat_cluster)
}



