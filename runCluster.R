setwd('Z:/2hat/project/Twitter.likely-en.player+risks.10Kjson')
#call the file containing the functions
source('ClusteringFunctions.R')

#load the data
load(DataFile)
#define data features
DataFeatures=colnames(mat.user_risk)[2:length(colnames(mat.user_risk))]
Features=DataFeatures[18]

t1=proc.time()[1]
runTheCluster(DataFile='mat_user_risk.RData',Features=Features,nUsers=150)
(t=(proc.time()[1]-t1)/60)


save(t,file=paste('time_', ifelse(length(Features)==1,Features,'all'),'.RData',sep=''))


