#Read Jsonfile

checkpackages("jsonlite")
library(jsonlite)

setwd('Z:/2hat/project/Twitter.likely-en.player+risks.10Kjson')
#User = fromJSON('[{"client_id":0,"topic_Drugs":"0","text":"I added a video to a playlist Nerf Blasters: Lake House Edition","topic_Website":"0","topic_PII":2,"topic_Junk":"0","simplified":"i added a video to a playlist nerf blasters lake house edition","topic_Bullying":"0","phrase":"QAAAACJD|0|en|I added a video to a playlist Nerf Blasters: Lake House Edition","topic_Sexting":"0","topic_Alarm":"0","topic_Fighting":1,"topic_Fraud":"0","filename":"Twitter.likely-en.json.gz","source":"twitter","topic_PublicThreats":"0","filter_time":0.0155489445,"topic_Grooming":"0","topic_TerroristRecruitment":"0","risk":2,"start_time":1430946511.2400569916,"topic_InGame":"0","topic_General":2,"last_filter":"HiddenCapWord","topic_Vulgar":"0","topic_RealName":"0","room":"None","language":"en","server":"None","flags":1073741824,"topic_Religion":"0","topic_Racist":"0"}]')
nLines                   = 10000
nTopicRisks              = 19+1
LineByLine               = readLines('Twitter.likely-en.player+risks.10K.json',n=nLines)
ExtractlineByline        = list()
mat.user_risk            = matrix(NA,nLines,nTopicRisks)

for (i in (1:10000)){
  ExtractlineByline[[i]] = fromJSON(LineByLine[i])
  mat.user_risk[i,]      = c(ExtractlineByline[[i]]$player,ExtractlineByline[[i]]$risk,ExtractlineByline[[i]]$topic_Drugs, ExtractlineByline[[i]]$topic_Website,ExtractlineByline[[i]]$topic_PII,ExtractlineByline[[i]]$topic_Junk,ExtractlineByline[[i]]$topic_Bullying,ExtractlineByline[[i]]$topic_Sexting,ExtractlineByline[[i]]$topic_Alarm,ExtractlineByline[[i]]$topic_Fighting,ExtractlineByline[[i]]$topic_Fraud,ExtractlineByline[[i]]$topic_PublicThreats,ExtractlineByline[[i]]$topic_Grooming,ExtractlineByline[[i]]$topic_TerroristRecruitment,ExtractlineByline[[i]]$topic_InGame,ExtractlineByline[[i]]$topic_General,ExtractlineByline[[i]]$topic_Vulgar,ExtractlineByline[[i]]$topic_RealName,ExtractlineByline[[i]]$topic_Religion,ExtractlineByline[[i]]$topic_Racist)
}

colnames(mat.user_risk)  = c('playerID','General risk','topic_Drugs', 'topic_Website','topic_PII','topic_Junk','topic_Bullying','topic_Sexting','topic_Alarm','topic_Fighting','topic_Fraud','topic_PublicThreats','topic_Grooming','topic_TerroristRecruitment','topic_InGame','topic_General','topic_Vulgar','topic_RealName','topic_Religion','topic_Racist')

save(mat.user_risk,file='mat_user_risk.RData')
