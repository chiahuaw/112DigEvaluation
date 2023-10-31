workc = readLines(file("~/GoogleDrive/工作/112營建署管挖考評/workcheck.txt"),encoding="UTF-8")
# workcheck.txt
# 用Chorme的開發者工具，複製出道挖的施工打卡查詢的DataList的回應的文字

workb = data.frame()

for (i in 1:length(workc)) {
  if (grepl("CaseID = ",workc[i])) {
    Temp = data.frame(CaseID=workc[i],
                      In=workc[i+6],
                      Out=workc[i+7])
    
    workb = rbind(workb,Temp)
    rm(Temp)
  }
}

reg_format = "[0-9]{4}"
doc_form1 = regexec(reg_format,workb$CaseID)

reg_format2 = "[0-9]{1,2}"
doc_form2 = regexec(reg_format2,workb$In)
doc_form3 = regexec(reg_format2,workb$Out)

workb$CaseID = regmatches(workb$CaseID,doc_form1)
workb$In = regmatches(workb$In,doc_form2)
workb$Out = regmatches(workb$Out,doc_form3)

workb = as.data.frame(workb)

###

load("01Data_alldig.RData")

worka = alldig
worka$CheckIn =0
worka$CheckOut=0

for (i in 1:nrow(worka)) {
  if (worka$CaseID[i] %in% workb$CaseID) {
    worka$CheckIn[i] = as.numeric(workb$In[workb$CaseID==worka$CaseID[i]])
    worka$CheckOut[i] = as.numeric(workb$Out[workb$CaseID==worka$CaseID[i]])
  }
}

worka$CheckPoint =0

for (i in 1:nrow(worka)) {
  
  if (worka$CheckIn[i]>0 & worka$CheckOut[i]>0) {worka$CheckPoint[i]=1}
  if (worka$CheckIn[i]>0 & worka$CheckOut[i]==0) {worka$CheckPoint[i]=0.5}
  if (worka$CheckIn[i]==0 & worka$CheckOut[i]==0) {worka$CheckPoint[i]=0}

}

summarise(group_by(worka,PPName),n=n(),point=sum(CheckPoint),ratio=round(point/n,4)) %>% 
  data.frame()
