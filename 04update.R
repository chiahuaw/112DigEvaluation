library("dplyr")
library("httr")
library("jsonlite")

#### Get Data ####
load("01Data_alldig.RData")
load("01Data_case1.RData")
load("01Data_case2.RData")

#### Get New Data ####

## 取得資料 ####

target.m<-as.Date("2020-09-01") #設定時間區間
target.m2<-as.Date("2021-10-31")

tryCatch({ #透過API，取得道路挖掘資料。
  newdig = GET(sprintf("https://roaddig.kinmen.gov.tw/KMDigAPI/api/OpenData/GetCaseList?sdate=%s&edate=%s",target.m-365*2,target.m2),
               config(ssl_verifypeer=0)
  )
  newdig = fromJSON(content(newdig,"text"))
},error = function(e){newdig = data.frame()})

newdig = newdig$Data

newdig = filter(newdig,!grepl("T",EngUse),!grepl("測試",EngUse)) #剔除測試案件
newdig = mutate(newdig,Length=as.numeric(Length),Width=as.numeric(Width),Area=as.numeric(Area),X=as.numeric(newdig$X),Y=as.numeric(newdig$Y),AppDate=as.Date(AppDate),AllowStart=as.Date(AllowStart),AllowStop=as.Date(AllowStop))
newdig = filter(newdig,!is.na(AllowStart)) #剔除未核定案件
newdig$ExDate = gsub("　"," ",newdig$ExDate) 

newdig$FiDate = ""
for (i in 1:nrow(newdig)) { #以最後的展延日期為結案日期
  if (newdig$ExDate[i]=="") {
    newdig$FiDate[i] = as.character(newdig$AllowStop[i])
    next
  } else {
    newdig$FiDate[i] = (substr(newdig$ExDate[i],nchar(newdig$ExDate[i])-10,nchar(newdig$ExDate[i])-1))
    next
  }
}

newdig$FiDate = as.Date(newdig$FiDate)

newdig = filter(newdig,(FiDate+30>=target.m)&(FiDate+30<=target.m2)) #篩選結案日期位於時間區間內的案件

#### fix ####

newdig$PPName2 = ifelse(grepl("自來水廠",newdig$PPName),"金門縣自來水廠",newdig$PPName)

#### update ####

newdig2 = filter(newdig,CaseID %in% newdig2$CaseID)

#### kpi ####

lottery.unit = c("中華電信公司金門營運處","台灣中油股份有限公司高雄營業處","台灣電力股份有限公司金門區營業處","金門縣自來水廠")
lottery.case = filter(newdig2,PPName2 %in% lottery.unit)
lottery.case = filter(lottery.case,!(CaseStatus %in% c("案件已撤銷","註銷申請")))
lottery.case$SDChg = ifelse(lottery.case$SDChg=="否",
                            ifelse(lottery.case$EngType!="鋪面工程","是","否"),lottery.case$SDChg)

kpi1 = summarise(group_by(lottery.case,PPName2),n=n()) %>% 
  as.data.frame()
kpi2 = summarise(group_by(lottery.case,PPName2),n=n()) %>% 
  as.data.frame()

kpi1$已申報完工 = 0
for (i in 1:nrow(kpi1)) {
  kpi1$已申報完工[i] = filter(lottery.case,PPName2==kpi1$PPName2[i],grepl("完工",CaseStatus)) %>% nrow()
}
kpi1$ratio = round(kpi1$已申報完工/kpi1$n,4)

kpi2$應辦理圖資更新 = 0
kpi2$已辦理圖資更新 = 0
for (i in 1:nrow(kpi2)) {
  kpi2$應辦理圖資更新[i] = filter(lottery.case,PPName2==kpi1$PPName2[i],SDChg=="是") %>% nrow()
  kpi2$已辦理圖資更新[i] = filter(lottery.case,PPName2==kpi1$PPName2[i],SDStatus=="圖資已核備") %>% nrow()
}
kpi2$ratio = round(kpi2$已辦理圖資更新/kpi2$應辦理圖資更新,4)

kpi1
kpi2

save(newdig2,file="02Data_newdig.RData")
save(lottery.case,file="02Data_case1.RData")
