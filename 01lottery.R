library("dplyr")
library("httr")
library("jsonlite")

## 取得資料 ####

target.m<-as.Date("2021-09-01") #設定時間區間
target.m2<-as.Date("2022-08-31")

tryCatch({ #透過API，取得道路挖掘資料。
  alldig = GET(sprintf("https://roaddig.kinmen.gov.tw/KMDigAPI/api/OpenData/GetCaseList?sdate=%s&edate=%s",target.m-365*2,target.m2),
               config(ssl_verifypeer=0)
  )
  alldig = fromJSON(content(alldig,"text"))
},error = function(e){alldig = data.frame()})

alldig = alldig$Data

alldig = filter(alldig,!grepl("T",EngUse),!grepl("測試",EngUse)) #剔除測試案件
alldig = mutate(alldig,Length=as.numeric(Length),Width=as.numeric(Width),Area=as.numeric(Area),X=as.numeric(alldig$X),Y=as.numeric(alldig$Y),AppDate=as.Date(AppDate),AllowStart=as.Date(AllowStart),AllowStop=as.Date(AllowStop))
alldig = filter(alldig,!is.na(AllowStart)) #剔除未核定案件
alldig$ExDate = gsub("　"," ",alldig$ExDate) 
alldig$RptDate = gsub("\\/","-",alldig$RptDate)

alldig$FiDate = ""
for (i in 1:nrow(alldig)) { #以提報完工日或最後的展延日期為結案日期
  if (alldig$RptDate[i]!="") {
    alldig$FiDate[i] = alldig$RptDate[i]
    next
  }
  if (alldig$ExDate[i]=="") {
    alldig$FiDate[i] = as.character(alldig$AllowStop[i])
    next
  } else {
    alldig$FiDate[i] = (substr(alldig$ExDate[i],nchar(alldig$ExDate[i])-10,nchar(alldig$ExDate[i])-1))
    next
    }
}

alldig$FiDate = as.Date(alldig$FiDate)

alldig = filter(alldig,(FiDate>=target.m)&(FiDate<=target.m2)) #篩選結案日期位於時間區間內的案件

#### fix ####

alldig$PPName = ifelse(grepl("自來水廠",alldig$PPName),"金門縣自來水廠",alldig$PPName)

town.list = c("金城鎮","金湖鎮","金沙鎮","金寧鄉","烈嶼鄉")
for (i in 1:nrow(alldig)) {
  if (alldig$Town[i] %in% town.list) { next }
  for (d in 1:length(town.list)) {
    if (grepl(town.list[d],alldig$Town[i])) {
      alldig$Town[i] = town.list[d]
    }
    if (grepl(town.list[d],alldig$Road[i])) {
      alldig$Town[i] = town.list[d]
    }
  }
}
alldig$Town[alldig$CaseID=="2985"] = "金城鎮"
alldig$Town[alldig$CaseID=="3306"] = "金沙鎮"
alldig$Town[alldig$CaseID=="3417"] = "金沙鎮"
alldig$Town[alldig$CaseID=="3421"] = "金沙鎮"
alldig$Town[alldig$CaseID=="3427"] = "金沙鎮"
alldig$Town[alldig$CaseID=="3431"] = "金沙鎮"
alldig$Town[alldig$CaseID=="3806"] = "金沙鎮"
alldig$Town[alldig$CaseID=="3808"] = "金沙鎮"
alldig$Town[alldig$CaseID=="3821"] = "金寧鄉"
alldig$Town[alldig$CaseID=="3839"] = "金寧鄉"
alldig$Town[alldig$CaseID=="3843"] = "金湖鎮"
alldig$Town[alldig$CaseID=="3899"] = "金寧鄉"
alldig$Town[alldig$CaseID=="3954"] = "金城鎮"
alldig$Town[alldig$CaseID=="3961"] = "金湖鎮"
alldig$Town[alldig$CaseID=="3973"] = "金湖鎮"
alldig$Town[alldig$CaseID=="4007"] = "烈嶼鄉"
alldig$Town[alldig$CaseID=="4060"] = "金城鎮"
alldig$Town[alldig$CaseID=="4063"] = "金寧鄉"
alldig$Town[alldig$CaseID=="4064"] = "金寧鄉"
alldig$Town[alldig$CaseID=="4067"] = "金寧鄉"
alldig$Town[alldig$CaseID=="4069"] = "金城鎮"
alldig$Town[alldig$CaseID=="4070"] = "金湖鎮"
alldig$Town[alldig$CaseID=="4078"] = "金寧鄉"
alldig$Town[alldig$CaseID=="4091"] = "金沙鎮"
alldig$Town[alldig$CaseID=="4098"] = "金城鎮"
alldig$Town[alldig$CaseID=="4170"] = "金湖鎮"
alldig$Town[alldig$CaseID=="4172"] = "金城鎮"
alldig$Town[alldig$CaseID=="4177"] = "金城鎮"
alldig$Town[alldig$CaseID=="4189"] = "金城鎮"
alldig$Town[alldig$CaseID=="4203"] = "金城鎮"
alldig$Town[alldig$CaseID=="4222"] = "金沙鎮"
alldig$Town[alldig$CaseID=="4265"] = "金湖鎮"
alldig$Town[alldig$CaseID=="4266"] = "金湖鎮"
alldig$Town[alldig$CaseID=="4267"] = "金湖鎮"
alldig$Town[alldig$CaseID=="4268"] = "金沙鎮"
alldig$Town[alldig$CaseID=="4272"] = "金湖鎮"
alldig$Town[alldig$CaseID=="4280"] = "金城鎮"
alldig$Town[alldig$CaseID=="4301"] = "金寧鄉"
alldig$Town[alldig$CaseID=="4309"] = "金城鎮"
alldig$Town[alldig$CaseID=="4313"] = "金湖鎮"
alldig$Town[alldig$CaseID=="4314"] = "金湖鎮"
alldig$Town[alldig$CaseID=="4315"] = "金寧鄉"
alldig$Town[alldig$CaseID=="4316"] = "烈嶼鄉"
alldig$Town[alldig$CaseID=="4326"] = "烈嶼鄉"
alldig$Town[alldig$CaseID=="4335"] = "烈嶼鄉"
alldig$Town[alldig$CaseID=="4358"] = "金湖鎮"
alldig$Town[alldig$CaseID=="4367"] = "金城鎮"
alldig$Town[alldig$CaseID=="4369"] = "金湖鎮"
alldig$Town[alldig$CaseID=="4405"] = "烈嶼鄉"
alldig$Town[alldig$CaseID=="4454"] = "金城鎮"
alldig$Town[alldig$CaseID=="4455"] = "金寧鄉"
alldig$Town[alldig$CaseID=="4466"] = "金寧鄉"
alldig$Town[alldig$CaseID=="4496"] = "金寧鄉"
alldig$Town[alldig$CaseID=="4555"] = "金湖鎮"


#### 抽選 ####

lottery.unit = c("中華電信公司金門營運處","台灣中油股份有限公司高雄營業處","台灣電力股份有限公司金門區營業處","金門縣自來水廠")

lottery.case = filter(alldig,PPName %in% lottery.unit)
lottery.case = filter(lottery.case,CaseStatus %in% c("已完工","已完工收件","已報完工待收件"))
lottery.case = arrange(lottery.case,PPName,desc(Area))

lottery.case2 = data.frame()
for (i in 1:length(lottery.unit)) {
  Temp = filter(lottery.case,PPName==lottery.unit[i]) #選取同單位案件
  
  if (nrow(Temp)==0) {next} #若無案件，跳過
  if (nrow(Temp)<=15) {  #若不足15件，全取
    lottery.case2 = rbind(lottery.case2,Temp)
    rm(Temp)
    next
  } else { #若大於15件，抽選方式
    if (nrow(Temp)<=30) { #抽選方式1，小於30件，按面積由大至小抽依序取15件
      lottery.case2 = rbind(lottery.case2,head(Temp,15))
      rm(Temp)
      next
    } else { #抽選方式2，大於30件時，採隨機抽選15件面積均大於中位數的案件
      Temp2.mid = quantile(Temp$Area,0.5)[[1]]
      
      Temp2 = sample_n(filter(Temp,Area>=Temp2.mid),15,replace = F)
      
      lottery.case2 = rbind(lottery.case2,Temp2)
      rm(Temp)
      rm(Temp2)
      next
    }
    
    }
  
  
}

summarise(group_by(lottery.case2,PPName),n=n())

lottery.case3 = mutate(lottery.case2,city="金門縣政府",repair="自行修復",RoadType = ifelse(grepl("柏油路面",lottery.case2$RoadType),"柔性","剛性")) %>% 
  select(.,CaseID,city,PPName,Town,EngUse,repair,Road,RoadType,Length,Width,Area,RptDate,CaseStatus,RptDate) %>% 
  `names<-`(c("案件編號","路權單位","申請單位","行政區","工程名稱","路面修復","施工地點","鋪面類型","挖掘長度","挖掘寬度","挖掘面積","報竣日期","案件狀態"))
lottery.case3$完工結案日期 = as.Date(lottery.case3$報竣日期)

#### 輸出 ####

save(alldig,file="01Data_alldig.RData")
save(lottery.case,file="01Data_case1.RData")
save(lottery.case2,file="01Data_case2.RData")


write.csv(lottery.case2,file="抽選案件清單.csv",fileEncoding="big5",row.names=F)
write.csv(lottery.case3,file="案件提報清單.csv",fileEncoding="big5",row.names=F)
