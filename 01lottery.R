library("dplyr")
library("httr")
library("jsonlite")

## 取得資料 ####

target.m<-as.Date("2020-09-01") #設定時間區間
target.m2<-as.Date("2021-08-31")

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

alldig$FiDate = ""
for (i in 1:nrow(alldig)) { #以最後的展延日期為結案日期
  if (alldig$ExDate[i]=="") {
    alldig$FiDate[i] = as.character(alldig$AllowStop[i])
    next
  } else {
    alldig$FiDate[i] = (substr(alldig$ExDate[i],nchar(alldig$ExDate[i])-10,nchar(alldig$ExDate[i])-1))
    next
    }
}

alldig$FiDate = as.Date(alldig$FiDate)

alldig = filter(alldig,(FiDate+30>=target.m)&(FiDate+30<=target.m2)) #篩選結案日期位於時間區間內的案件

#### fix ####

alldig$PPName2 = ifelse(grepl("自來水廠",alldig$PPName),"金門縣自來水廠",alldig$PPName)

#### 抽選 ####

lottery.unit = c("中華電信公司金門營運處","台灣中油股份有限公司高雄營業處","台灣電力股份有限公司金門區營業處","金門縣自來水廠")

lottery.case = filter(alldig,PPName2 %in% lottery.unit)
lottery.case = filter(lottery.case,(FiDate<=as.Date("2021-10-14")))
lottery.case = filter(lottery.case,CaseStatus %in% c("已完工","已完工收件","已報完工待收件"))
lottery.case = arrange(lottery.case,PPName2,desc(Area))

lottery.case2 = data.frame()
for (i in 1:length(lottery.unit)) {
  Temp = filter(lottery.case,PPName2==lottery.unit[i]) #選取同單位案件
  
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

#### 輸出 ####

save(alldig,file="01Data_alldig.RData")
save(lottery.case,file="01Data_case1.RData")
save(lottery.case2,file="01Data_case2.RData")

write.csv(lottery.case2,file="抽選案件清單.csv",fileEncoding="big5",row.names=F)
