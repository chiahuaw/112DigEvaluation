library("dplyr")
library("magrittr")
library("data.table")
library("RCurl")
library("XML")
library("httr")
library("jsonlite")


#### Get Dig Data ####
load("01Data_alldig.RData")
load("01Data_case1.RData")
load("01Data_case2.RData")

load("polylist.RData")

#### 建立資料表 ####

appcase<-mutate(lottery.case2,StartDate=as.Date(AllowStart))
appcase<-arrange(appcase,CaseID)
appcase$town_name<-""
appcase$un_na<-""
appcase$ur_dr<-""

appcase$CurrentStatus = appcase$CaseStatus
appcase$un_na = appcase$PPName
appcase$town_name = substr(appcase$Town,1,3)
appcase$DistrictNo = ""

#建立
#完工結案日期CL_DA、最後核定復工/展延起始日期CBE_DA、最後核定復工/展延結束日期CEN_DA
#變更類型(1~3)CHG_TYPE、最後異動日期LASTMOD

appcase$cl_da<-appcase$FiDate

appcase$abe_da<-"" #初核起
appcase$aen_da<-"" #初核末
appcase$adg_da<-"" #工期日數
appcase$cbe_da<-"" #最後核起
appcase$cen_da<-"" #最後核末
appcase$ac_no<-"" #許可證號
appcase$daco_ti<-"" #日間施工時間
appcase$naco_ti<-"" #夜間施工時間
appcase$area_ta<-0

for (i in 1:nrow(appcase)) {
  
  appcase$abe_da[i] = as.character(as.Date(appcase$AllowStart[i]))
  appcase$aen_da[i] = as.character(as.Date(appcase$AllowStop[i]))
  appcase$adg_da[i] = difftime(as.Date(appcase$AllowStop[i]),as.Date(appcase$AllowStart[i]),units = "days")
  appcase$cbe_da[i] = as.character(as.Date(appcase$AllowStart[i]))
  appcase$cen_da[i] = as.character(as.Date(appcase$AllowStart[i]))
  appcase$ac_no[i] = appcase$CaseID[i]
  appcase$daco_ti[i] = "09:00~17:00"
  appcase$naco_ti[i] = "09:00~17:00"
  appcase$area_ta[i] = appcase$Area[i]
  
}
####計算巷弄####

info1<-appcase$Road
info1 = gsub("\\(","",info1)
info1 = gsub("\\)","",info1)



####清理座標中心####

info2<-select(appcase,X,Y)

####清理座標範圍清單####

info3<-paste0('<POLY_DETAIL WAREA_NO="',polylist$no,'"><WAREA_POLY><gml:MultiPolygon srsName="EPSG:3825">',polylist$gml,'</gml:MultiPolygon></WAREA_POLY>') %>% noquote()

#### 組合欄位 ####
case.nor<-data.frame(
  COUNTY_CODE=rep("W",nrow(appcase)),#縣市代碼
  TOWN_CODE=appcase$DistrictNo,#行政區代碼 DistrictNo
  TOWN_NAME=appcase$town_name,#行政區名稱
  AC_NO=appcase$ac_no,#許可證號 IssuanceNo
  CASE_ID=appcase$CaseID,#案件編號 AppNo
  CONST_NAME=appcase$EngUse,#工程名稱 Purpose
  LOCATION=appcase$Road,#工程地點 LocationRoadName
  ADD_VI="",#村里 info1$村里
  DG_ROAD=info1,#街路大道名 info1$路
  ADD_DAN="",#段 info1$段
  ADD_SH="",#巷 info1$巷
  ADD_NA="",#弄 info1$弄
  ADD_NO="",#號 info1$號
  DG_ROAD2=rep("",nrow(appcase)),#省縣鄉道名
  DG_ROAD2_BE=rep("00",nrow(appcase)),#省縣鄉道起始公里
  DG_ROAD2_EE=rep("00",nrow(appcase)),#省縣鄉道結束公里
  A_UN=rep("金門縣政府",nrow(appcase)),#核定單位
  ABE_DA=appcase$abe_da,#核定挖掘起日 ApprovedStartDate
  AEN_DA=appcase$aen_da,#核定挖掘迄日 ApprovedEndDate
  ADG_DA=as.numeric(appcase$adg_da),#核定工期日數 IssuanceDate
  DACO_TI=appcase$daco_ti,#核定日間施工時段 ApprovedStartTime
  NACO_TI=appcase$naco_ti,#核定夜間施工時段 ApprovedEndTime
  AREA_TA=appcase$area_ta,#申請施工面積 TarArea+CementArea+PedestrianArea
  UN_NA=appcase$un_na,#管線單位名稱 un_na
  UR_NA=rep("",nrow(appcase)),#管線單位連絡人名稱 UndertakerName
  UR_DR=appcase$ur_dr,#聯絡地址 ur_dr
  UR_TI=rep("",nrow(appcase)),#聯絡電話 UndertakerPhone
  PURP=appcase$EngUse,#施工內容 Purpose
  PH_URLS=rep("",nrow(appcase)),#施工前照片超連結
  PH_URLA=rep("",nrow(appcase)),#施工後照片超連結
  DG_STATUS=as.character(appcase$CurrentStatus),#施工狀態 CurrentStatus
  CL_DA=appcase$cl_da,#完工結案日期 done
  CHG_TYPE=rep("",nrow(appcase)),#變更類型(1~3)
  CBE_DA=appcase$cbe_da,#最後核定復工/展延起始日期
  CEN_DA=appcase$cen_da,#最後核定復工/展延結束日期
  CENTER_COORDS_X=info2$X,#施工範圍中心點x坐標 info2$X
  CENTER_COORDS_Y=info2$Y,#施工範圍中心點y坐標 info2$Y
  #POLY_LIST=appcase$polylist,
  LASTMOD=rep("",nrow(appcase)),#最後異動日期
  CASE_TYPE = ifelse(appcase$CaseType=="一般案件",0,ifelse(appcase$CaseType=="民生案件",5,9)),
  WAREA_TYPE=ifelse(grepl("WAREA_NO=1>",appcase$polylist),0,1)
  ,stringsAsFactors=F)


#### Check and create a export file####

case.update = case.nor

case.update$XY<-paste0(case.update$CENTER_COORDS_X,",",case.update$CENTER_COORDS_Y)

case.update$ADG_DA<-ifelse(is.na(case.update$ADG_DA),0,case.update$ADG_DA)

case.update<-filter(case.update,AEN_DA!="")

xmlbuff<-sprintf('<CASE_DETAIL LASTMOD="%s"><TOWN_CODE>%s</TOWN_CODE><TOWN_NAME>%s</TOWN_NAME><AC_NO>%s</AC_NO><CASE_ID>%s</CASE_ID><CONST_NAME>%s</CONST_NAME><LOCATION>%s</LOCATION><A_UN>%s</A_UN><ABE_DA>%s</ABE_DA><AEN_DA>%s</AEN_DA><ADG_DA>%s</ADG_DA><DACO_TI>%s</DACO_TI><NACO_TI>%s</NACO_TI><AREA_TA>%s</AREA_TA><UN_NA>%s</UN_NA><UR_NA>%s</UR_NA><UR_DR>%s</UR_DR><UR_TI>%s</UR_TI><DG_STATUS>%s</DG_STATUS><CENTER_COORDS><gml:Point><gml:coordinates>%s</gml:coordinates></gml:Point></CENTER_COORDS><POLY_LIST>%s</POLY_LIST></CASE_DETAIL>',
                 case.update$LASTMOD,case.update$TOWN_CODE,case.update$TOWN_NAME,case.update$AC_NO,case.update$CASE_ID,case.update$CONST_NAME,
                 case.update$LOCATION,case.update$A_UN,case.update$ABE_DA,case.update$AEN_DA,case.update$ADG_DA,
                 case.update$DACO_TI,case.update$NACO_TI,case.update$AREA_TA,case.update$UN_NA,case.update$UR_NA,
                 case.update$UR_DR,case.update$UR_TI,case.update$DG_STATUS,case.update$XY,
                 info3) %>% noquote()

for (i in 1:length(xmlbuff)) {
  if (i==1) {temp<-data.frame()}
  temp<-paste0(temp,xmlbuff[i])
  if (i==length(xmlbuff)) {
    xmlbuff<-temp %>% noquote()
    rm(temp)
  }
}

xmlbuff<-paste0('<?xml version="1.0" encoding="UTF-8"?><DIG_CASE xmlns:gml="http://www.opengis.net/gml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"><INFO County_Code="W" Count="',nrow(case.update),'"/><CASE_LIST>',
                xmlbuff,'</CASE_LIST></DIG_CASE>') # %>% base64Encode()

writeLines(xmlbuff,con = file("111digcaselist.xml"))

