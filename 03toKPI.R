library(dplyr)
load("01Data_alldig.RData")

lottery.unit = c("中華電信公司金門營運處","台灣中油股份有限公司高雄營業處","台灣電力股份有限公司金門區營業處","金門縣自來水廠")
lottery.case = filter(alldig,PPName2 %in% lottery.unit)
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
