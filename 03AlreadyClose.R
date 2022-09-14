fi.list = list.files("X:/03道路橋樑業務/04道路挖掘管理/完工查驗紀錄")
fi.list = fi.list[grepl("pdf",fi.list)]

fi.up=data.frame()
for (i in 1:nrow(lottery.case)) {
  for (d in 1:(length(fi.list))) {
    if (grepl(lottery.case$CaseID[i],fi.list[d])) {
      fi.up = rbind(fi.up,data.frame(CaseID=lottery.case$CaseID[i],SDChg=lottery.case$SDChg[i],SDStatus=lottery.case$SDStatus[i]))
    }
  }
}

write.csv(fi.up,file="fi_up.csv")
