polylist = readLines("111polygon.gml")

temp = data.frame()
for (i in 1:length(polylist)) {
  if (grepl("<gml:polygonMember>",polylist[i])) {
    
    for (d in 1:10) {
      if (grepl("CASEID",polylist[i+d])) {
        temp = rbind(temp,data.frame(CaseID=as.character(regmatches(polylist[i+d],regexec("[0-9]{4}",polylist[i+d]))),
                                     gml=substr(polylist[i],regexpr("<gml:MultiPolygon",polylist[i])[1],regexpr("</gml:MultiPolygon",polylist[i])[1]+18)))
      }
    }
    
  }
  
}

temp = filter(temp,CaseID!="character(0)")
temp$gml = gsub("<gml:MultiPolygon srsName=\"EPSG:3825\">","",temp$gml)
temp$gml = gsub("</gml:MultiPolygon>","",temp$gml)

temp$no = 1
for (i in 1:nrow(temp)) {
  if (grepl("</gml:polygonMember><gml:polygonMember>",temp$gml[i])) {
    temp2 = length(unlist(strsplit(temp$gml[i],"</gml:polygonMember><gml:polygonMember>")))
    temp$no[i] = temp2
    rm(temp2)
  }
}

 polylist = temp
 rm(temp)

save(polylist,file="polylist.RData")
