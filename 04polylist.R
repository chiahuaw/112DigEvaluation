polylist = read.table("clipboard",header = T,sep="\t")
save(polylist,file="polylist.RData")

load("polylist.RData")

polylist = select(polylist,-poly)
