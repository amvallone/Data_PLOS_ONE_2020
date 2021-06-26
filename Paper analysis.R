
#Require pacakgaes

pkg <- c("magrittr", "ggplot2","directlabels","ggalt","ggrepel","dplyr","plyr","tidyverse","PAFit","igraph","reshape2","poweRlaw","lubridate","tidyr","scales")
sapply(pkg,library,character.only = TRUE)


# preparing the data for sna anañysis
# Load the rdata files abailable at https://github.com/amvallone/Data_PLOS_ONE_2020/tree/main 

full2007<-distinct(full2007)
full2008<-distinct(full2008)
full2009<-distinct(full2009)
full2010<-distinct(full2010)
full2011<-distinct(full2011)
full2012<-distinct(full2012)
full2013<-distinct(full2013)
full2014<-distinct(full2014)
full2015<-distinct(full2015)
full2016<-distinct(full2016)
full2017<-distinct(full2017)
full2018<-distinct(full2018)
full2019<-distinct(full2019)


bex<-Reduce(rbind.fill,list(full2007,full2008,full2009,full2010,full2011,full2012,full2013,full2014,full2015,full2016,full2017,full2018,full2019))
rm(full2007,full2008,full2009,full2010,full2011,full2012,full2013,full2014,full2015,full2016,full2017,full2018,full2019)

#library(lubridate)
bex$aa<-difftime(bex$patent_date-bex$patent_processing_time)

bex$date<-as.Date(bex$patent_date)
bex$patent_processing_time<-as.numeric(bex$patent_processing_time)

bex$aa<-bex$date-bex$patent_processing_time
bex$year_fil<-year(bex$aa)
bb<-select(bex,patent_id,year_fil)
bb1<-merge(base,bb,by.x = "patent_id",by.y="patent_id")
bb1$diff<-bb1$year_fil-bb1$year

rm(bex)
rm(bb)
base<-bb1
rm(bb1)
base1<-select(base,-diff,-year)
#save(base1,file="base1.RData")


##base1

dd<-select(base1,inventor_id,ipc_section,patent_id,year_fil)
names(dd)[4]<-"patent_year"

dd$patent_id<-as.character(dd$patent_id)
dd$inventor_id<-as.character(dd$inventor_id)
dd$ipc_section<-as.character(dd$ipc_section)
dd$patent_year<-as.character(dd$patent_year)
dd$patent_year<-as.numeric(dd$patent_year)


pyear<-select(dd,patent_id,patent_year)
pyear<-distinct(pyear)

psection<-select(dd,patent_id,ipc_section)
psection<-distinct(psection)

pinventor<-select(dd,patent_id,inventor_id)

pinventor<-distinct(pinventor)


pinventor$inventor <- sequence(rle(pinventor$inventor_id)$lengths)
pinv<-dcast(pinventor,inventor_id ~ inventor,value.var = "patent_id")



pinv<-filter(pinv,!is.na(pinv$`2`)) #delete the isolated patent


base<-list()
for(k in 2:ncol(pinv)){
  expo<-list()
  for (j in 2:ncol(pinv)){
    data <- pinv[c(1,k,j)]
    expo[[j]]<-data%>%drop_na()
  }
  base[[k]]<-expo
}

base<-base[!sapply(base,is.null)]

base2<-list()
for(n in  1:length(base)){
li<-base[[n]]
li<-compact(li)
li<-lapply(li,setNames,c("patent_id","inv_i","inv_j"))
ll<-do.call("rbind.fill",Filter(is.data.frame,li))
base2[[n]]<-ll
}

base2<-Reduce(rbind.fill,base2)
base2<-distinct(base2)
base2<-filter(base2,inv_i!=inv_j)

psection$ipc_section[psection$ipc_section=="e"]<-"E"
psection$ipc_section[psection$ipc_section=="g"]<-"G"
psection$ipc_section[psection$ipc_section=="h"]<-"H"
#section A-H
pA<-psection %>% filter(ipc_section=="A")
pB<-psection %>% filter(ipc_section=="B")
pC<-psection %>% filter(ipc_section=="C")
pD<-psection %>% filter(ipc_section=="D")
pE<-psection %>% filter(ipc_section=="E")
pF<-psection %>% filter(ipc_section=="F")
pG<-psection %>% filter(ipc_section=="G")
pH<-psection %>% filter(ipc_section=="H")

pAH<-Reduce(rbind.fill,list(pA,pB,pC,pD,pE,pF,pG,pH))
pAH$num<-1
base2<-merge(base2,pAH,by.x="patent_id",by.y="patent_id")
base2<-select(base2,-num)

aux1<-select(base2,inv_j)
aux2<-select(base2,inv_i)
names(aux1)[1]<-"inv"
names(aux2)[1]<-"inv"
aux<-rbind.fill(aux1,aux2)
aux<-unique(aux)
aux<-arrange(aux,inv)
aux$num<-ave(aux$inv,FUN=seq_along)
aux$num<-as.numeric(aux$num)

names(aux)[1]<-"inv_i"
base2<-merge(base2,aux,by.x="inv_i",by.y="inv_i")
names(base2)[5]<-"ff"
names(aux)[1]<-"inv_j"
base2<-merge(base2,aux,by.x="inv_j",by.y="inv_j")
names(base2)[6]<-"tt"


base2<-merge(base2,pyear,by.x="patent_id",by.y="patent_id")
base<-select(base2,patent_id,ff,tt)

base<-merge(base,pAH,by.x="patent_id",by.y="patent_id")
#bb es base igual


baseA<-base %>% filter(ipc_section=="A")
baseB<-base %>% filter(ipc_section=="B")
baseC<-base %>% filter(ipc_section=="C")
baseD<-base %>% filter(ipc_section=="D")
baseE<-base %>% filter(ipc_section=="E")
baseF<-base %>% filter(ipc_section=="F")
baseG<-base %>% filter(ipc_section=="G")
baseH<-base %>% filter(ipc_section=="H")


#########################
#network construction

base<-base %>% filter(patent_year>=1999)
##new network

##Total

b1<-base %>% filter(patent_year<=1999)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n1b<-decompose(n1,min.vertices =gcc)
n1b<-n1b[[1]]
diam1<-diameter(n1b)
v1<-as.numeric(length(V(n1b)))
e1<-gsize(n1b)
gcc1<-c1
names(gcc1)[1]<-"size"
gcc1$year<-1999
gcc1$gp<-c("GCC","CC2","CC3")
n1999L<-n1b

b1<-base %>% filter(patent_year<=2000)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n2b<-decompose(n1,min.vertices =gcc)
n2b<-n2b[[1]]
diam2<-diameter(n2b)
v2<-as.numeric(length(V(n2b)))
e2<-gsize(n2b)
gcc2<-c1
names(gcc2)[1]<-"size"
gcc2$year<-2000
gcc2$gp<-c("GCC","CC2","CC3")

b1<-base %>% filter(patent_year<=2001)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n3b<-decompose(n1,min.vertices =gcc)
n3b<-n3b[[1]]
diam3<-diameter(n3b)
v3<-as.numeric(length(V(n3b)))
e3<-gsize(n3b)
gcc3<-c1
names(gcc3)[1]<-"size"
gcc3$year<-2001
gcc3$gp<-c("GCC","CC2","CC3")

b1<-base %>% filter(patent_year<=2002)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n4b<-decompose(n1,min.vertices =gcc)
n4b<-n4b[[1]]
diam4<-diameter(n4b)
v4<-as.numeric(length(V(n4b)))
e4<-gsize(n4b)
gcc4<-c1
names(gcc4)[1]<-"size"
gcc4$year<-2002
gcc4$gp<-c("GCC","CC2","CC3")

b1<-base %>% filter(patent_year<=2003)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n5b<-decompose(n1,min.vertices =gcc)
n5b<-n5b[[1]]
diam5<-diameter(n5b)
v5<-as.numeric(length(V(n5b)))
e5<-gsize(n5b)
gcc5<-c1
names(gcc5)[1]<-"size"
gcc5$year<-2003
gcc5$gp<-c("GCC","CC2","CC3")

b1<-base %>% filter(patent_year<=2004)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n6b<-decompose(n1,min.vertices =gcc)
n6b<-n6b[[1]]
diam6<-diameter(n6b)
v6<-as.numeric(length(V(n6b)))
e6<-gsize(n6b)
gcc6<-c1
names(gcc6)[1]<-"size"
gcc6$year<-2004
gcc6$gp<-c("GCC","CC2","CC3")

b1<-base %>% filter(patent_year<=2005)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n7b<-decompose(n1,min.vertices =gcc)
n7b<-n7b[[1]]
diam7<-diameter(n7b)
v7<-as.numeric(length(V(n7b)))
e7<-gsize(n7b)
gcc7<-c1
names(gcc7)[1]<-"size"
gcc7$year<-2005
gcc7$gp<-c("GCC","CC2","CC3")

b1<-base %>% filter(patent_year<=2006)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n8b<-decompose(n1,min.vertices =gcc)
n8b<-n8b[[1]]
diam8<-diameter(n8b)
v8<-as.numeric(length(V(n8b)))
e8<-gsize(n8b)
gcc8<-c1
names(gcc8)[1]<-"size"
gcc8$year<-2006
gcc8$gp<-c("GCC","CC2","CC3")

b1<-base %>% filter(patent_year<=2007)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n9b<-decompose(n1,min.vertices =gcc)
n9b<-n9b[[1]]
diam9<-diameter(n9b)
v9<-as.numeric(length(V(n9b)))
e9<-gsize(n9b)
gcc9<-c1
names(gcc9)[1]<-"size"
gcc9$year<-2007
gcc9$gp<-c("GCC","CC2","CC3")

b1<-base %>% filter(patent_year<=2008)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n10b<-decompose(n1,min.vertices =gcc)
n10b<-n10b[[1]]
diam10<-diameter(n10b)
v10<-as.numeric(length(V(n10b)))
e10<-gsize(n10b)
gcc10<-c1
names(gcc10)[1]<-"size"
gcc10$year<-2008
gcc10$gp<-c("GCC","CC2","CC3")

b1<-base %>% filter(patent_year<=2009)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n11b<-decompose(n1,min.vertices =gcc)
n11b<-n11b[[1]]
diam11<-diameter(n11b)
v11<-as.numeric(length(V(n11b)))
e11<-gsize(n11b)
gcc11<-c1
names(gcc11)[1]<-"size"
gcc11$year<-2009
gcc11$gp<-c("GCC","CC2","CC3")
n2009L<-n11b

b1<-base %>% filter(patent_year<=2010)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n12b<-decompose(n1,min.vertices =gcc)
n12b<-n12b[[1]]
diam12<-diameter(n12b)
v12<-as.numeric(length(V(n12b)))
e12<-gsize(n12b)
gcc12<-c1
names(gcc12)[1]<-"size"
gcc12$year<-2010
gcc12$gp<-c("GCC","CC2","CC3")

b1<-base %>% filter(patent_year<=2011)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n13b<-decompose(n1,min.vertices =gcc)
n13b<-n13b[[1]]
diam13<-diameter(n13b)
v13<-as.numeric(length(V(n13b)))
e13<-gsize(n13b)
gcc13<-c1
names(gcc13)[1]<-"size"
gcc13$year<-2011
gcc13$gp<-c("GCC","CC2","CC3")

b1<-base %>% filter(patent_year<=2012)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n14b<-decompose(n1,min.vertices =gcc)
n14b<-n14b[[1]]
diam14<-diameter(n14b)
v14<-as.numeric(length(V(n14b)))
e14<-gsize(n14b)
gcc14<-c1
names(gcc14)[1]<-"size"
gcc14$year<-2012
gcc14$gp<-c("GCC","CC2","CC3")

b1<-base %>% filter(patent_year<=2013)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n15b<-decompose(n1,min.vertices =gcc)
n15b<-n15b[[1]]
diam15<-diameter(n15b)
v15<-as.numeric(length(V(n15b)))
e15<-gsize(n15b)
gcc15<-c1
names(gcc15)[1]<-"size"
gcc15$year<-2013
gcc15$gp<-c("GCC","CC2","CC3")

b1<-base %>% filter(patent_year<=2014)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n16b<-decompose(n1,min.vertices =gcc)
n16b<-n16b[[1]]
diam16<-diameter(n16b)
v16<-as.numeric(length(V(n16b)))
e16<-gsize(n16b)
gcc16<-c1
names(gcc16)[1]<-"size"
gcc16$year<-2014
gcc16$gp<-c("GCC","CC2","CC3")

b1<-base %>% filter(patent_year<=2015)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n17b<-decompose(n1,min.vertices =gcc)
n17b<-n17b[[1]]
diam17<-diameter(n17b)
v17<-as.numeric(length(V(n17b)))
e17<-gsize(n17b)
gcc17<-c1
names(gcc17)[1]<-"size"
gcc17$year<-2015
gcc17$gp<-c("GCC","CC2","CC3")

b1<-base %>% filter(patent_year<=2016)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n18b<-decompose(n1,min.vertices =gcc)
n18b<-n18b[[1]]
diam18<-diameter(n18b)
v18<-as.numeric(length(V(n18b)))
e18<-gsize(n18b)
gcc18<-c1
names(gcc18)[1]<-"size"
gcc18$year<-2016
gcc18$gp<-c("GCC","CC2","CC3")

b1<-base %>% filter(patent_year<=2017)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n19b<-decompose(n1,min.vertices =gcc)
n19b<-n19b[[1]]
diam19<-diameter(n19b)
v19<-as.numeric(length(V(n19b)))
e19<-gsize(n19b)
gcc19<-c1
names(gcc19)[1]<-"size"
gcc19$year<-2017
gcc19$gp<-c("GCC","CC2","CC3")

b1<-base %>% filter(patent_year<=2018)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n20b<-decompose(n1,min.vertices =gcc)
n20b<-n20b[[1]]
diam20<-diameter(n20b)
v20<-as.numeric(length(V(n20b)))
e20<-gsize(n20b)
gcc20<-c1
names(gcc20)[1]<-"size"
gcc20$year<-2018
gcc20$gp<-c("GCC","CC2","CC3")

b1<-base %>% filter(patent_year<=2019)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n21b<-decompose(n1,min.vertices =gcc)
n21b<-n21b[[1]]
diam21<-diameter(n21b)
v21<-as.numeric(length(V(n21b)))
e21<-gsize(n21b)
gcc21<-c1
names(gcc21)[1]<-"size"
gcc21$year<-2019
gcc21$gp<-c("GCC","CC2","CC3")
n2019L<-n21b

year<-c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
L<-c(diam1,diam2,diam3,diam4,diam5,diam6,diam7,diam8,diam9,diam10,diam11,diam12,diam13,diam14,diam15,diam16,diam17,diam18,diam19,diam20,diam21)
node<-c(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,v19,v20,v21)
edge<-c(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18,e19,e20,e21)
diamL<-data.frame(year,L)
neL<-data.frame(year,node,edge)
gpL<-Reduce(rbind.fill,list(gcc1,gcc2,gcc3,gcc4,gcc5,gcc6,gcc7,gcc8,gcc9,gcc10,gcc11,gcc12,gcc13,gcc14,gcc15,gcc16,gcc17,gcc18,gcc19,gcc20,gcc21))


#####A

b1<-baseA %>% filter(patent_year<=1999)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n1b<-decompose(n1,min.vertices =gcc)
n1b<-n1b[[1]]
diam1<-diameter(n1b)
v1<-as.numeric(length(V(n1b)))
e1<-gsize(n1b)
gcc1<-c1
names(gcc1)[1]<-"size"
gcc1$year<-1999
gcc1$gp<-c("GCC","CC2","CC3")
n1999A<-n1b

b1<-baseA %>% filter(patent_year<=2000)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n2b<-decompose(n1,min.vertices =gcc)
n2b<-n2b[[1]]
diam2<-diameter(n2b)
v2<-as.numeric(length(V(n2b)))
e2<-gsize(n2b)
gcc2<-c1
names(gcc2)[1]<-"size"
gcc2$year<-2000
gcc2$gp<-c("GCC","CC2","CC3")

b1<-baseA %>% filter(patent_year<=2001)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n3b<-decompose(n1,min.vertices =gcc)
n3b<-n3b[[1]]
diam3<-diameter(n3b)
v3<-as.numeric(length(V(n3b)))
e3<-gsize(n3b)
gcc3<-c1
names(gcc3)[1]<-"size"
gcc3$year<-2001
gcc3$gp<-c("GCC","CC2","CC3")

b1<-baseA %>% filter(patent_year<=2002)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n4b<-decompose(n1,min.vertices =gcc)
n4b<-n4b[[1]]
diam4<-diameter(n4b)
v4<-as.numeric(length(V(n4b)))
e4<-gsize(n4b)
gcc4<-c1
names(gcc4)[1]<-"size"
gcc4$year<-2002
gcc4$gp<-c("GCC","CC2","CC3")

b1<-baseA %>% filter(patent_year<=2003)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n5b<-decompose(n1,min.vertices =gcc)
n5b<-n5b[[1]]
diam5<-diameter(n5b)
v5<-as.numeric(length(V(n5b)))
e5<-gsize(n5b)
gcc5<-c1
names(gcc5)[1]<-"size"
gcc5$year<-2003
gcc5$gp<-c("GCC","CC2","CC3")

b1<-baseA %>% filter(patent_year<=2004)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n6b<-decompose(n1,min.vertices =gcc)
n6b<-n6b[[1]]
diam6<-diameter(n6b)
v6<-as.numeric(length(V(n6b)))
e6<-gsize(n6b)
gcc6<-c1
names(gcc6)[1]<-"size"
gcc6$year<-2004
gcc6$gp<-c("GCC","CC2","CC3")

b1<-baseA %>% filter(patent_year<=2005)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n7b<-decompose(n1,min.vertices =gcc)
n7b<-n7b[[1]]
diam7<-diameter(n7b)
v7<-as.numeric(length(V(n7b)))
e7<-gsize(n7b)
gcc7<-c1
names(gcc7)[1]<-"size"
gcc7$year<-2005
gcc7$gp<-c("GCC","CC2","CC3")

b1<-baseA %>% filter(patent_year<=2006)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n8b<-decompose(n1,min.vertices =gcc)
n8b<-n8b[[1]]
diam8<-diameter(n8b)
v8<-as.numeric(length(V(n8b)))
e8<-gsize(n8b)
gcc8<-c1
names(gcc8)[1]<-"size"
gcc8$year<-2006
gcc8$gp<-c("GCC","CC2","CC3")

b1<-baseA %>% filter(patent_year<=2007)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n9b<-decompose(n1,min.vertices =gcc)
n9b<-n9b[[1]]
diam9<-diameter(n9b)
v9<-as.numeric(length(V(n9b)))
e9<-gsize(n9b)
gcc9<-c1
names(gcc9)[1]<-"size"
gcc9$year<-2007
gcc9$gp<-c("GCC","CC2","CC3")

b1<-baseA %>% filter(patent_year<=2008)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n10b<-decompose(n1,min.vertices =gcc)
n10b<-n10b[[1]]
diam10<-diameter(n10b)
v10<-as.numeric(length(V(n10b)))
e10<-gsize(n10b)
gcc10<-c1
names(gcc10)[1]<-"size"
gcc10$year<-2008
gcc10$gp<-c("GCC","CC2","CC3")

b1<-baseA %>% filter(patent_year<=2009)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n11b<-decompose(n1,min.vertices =gcc)
n11b<-n11b[[1]]
diam11<-diameter(n11b)
v11<-as.numeric(length(V(n11b)))
e11<-gsize(n11b)
gcc11<-c1
names(gcc11)[1]<-"size"
gcc11$year<-2009
gcc11$gp<-c("GCC","CC2","CC3")
n2009A<-n11b

b1<-baseA %>% filter(patent_year<=2010)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n12b<-decompose(n1,min.vertices =gcc)
n12b<-n12b[[1]]
diam12<-diameter(n12b)
v12<-as.numeric(length(V(n12b)))
e12<-gsize(n12b)
gcc12<-c1
names(gcc12)[1]<-"size"
gcc12$year<-2010
gcc12$gp<-c("GCC","CC2","CC3")

b1<-baseA %>% filter(patent_year<=2011)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n13b<-decompose(n1,min.vertices =gcc)
n13b<-n13b[[1]]
diam13<-diameter(n13b)
v13<-as.numeric(length(V(n13b)))
e13<-gsize(n13b)
gcc13<-c1
names(gcc13)[1]<-"size"
gcc13$year<-2011
gcc13$gp<-c("GCC","CC2","CC3")

b1<-baseA %>% filter(patent_year<=2012)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n14b<-decompose(n1,min.vertices =gcc)
n14b<-n14b[[1]]
diam14<-diameter(n14b)
v14<-as.numeric(length(V(n14b)))
e14<-gsize(n14b)
gcc14<-c1
names(gcc14)[1]<-"size"
gcc14$year<-2012
gcc14$gp<-c("GCC","CC2","CC3")

b1<-baseA %>% filter(patent_year<=2013)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n15b<-decompose(n1,min.vertices =gcc)
n15b<-n15b[[1]]
diam15<-diameter(n15b)
v15<-as.numeric(length(V(n15b)))
e15<-gsize(n15b)
gcc15<-c1
names(gcc15)[1]<-"size"
gcc15$year<-2013
gcc15$gp<-c("GCC","CC2","CC3")

b1<-baseA %>% filter(patent_year<=2014)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n16b<-decompose(n1,min.vertices =gcc)
n16b<-n16b[[1]]
diam16<-diameter(n16b)
v16<-as.numeric(length(V(n16b)))
e16<-gsize(n16b)
gcc16<-c1
names(gcc16)[1]<-"size"
gcc16$year<-2014
gcc16$gp<-c("GCC","CC2","CC3")

b1<-baseA %>% filter(patent_year<=2015)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n17b<-decompose(n1,min.vertices =gcc)
n17b<-n17b[[1]]
diam17<-diameter(n17b)
v17<-as.numeric(length(V(n17b)))
e17<-gsize(n17b)
gcc17<-c1
names(gcc17)[1]<-"size"
gcc17$year<-2015
gcc17$gp<-c("GCC","CC2","CC3")

b1<-baseA %>% filter(patent_year<=2016)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n18b<-decompose(n1,min.vertices =gcc)
n18b<-n18b[[1]]
diam18<-diameter(n18b)
v18<-as.numeric(length(V(n18b)))
e18<-gsize(n18b)
gcc18<-c1
names(gcc18)[1]<-"size"
gcc18$year<-2016
gcc18$gp<-c("GCC","CC2","CC3")

b1<-baseA %>% filter(patent_year<=2017)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n19b<-decompose(n1,min.vertices =gcc)
n19b<-n19b[[1]]
diam19<-diameter(n19b)
v19<-as.numeric(length(V(n19b)))
e19<-gsize(n19b)
gcc19<-c1
names(gcc19)[1]<-"size"
gcc19$year<-2017
gcc19$gp<-c("GCC","CC2","CC3")

b1<-baseA %>% filter(patent_year<=2018)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n20b<-decompose(n1,min.vertices =gcc)
n20b<-n20b[[1]]
diam20<-diameter(n20b)
v20<-as.numeric(length(V(n20b)))
e20<-gsize(n20b)
gcc20<-c1
names(gcc20)[1]<-"size"
gcc20$year<-2018
gcc20$gp<-c("GCC","CC2","CC3")

b1<-baseA %>% filter(patent_year<=2019)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n21b<-decompose(n1,min.vertices =gcc)
n21b<-n21b[[1]]
diam21<-diameter(n21b)
v21<-as.numeric(length(V(n21b)))
e21<-gsize(n21b)
gcc21<-c1
names(gcc21)[1]<-"size"
gcc21$year<-2019
gcc21$gp<-c("GCC","CC2","CC3")
n2019A<-n21b

year<-c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
A<-c(diam1,diam2,diam3,diam4,diam5,diam6,diam7,diam8,diam9,diam10,diam11,diam12,diam13,diam14,diam15,diam16,diam17,diam18,diam19,diam20,diam21)
node<-c(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,v19,v20,v21)
edge<-c(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18,e19,e20,e21)
diamA<-data.frame(year,A)
neA<-data.frame(year,node,edge)
gpA<-Reduce(rbind.fill,list(gcc1,gcc2,gcc3,gcc4,gcc5,gcc6,gcc7,gcc8,gcc9,gcc10,gcc11,gcc12,gcc13,gcc14,gcc15,gcc16,gcc17,gcc18,gcc19,gcc20,gcc21))


###B

b1<-baseB %>% filter(patent_year<=1999)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n1b<-decompose(n1,min.vertices =gcc)
n1b<-n1b[[1]]
diam1<-diameter(n1b)
v1<-as.numeric(length(V(n1b)))
e1<-gsize(n1b)
gcc1<-c1
names(gcc1)[1]<-"size"
gcc1$year<-1999
gcc1$gp<-c("GCC","CC2","CC3")
n1999B<-n1b

b1<-baseB %>% filter(patent_year<=2000)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n2b<-decompose(n1,min.vertices =gcc)
n2b<-n2b[[1]]
diam2<-diameter(n2b)
v2<-as.numeric(length(V(n2b)))
e2<-gsize(n2b)
gcc2<-c1
names(gcc2)[1]<-"size"
gcc2$year<-2000
gcc2$gp<-c("GCC","CC2","CC3")

b1<-baseB %>% filter(patent_year<=2001)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n3b<-decompose(n1,min.vertices =gcc)
n3b<-n3b[[1]]
diam3<-diameter(n3b)
v3<-as.numeric(length(V(n3b)))
e3<-gsize(n3b)
gcc3<-c1
names(gcc3)[1]<-"size"
gcc3$year<-2001
gcc3$gp<-c("GCC","CC2","CC3")

b1<-baseB %>% filter(patent_year<=2002)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n4b<-decompose(n1,min.vertices =gcc)
n4b<-n4b[[1]]
diam4<-diameter(n4b)
v4<-as.numeric(length(V(n4b)))
e4<-gsize(n4b)
gcc4<-c1
names(gcc4)[1]<-"size"
gcc4$year<-2002
gcc4$gp<-c("GCC","CC2","CC3")

b1<-baseB %>% filter(patent_year<=2003)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n5b<-decompose(n1,min.vertices =gcc)
n5b<-n5b[[1]]
diam5<-diameter(n5b)
v5<-as.numeric(length(V(n5b)))
e5<-gsize(n5b)
gcc5<-c1
names(gcc5)[1]<-"size"
gcc5$year<-2003
gcc5$gp<-c("GCC","CC2","CC3")

b1<-baseB %>% filter(patent_year<=2004)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n6b<-decompose(n1,min.vertices =gcc)
n6b<-n6b[[1]]
diam6<-diameter(n6b)
v6<-as.numeric(length(V(n6b)))
e6<-gsize(n6b)
gcc6<-c1
names(gcc6)[1]<-"size"
gcc6$year<-2004
gcc6$gp<-c("GCC","CC2","CC3")

b1<-baseB %>% filter(patent_year<=2005)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n7b<-decompose(n1,min.vertices =gcc)
n7b<-n7b[[1]]
diam7<-diameter(n7b)
v7<-as.numeric(length(V(n7b)))
e7<-gsize(n7b)
gcc7<-c1
names(gcc7)[1]<-"size"
gcc7$year<-2005
gcc7$gp<-c("GCC","CC2","CC3")

b1<-baseB %>% filter(patent_year<=2006)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n8b<-decompose(n1,min.vertices =gcc)
n8b<-n8b[[1]]
diam8<-diameter(n8b)
v8<-as.numeric(length(V(n8b)))
e8<-gsize(n8b)
gcc8<-c1
names(gcc8)[1]<-"size"
gcc8$year<-2006
gcc8$gp<-c("GCC","CC2","CC3")

b1<-baseB %>% filter(patent_year<=2007)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n9b<-decompose(n1,min.vertices =gcc)
n9b<-n9b[[1]]
diam9<-diameter(n9b)
v9<-as.numeric(length(V(n9b)))
e9<-gsize(n9b)
gcc9<-c1
names(gcc9)[1]<-"size"
gcc9$year<-2007
gcc9$gp<-c("GCC","CC2","CC3")

b1<-baseB %>% filter(patent_year<=2008)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n10b<-decompose(n1,min.vertices =gcc)
n10b<-n10b[[1]]
diam10<-diameter(n10b)
v10<-as.numeric(length(V(n10b)))
e10<-gsize(n10b)
gcc10<-c1
names(gcc10)[1]<-"size"
gcc10$year<-2008
gcc10$gp<-c("GCC","CC2","CC3")

b1<-baseB %>% filter(patent_year<=2009)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n11b<-decompose(n1,min.vertices =gcc)
n11b<-n11b[[1]]
diam11<-diameter(n11b)
v11<-as.numeric(length(V(n11b)))
e11<-gsize(n11b)
gcc11<-c1
names(gcc11)[1]<-"size"
gcc11$year<-2009
gcc11$gp<-c("GCC","CC2","CC3")
n2009B<-n11b

b1<-baseB %>% filter(patent_year<=2010)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n12b<-decompose(n1,min.vertices =gcc)
n12b<-n12b[[1]]
diam12<-diameter(n12b)
v12<-as.numeric(length(V(n12b)))
e12<-gsize(n12b)
gcc12<-c1
names(gcc12)[1]<-"size"
gcc12$year<-2010
gcc12$gp<-c("GCC","CC2","CC3")

b1<-baseB %>% filter(patent_year<=2011)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n13b<-decompose(n1,min.vertices =gcc)
n13b<-n13b[[1]]
diam13<-diameter(n13b)
v13<-as.numeric(length(V(n13b)))
e13<-gsize(n13b)
gcc13<-c1
names(gcc13)[1]<-"size"
gcc13$year<-2011
gcc13$gp<-c("GCC","CC2","CC3")

b1<-baseB %>% filter(patent_year<=2012)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n14b<-decompose(n1,min.vertices =gcc)
n14b<-n14b[[1]]
diam14<-diameter(n14b)
v14<-as.numeric(length(V(n14b)))
e14<-gsize(n14b)
gcc14<-c1
names(gcc14)[1]<-"size"
gcc14$year<-2012
gcc14$gp<-c("GCC","CC2","CC3")

b1<-baseB %>% filter(patent_year<=2013)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n15b<-decompose(n1,min.vertices =gcc)
n15b<-n15b[[1]]
diam15<-diameter(n15b)
v15<-as.numeric(length(V(n15b)))
e15<-gsize(n15b)
gcc15<-c1
names(gcc15)[1]<-"size"
gcc15$year<-2013
gcc15$gp<-c("GCC","CC2","CC3")

b1<-baseB %>% filter(patent_year<=2014)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n16b<-decompose(n1,min.vertices =gcc)
n16b<-n16b[[1]]
diam16<-diameter(n16b)
v16<-as.numeric(length(V(n16b)))
e16<-gsize(n16b)
gcc16<-c1
names(gcc16)[1]<-"size"
gcc16$year<-2014
gcc16$gp<-c("GCC","CC2","CC3")

b1<-baseB %>% filter(patent_year<=2015)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n17b<-decompose(n1,min.vertices =gcc)
n17b<-n17b[[1]]
diam17<-diameter(n17b)
v17<-as.numeric(length(V(n17b)))
e17<-gsize(n17b)
gcc17<-c1
names(gcc17)[1]<-"size"
gcc17$year<-2015
gcc17$gp<-c("GCC","CC2","CC3")

b1<-baseB %>% filter(patent_year<=2016)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n18b<-decompose(n1,min.vertices =gcc)
n18b<-n18b[[1]]
diam18<-diameter(n18b)
v18<-as.numeric(length(V(n18b)))
e18<-gsize(n18b)
gcc18<-c1
names(gcc18)[1]<-"size"
gcc18$year<-2016
gcc18$gp<-c("GCC","CC2","CC3")

b1<-baseB %>% filter(patent_year<=2017)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n19b<-decompose(n1,min.vertices =gcc)
n19b<-n19b[[1]]
diam19<-diameter(n19b)
v19<-as.numeric(length(V(n19b)))
e19<-gsize(n19b)
gcc19<-c1
names(gcc19)[1]<-"size"
gcc19$year<-2017
gcc19$gp<-c("GCC","CC2","CC3")

b1<-baseB %>% filter(patent_year<=2018)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n20b<-decompose(n1,min.vertices =gcc)
n20b<-n20b[[1]]
diam20<-diameter(n20b)
v20<-as.numeric(length(V(n20b)))
e20<-gsize(n20b)
gcc20<-c1
names(gcc20)[1]<-"size"
gcc20$year<-2018
gcc20$gp<-c("GCC","CC2","CC3")

b1<-baseB %>% filter(patent_year<=2019)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n21b<-decompose(n1,min.vertices =gcc)
n21b<-n21b[[1]]
diam21<-diameter(n21b)
v21<-as.numeric(length(V(n21b)))
e21<-gsize(n21b)
gcc21<-c1
names(gcc21)[1]<-"size"
gcc21$year<-2019
gcc21$gp<-c("GCC","CC2","CC3")
n2019B<-n21b

year<-c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
B<-c(diam1,diam2,diam3,diam4,diam5,diam6,diam7,diam8,diam9,diam10,diam11,diam12,diam13,diam14,diam15,diam16,diam17,diam18,diam19,diam20,diam21)
node<-c(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,v19,v20,v21)
edge<-c(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18,e19,e20,e21)
diamB<-data.frame(year,B)
neB<-data.frame(year,node,edge)
gpB<-Reduce(rbind.fill,list(gcc1,gcc2,gcc3,gcc4,gcc5,gcc6,gcc7,gcc8,gcc9,gcc10,gcc11,gcc12,gcc13,gcc14,gcc15,gcc16,gcc17,gcc18,gcc19,gcc20,gcc21))


##C

b1<-baseC %>% filter(patent_year<=1999)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n1b<-decompose(n1,min.vertices =gcc)
n1b<-n1b[[1]]
diam1<-diameter(n1b)
v1<-as.numeric(length(V(n1b)))
e1<-gsize(n1b)
gcc1<-c1
names(gcc1)[1]<-"size"
gcc1$year<-1999
gcc1$gp<-c("GCC","CC2","CC3")
n1999C<-n1b

b1<-baseC %>% filter(patent_year<=2000)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n2b<-decompose(n1,min.vertices =gcc)
n2b<-n2b[[1]]
diam2<-diameter(n2b)
v2<-as.numeric(length(V(n2b)))
e2<-gsize(n2b)
gcc2<-c1
names(gcc2)[1]<-"size"
gcc2$year<-2000
gcc2$gp<-c("GCC","CC2","CC3")

b1<-baseC %>% filter(patent_year<=2001)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n3b<-decompose(n1,min.vertices =gcc)
n3b<-n3b[[1]]
diam3<-diameter(n3b)
v3<-as.numeric(length(V(n3b)))
e3<-gsize(n3b)
gcc3<-c1
names(gcc3)[1]<-"size"
gcc3$year<-2001
gcc3$gp<-c("GCC","CC2","CC3")

b1<-baseC %>% filter(patent_year<=2002)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n4b<-decompose(n1,min.vertices =gcc)
n4b<-n4b[[1]]
diam4<-diameter(n4b)
v4<-as.numeric(length(V(n4b)))
e4<-gsize(n4b)
gcc4<-c1
names(gcc4)[1]<-"size"
gcc4$year<-2002
gcc4$gp<-c("GCC","CC2","CC3")

b1<-baseC %>% filter(patent_year<=2003)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n5b<-decompose(n1,min.vertices =gcc)
n5b<-n5b[[1]]
diam5<-diameter(n5b)
v5<-as.numeric(length(V(n5b)))
e5<-gsize(n5b)
gcc5<-c1
names(gcc5)[1]<-"size"
gcc5$year<-2003
gcc5$gp<-c("GCC","CC2","CC3")

b1<-baseC %>% filter(patent_year<=2004)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n6b<-decompose(n1,min.vertices =gcc)
n6b<-n6b[[1]]
diam6<-diameter(n6b)
v6<-as.numeric(length(V(n6b)))
e6<-gsize(n6b)
gcc6<-c1
names(gcc6)[1]<-"size"
gcc6$year<-2004
gcc6$gp<-c("GCC","CC2","CC3")

b1<-baseC %>% filter(patent_year<=2005)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n7b<-decompose(n1,min.vertices =gcc)
n7b<-n7b[[1]]
diam7<-diameter(n7b)
v7<-as.numeric(length(V(n7b)))
e7<-gsize(n7b)
gcc7<-c1
names(gcc7)[1]<-"size"
gcc7$year<-2005
gcc7$gp<-c("GCC","CC2","CC3")

b1<-baseC %>% filter(patent_year<=2006)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n8b<-decompose(n1,min.vertices =gcc)
n8b<-n8b[[1]]
diam8<-diameter(n8b)
v8<-as.numeric(length(V(n8b)))
e8<-gsize(n8b)
gcc8<-c1
names(gcc8)[1]<-"size"
gcc8$year<-2006
gcc8$gp<-c("GCC","CC2","CC3")

b1<-baseC %>% filter(patent_year<=2007)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n9b<-decompose(n1,min.vertices =gcc)
n9b<-n9b[[1]]
diam9<-diameter(n9b)
v9<-as.numeric(length(V(n9b)))
e9<-gsize(n9b)
gcc9<-c1
names(gcc9)[1]<-"size"
gcc9$year<-2007
gcc9$gp<-c("GCC","CC2","CC3")

b1<-baseC %>% filter(patent_year<=2008)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n10b<-decompose(n1,min.vertices =gcc)
n10b<-n10b[[1]]
diam10<-diameter(n10b)
v10<-as.numeric(length(V(n10b)))
e10<-gsize(n10b)
gcc10<-c1
names(gcc10)[1]<-"size"
gcc10$year<-2008
gcc10$gp<-c("GCC","CC2","CC3")

b1<-baseC %>% filter(patent_year<=2009)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n11b<-decompose(n1,min.vertices =gcc)
n11b<-n11b[[1]]
diam11<-diameter(n11b)
v11<-as.numeric(length(V(n11b)))
e11<-gsize(n11b)
gcc11<-c1
names(gcc11)[1]<-"size"
gcc11$year<-2009
gcc11$gp<-c("GCC","CC2","CC3")
n2009C<-n11b

b1<-baseC %>% filter(patent_year<=2010)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n12b<-decompose(n1,min.vertices =gcc)
n12b<-n12b[[1]]
diam12<-diameter(n12b)
v12<-as.numeric(length(V(n12b)))
e12<-gsize(n12b)
gcc12<-c1
names(gcc12)[1]<-"size"
gcc12$year<-2010
gcc12$gp<-c("GCC","CC2","CC3")

b1<-baseC %>% filter(patent_year<=2011)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n13b<-decompose(n1,min.vertices =gcc)
n13b<-n13b[[1]]
diam13<-diameter(n13b)
v13<-as.numeric(length(V(n13b)))
e13<-gsize(n13b)
gcc13<-c1
names(gcc13)[1]<-"size"
gcc13$year<-2011
gcc13$gp<-c("GCC","CC2","CC3")

b1<-baseC %>% filter(patent_year<=2012)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n14b<-decompose(n1,min.vertices =gcc)
n14b<-n14b[[1]]
diam14<-diameter(n14b)
v14<-as.numeric(length(V(n14b)))
e14<-gsize(n14b)
gcc14<-c1
names(gcc14)[1]<-"size"
gcc14$year<-2012
gcc14$gp<-c("GCC","CC2","CC3")

b1<-baseC %>% filter(patent_year<=2013)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n15b<-decompose(n1,min.vertices =gcc)
n15b<-n15b[[1]]
diam15<-diameter(n15b)
v15<-as.numeric(length(V(n15b)))
e15<-gsize(n15b)
gcc15<-c1
names(gcc15)[1]<-"size"
gcc15$year<-2013
gcc15$gp<-c("GCC","CC2","CC3")

b1<-baseC %>% filter(patent_year<=2014)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n16b<-decompose(n1,min.vertices =gcc)
n16b<-n16b[[1]]
diam16<-diameter(n16b)
v16<-as.numeric(length(V(n16b)))
e16<-gsize(n16b)
gcc16<-c1
names(gcc16)[1]<-"size"
gcc16$year<-2014
gcc16$gp<-c("GCC","CC2","CC3")

b1<-baseC %>% filter(patent_year<=2015)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n17b<-decompose(n1,min.vertices =gcc)
n17b<-n17b[[1]]
diam17<-diameter(n17b)
v17<-as.numeric(length(V(n17b)))
e17<-gsize(n17b)
gcc17<-c1
names(gcc17)[1]<-"size"
gcc17$year<-2015
gcc17$gp<-c("GCC","CC2","CC3")

b1<-baseC %>% filter(patent_year<=2016)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n18b<-decompose(n1,min.vertices =gcc)
n18b<-n18b[[1]]
diam18<-diameter(n18b)
v18<-as.numeric(length(V(n18b)))
e18<-gsize(n18b)
gcc18<-c1
names(gcc18)[1]<-"size"
gcc18$year<-2016
gcc18$gp<-c("GCC","CC2","CC3")

b1<-baseC %>% filter(patent_year<=2017)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n19b<-decompose(n1,min.vertices =gcc)
n19b<-n19b[[1]]
diam19<-diameter(n19b)
v19<-as.numeric(length(V(n19b)))
e19<-gsize(n19b)
gcc19<-c1
names(gcc19)[1]<-"size"
gcc19$year<-2017
gcc19$gp<-c("GCC","CC2","CC3")

b1<-baseC %>% filter(patent_year<=2018)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n20b<-decompose(n1,min.vertices =gcc)
n20b<-n20b[[1]]
diam20<-diameter(n20b)
v20<-as.numeric(length(V(n20b)))
e20<-gsize(n20b)
gcc20<-c1
names(gcc20)[1]<-"size"
gcc20$year<-2018
gcc20$gp<-c("GCC","CC2","CC3")

b1<-baseC %>% filter(patent_year<=2019)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n21b<-decompose(n1,min.vertices =gcc)
n21b<-n21b[[1]]
diam21<-diameter(n21b)
v21<-as.numeric(length(V(n21b)))
e21<-gsize(n21b)
gcc21<-c1
names(gcc21)[1]<-"size"
gcc21$year<-2019
gcc21$gp<-c("GCC","CC2","CC3")
n2019C<-n21b

year<-c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
C<-c(diam1,diam2,diam3,diam4,diam5,diam6,diam7,diam8,diam9,diam10,diam11,diam12,diam13,diam14,diam15,diam16,diam17,diam18,diam19,diam20,diam21)
node<-c(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,v19,v20,v21)
edge<-c(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18,e19,e20,e21)
diamC<-data.frame(year,C)
neC<-data.frame(year,node,edge)
gpC<-Reduce(rbind.fill,list(gcc1,gcc2,gcc3,gcc4,gcc5,gcc6,gcc7,gcc8,gcc9,gcc10,gcc11,gcc12,gcc13,gcc14,gcc15,gcc16,gcc17,gcc18,gcc19,gcc20,gcc21))

##D

b1<-baseD %>% filter(patent_year<=1999)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n1b<-decompose(n1,min.vertices =gcc)
n1b<-n1b[[1]]
diam1<-diameter(n1b)
v1<-as.numeric(length(V(n1b)))
e1<-gsize(n1b)
gcc1<-c1
names(gcc1)[1]<-"size"
gcc1$year<-1999
gcc1$gp<-c("GCC","CC2","CC3")
n1999D<-n1b

b1<-baseD %>% filter(patent_year<=2000)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n2b<-decompose(n1,min.vertices =gcc)
n2b<-n2b[[1]]
diam2<-diameter(n2b)
v2<-as.numeric(length(V(n2b)))
e2<-gsize(n2b)
gcc2<-c1
names(gcc2)[1]<-"size"
gcc2$year<-2000
gcc2$gp<-c("GCC","CC2","CC3")

b1<-baseD %>% filter(patent_year<=2001)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n3b<-decompose(n1,min.vertices =gcc)
n3b<-n3b[[1]]
diam3<-diameter(n3b)
v3<-as.numeric(length(V(n3b)))
e3<-gsize(n3b)
gcc3<-c1
names(gcc3)[1]<-"size"
gcc3$year<-2001
gcc3$gp<-c("GCC","CC2","CC3")

b1<-baseD %>% filter(patent_year<=2002)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n4b<-decompose(n1,min.vertices =gcc)
n4b<-n4b[[1]]
diam4<-diameter(n4b)
v4<-as.numeric(length(V(n4b)))
e4<-gsize(n4b)
gcc4<-c1
names(gcc4)[1]<-"size"
gcc4$year<-2002
gcc4$gp<-c("GCC","CC2","CC3")

b1<-baseD %>% filter(patent_year<=2003)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n5b<-decompose(n1,min.vertices =gcc)
n5b<-n5b[[1]]
diam5<-diameter(n5b)
v5<-as.numeric(length(V(n5b)))
e5<-gsize(n5b)
gcc5<-c1
names(gcc5)[1]<-"size"
gcc5$year<-2003
gcc5$gp<-c("GCC","CC2","CC3")

b1<-baseD %>% filter(patent_year<=2004)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n6b<-decompose(n1,min.vertices =gcc)
n6b<-n6b[[1]]
diam6<-diameter(n6b)
v6<-as.numeric(length(V(n6b)))
e6<-gsize(n6b)
gcc6<-c1
names(gcc6)[1]<-"size"
gcc6$year<-2004
gcc6$gp<-c("GCC","CC2","CC3")

b1<-baseD %>% filter(patent_year<=2005)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n7b<-decompose(n1,min.vertices =gcc)
n7b<-n7b[[1]]
diam7<-diameter(n7b)
v7<-as.numeric(length(V(n7b)))
e7<-gsize(n7b)
gcc7<-c1
names(gcc7)[1]<-"size"
gcc7$year<-2005
gcc7$gp<-c("GCC","CC2","CC3")

b1<-baseD %>% filter(patent_year<=2006)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n8b<-decompose(n1,min.vertices =gcc)
n8b<-n8b[[1]]
diam8<-diameter(n8b)
v8<-as.numeric(length(V(n8b)))
e8<-gsize(n8b)
gcc8<-c1
names(gcc8)[1]<-"size"
gcc8$year<-2006
gcc8$gp<-c("GCC","CC2","CC3")

b1<-baseD %>% filter(patent_year<=2007)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n9b<-decompose(n1,min.vertices =gcc)
n9b<-n9b[[1]]
diam9<-diameter(n9b)
v9<-as.numeric(length(V(n9b)))
e9<-gsize(n9b)
gcc9<-c1
names(gcc9)[1]<-"size"
gcc9$year<-2007
gcc9$gp<-c("GCC","CC2","CC3")

b1<-baseD %>% filter(patent_year<=2008)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n10b<-decompose(n1,min.vertices =gcc)
n10b<-n10b[[1]]
diam10<-diameter(n10b)
v10<-as.numeric(length(V(n10b)))
e10<-gsize(n10b)
gcc10<-c1
names(gcc10)[1]<-"size"
gcc10$year<-2008
gcc10$gp<-c("GCC","CC2","CC3")

b1<-baseD %>% filter(patent_year<=2009)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n11b<-decompose(n1,min.vertices =gcc)
n11b<-n11b[[1]]
diam11<-diameter(n11b)
v11<-as.numeric(length(V(n11b)))
e11<-gsize(n11b)
gcc11<-c1
names(gcc11)[1]<-"size"
gcc11$year<-2009
gcc11$gp<-c("GCC","CC2","CC3")
n2009D<-n11b

b1<-baseD %>% filter(patent_year<=2010)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n12b<-decompose(n1,min.vertices =gcc)
n12b<-n12b[[1]]
diam12<-diameter(n12b)
v12<-as.numeric(length(V(n12b)))
e12<-gsize(n12b)
gcc12<-c1
names(gcc12)[1]<-"size"
gcc12$year<-2010
gcc12$gp<-c("GCC","CC2","CC3")

b1<-baseD %>% filter(patent_year<=2011)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n13b<-decompose(n1,min.vertices =gcc)
n13b<-n13b[[1]]
diam13<-diameter(n13b)
v13<-as.numeric(length(V(n13b)))
e13<-gsize(n13b)
gcc13<-c1
names(gcc13)[1]<-"size"
gcc13$year<-2011
gcc13$gp<-c("GCC","CC2","CC3")

b1<-baseD %>% filter(patent_year<=2012)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n14b<-decompose(n1,min.vertices =gcc)
n14b<-n14b[[1]]
diam14<-diameter(n14b)
v14<-as.numeric(length(V(n14b)))
e14<-gsize(n14b)
gcc14<-c1
names(gcc14)[1]<-"size"
gcc14$year<-2012
gcc14$gp<-c("GCC","CC2","CC3")

b1<-baseD %>% filter(patent_year<=2013)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n15b<-decompose(n1,min.vertices =gcc)
n15b<-n15b[[1]]
diam15<-diameter(n15b)
v15<-as.numeric(length(V(n15b)))
e15<-gsize(n15b)
gcc15<-c1
names(gcc15)[1]<-"size"
gcc15$year<-2013
gcc15$gp<-c("GCC","CC2","CC3")

b1<-baseD %>% filter(patent_year<=2014)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n16b<-decompose(n1,min.vertices =gcc)
n16b<-n16b[[1]]
diam16<-diameter(n16b)
v16<-as.numeric(length(V(n16b)))
e16<-gsize(n16b)
gcc16<-c1
names(gcc16)[1]<-"size"
gcc16$year<-2014
gcc16$gp<-c("GCC","CC2","CC3")

b1<-baseD %>% filter(patent_year<=2015)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n17b<-decompose(n1,min.vertices =gcc)
n17b<-n17b[[1]]
diam17<-diameter(n17b)
v17<-as.numeric(length(V(n17b)))
e17<-gsize(n17b)
gcc17<-c1
names(gcc17)[1]<-"size"
gcc17$year<-2015
gcc17$gp<-c("GCC","CC2","CC3")

b1<-baseD %>% filter(patent_year<=2016)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n18b<-decompose(n1,min.vertices =gcc)
n18b<-n18b[[1]]
diam18<-diameter(n18b)
v18<-as.numeric(length(V(n18b)))
e18<-gsize(n18b)
gcc18<-c1
names(gcc18)[1]<-"size"
gcc18$year<-2016
gcc18$gp<-c("GCC","CC2","CC3")

b1<-baseD %>% filter(patent_year<=2017)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n19b<-decompose(n1,min.vertices =gcc)
n19b<-n19b[[1]]
diam19<-diameter(n19b)
v19<-as.numeric(length(V(n19b)))
e19<-gsize(n19b)
gcc19<-c1
names(gcc19)[1]<-"size"
gcc19$year<-2017
gcc19$gp<-c("GCC","CC2","CC3")

b1<-baseD %>% filter(patent_year<=2018)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n20b<-decompose(n1,min.vertices =gcc)
n20b<-n20b[[1]]
diam20<-diameter(n20b)
v20<-as.numeric(length(V(n20b)))
e20<-gsize(n20b)
gcc20<-c1
names(gcc20)[1]<-"size"
gcc20$year<-2018
gcc20$gp<-c("GCC","CC2","CC3")

b1<-baseD %>% filter(patent_year<=2019)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n21b<-decompose(n1,min.vertices =gcc)
n21b<-n21b[[1]]
diam21<-diameter(n21b)
v21<-as.numeric(length(V(n21b)))
e21<-gsize(n21b)
gcc21<-c1
names(gcc21)[1]<-"size"
gcc21$year<-2019
gcc21$gp<-c("GCC","CC2","CC3")
n2019D<-n21b

year<-c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
D<-c(diam1,diam2,diam3,diam4,diam5,diam6,diam7,diam8,diam9,diam10,diam11,diam12,diam13,diam14,diam15,diam16,diam17,diam18,diam19,diam20,diam21)
node<-c(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,v19,v20,v21)
edge<-c(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18,e19,e20,e21)
diamD<-data.frame(year,D)
neD<-data.frame(year,node,edge)
gpD<-Reduce(rbind.fill,list(gcc1,gcc2,gcc3,gcc4,gcc5,gcc6,gcc7,gcc8,gcc9,gcc10,gcc11,gcc12,gcc13,gcc14,gcc15,gcc16,gcc17,gcc18,gcc19,gcc20,gcc21))

#E
b1<-baseE %>% filter(patent_year<=1999)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n1b<-decompose(n1,min.vertices =gcc)
n1b<-n1b[[1]]
diam1<-diameter(n1b)
v1<-as.numeric(length(V(n1b)))
e1<-gsize(n1b)
gcc1<-c1
names(gcc1)[1]<-"size"
gcc1$year<-1999
gcc1$gp<-c("GCC","CC2","CC3")
n1999E<-n1b

b1<-baseE %>% filter(patent_year<=2000)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n2b<-decompose(n1,min.vertices =gcc)
n2b<-n2b[[1]]
diam2<-diameter(n2b)
v2<-as.numeric(length(V(n2b)))
e2<-gsize(n2b)
gcc2<-c1
names(gcc2)[1]<-"size"
gcc2$year<-2000
gcc2$gp<-c("GCC","CC2","CC3")

b1<-baseE %>% filter(patent_year<=2001)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n3b<-decompose(n1,min.vertices =gcc)
n3b<-n3b[[1]]
diam3<-diameter(n3b)
v3<-as.numeric(length(V(n3b)))
e3<-gsize(n3b)
gcc3<-c1
names(gcc3)[1]<-"size"
gcc3$year<-2001
gcc3$gp<-c("GCC","CC2","CC3")

b1<-baseE %>% filter(patent_year<=2002)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n4b<-decompose(n1,min.vertices =gcc)
n4b<-n4b[[1]]
diam4<-diameter(n4b)
v4<-as.numeric(length(V(n4b)))
e4<-gsize(n4b)
gcc4<-c1
names(gcc4)[1]<-"size"
gcc4$year<-2002
gcc4$gp<-c("GCC","CC2","CC3")

b1<-baseE %>% filter(patent_year<=2003)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n5b<-decompose(n1,min.vertices =gcc)
n5b<-n5b[[1]]
diam5<-diameter(n5b)
v5<-as.numeric(length(V(n5b)))
e5<-gsize(n5b)
gcc5<-c1
names(gcc5)[1]<-"size"
gcc5$year<-2003
gcc5$gp<-c("GCC","CC2","CC3")

b1<-baseE %>% filter(patent_year<=2004)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n6b<-decompose(n1,min.vertices =gcc)
n6b<-n6b[[1]]
diam6<-diameter(n6b)
v6<-as.numeric(length(V(n6b)))
e6<-gsize(n6b)
gcc6<-c1
names(gcc6)[1]<-"size"
gcc6$year<-2004
gcc6$gp<-c("GCC","CC2","CC3")

b1<-baseE %>% filter(patent_year<=2005)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n7b<-decompose(n1,min.vertices =gcc)
n7b<-n7b[[1]]
diam7<-diameter(n7b)
v7<-as.numeric(length(V(n7b)))
e7<-gsize(n7b)
gcc7<-c1
names(gcc7)[1]<-"size"
gcc7$year<-2005
gcc7$gp<-c("GCC","CC2","CC3")

b1<-baseE %>% filter(patent_year<=2006)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n8b<-decompose(n1,min.vertices =gcc)
n8b<-n8b[[1]]
diam8<-diameter(n8b)
v8<-as.numeric(length(V(n8b)))
e8<-gsize(n8b)
gcc8<-c1
names(gcc8)[1]<-"size"
gcc8$year<-2006
gcc8$gp<-c("GCC","CC2","CC3")

b1<-baseE %>% filter(patent_year<=2007)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n9b<-decompose(n1,min.vertices =gcc)
n9b<-n9b[[1]]
diam9<-diameter(n9b)
v9<-as.numeric(length(V(n9b)))
e9<-gsize(n9b)
gcc9<-c1
names(gcc9)[1]<-"size"
gcc9$year<-2007
gcc9$gp<-c("GCC","CC2","CC3")

b1<-baseE %>% filter(patent_year<=2008)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n10b<-decompose(n1,min.vertices =gcc)
n10b<-n10b[[1]]
diam10<-diameter(n10b)
v10<-as.numeric(length(V(n10b)))
e10<-gsize(n10b)
gcc10<-c1
names(gcc10)[1]<-"size"
gcc10$year<-2008
gcc10$gp<-c("GCC","CC2","CC3")

b1<-baseE %>% filter(patent_year<=2009)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n11b<-decompose(n1,min.vertices =gcc)
n11b<-n11b[[1]]
diam11<-diameter(n11b)
v11<-as.numeric(length(V(n11b)))
e11<-gsize(n11b)
gcc11<-c1
names(gcc11)[1]<-"size"
gcc11$year<-2009
gcc11$gp<-c("GCC","CC2","CC3")
n2009E<-n11b

b1<-baseE %>% filter(patent_year<=2010)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n12b<-decompose(n1,min.vertices =gcc)
n12b<-n12b[[1]]
diam12<-diameter(n12b)
v12<-as.numeric(length(V(n12b)))
e12<-gsize(n12b)
gcc12<-c1
names(gcc12)[1]<-"size"
gcc12$year<-2010
gcc12$gp<-c("GCC","CC2","CC3")

b1<-baseE %>% filter(patent_year<=2011)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n13b<-decompose(n1,min.vertices =gcc)
n13b<-n13b[[1]]
diam13<-diameter(n13b)
v13<-as.numeric(length(V(n13b)))
e13<-gsize(n13b)
gcc13<-c1
names(gcc13)[1]<-"size"
gcc13$year<-2011
gcc13$gp<-c("GCC","CC2","CC3")

b1<-baseE %>% filter(patent_year<=2012)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n14b<-decompose(n1,min.vertices =gcc)
n14b<-n14b[[1]]
diam14<-diameter(n14b)
v14<-as.numeric(length(V(n14b)))
e14<-gsize(n14b)
gcc14<-c1
names(gcc14)[1]<-"size"
gcc14$year<-2012
gcc14$gp<-c("GCC","CC2","CC3")

b1<-baseE %>% filter(patent_year<=2013)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n15b<-decompose(n1,min.vertices =gcc)
n15b<-n15b[[1]]
diam15<-diameter(n15b)
v15<-as.numeric(length(V(n15b)))
e15<-gsize(n15b)
gcc15<-c1
names(gcc15)[1]<-"size"
gcc15$year<-2013
gcc15$gp<-c("GCC","CC2","CC3")

b1<-baseE %>% filter(patent_year<=2014)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n16b<-decompose(n1,min.vertices =gcc)
n16b<-n16b[[1]]
diam16<-diameter(n16b)
v16<-as.numeric(length(V(n16b)))
e16<-gsize(n16b)
gcc16<-c1
names(gcc16)[1]<-"size"
gcc16$year<-2014
gcc16$gp<-c("GCC","CC2","CC3")

b1<-baseE %>% filter(patent_year<=2015)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n17b<-decompose(n1,min.vertices =gcc)
n17b<-n17b[[1]]
diam17<-diameter(n17b)
v17<-as.numeric(length(V(n17b)))
e17<-gsize(n17b)
gcc17<-c1
names(gcc17)[1]<-"size"
gcc17$year<-2015
gcc17$gp<-c("GCC","CC2","CC3")

b1<-baseE %>% filter(patent_year<=2016)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n18b<-decompose(n1,min.vertices =gcc)
n18b<-n18b[[1]]
diam18<-diameter(n18b)
v18<-as.numeric(length(V(n18b)))
e18<-gsize(n18b)
gcc18<-c1
names(gcc18)[1]<-"size"
gcc18$year<-2016
gcc18$gp<-c("GCC","CC2","CC3")

b1<-baseE %>% filter(patent_year<=2017)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n19b<-decompose(n1,min.vertices =gcc)
n19b<-n19b[[1]]
diam19<-diameter(n19b)
v19<-as.numeric(length(V(n19b)))
e19<-gsize(n19b)
gcc19<-c1
names(gcc19)[1]<-"size"
gcc19$year<-2017
gcc19$gp<-c("GCC","CC2","CC3")

b1<-baseE %>% filter(patent_year<=2018)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n20b<-decompose(n1,min.vertices =gcc)
n20b<-n20b[[1]]
diam20<-diameter(n20b)
v20<-as.numeric(length(V(n20b)))
e20<-gsize(n20b)
gcc20<-c1
names(gcc20)[1]<-"size"
gcc20$year<-2018
gcc20$gp<-c("GCC","CC2","CC3")

b1<-baseE %>% filter(patent_year<=2019)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n21b<-decompose(n1,min.vertices =gcc)
n21b<-n21b[[1]]
diam21<-diameter(n21b)
v21<-as.numeric(length(V(n21b)))
e21<-gsize(n21b)
gcc21<-c1
names(gcc21)[1]<-"size"
gcc21$year<-2019
gcc21$gp<-c("GCC","CC2","CC3")
n2019E<-n21b

year<-c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
E<-c(diam1,diam2,diam3,diam4,diam5,diam6,diam7,diam8,diam9,diam10,diam11,diam12,diam13,diam14,diam15,diam16,diam17,diam18,diam19,diam20,diam21)
node<-c(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,v19,v20,v21)
edge<-c(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18,e19,e20,e21)
diamE<-data.frame(year,E)
neE<-data.frame(year,node,edge)
gpE<-Reduce(rbind.fill,list(gcc1,gcc2,gcc3,gcc4,gcc5,gcc6,gcc7,gcc8,gcc9,gcc10,gcc11,gcc12,gcc13,gcc14,gcc15,gcc16,gcc17,gcc18,gcc19,gcc20,gcc21))


#F
b1<-baseF %>% filter(patent_year<=1999)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n1b<-decompose(n1,min.vertices =gcc)
n1b<-n1b[[1]]
diam1<-diameter(n1b)
v1<-as.numeric(length(V(n1b)))
e1<-gsize(n1b)
gcc1<-c1
names(gcc1)[1]<-"size"
gcc1$year<-1999
gcc1$gp<-c("GCC","CC2","CC3")
n1999F<-n1b

b1<-baseF %>% filter(patent_year<=2000)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n2b<-decompose(n1,min.vertices =gcc)
n2b<-n2b[[1]]
diam2<-diameter(n2b)
v2<-as.numeric(length(V(n2b)))
e2<-gsize(n2b)
gcc2<-c1
names(gcc2)[1]<-"size"
gcc2$year<-2000
gcc2$gp<-c("GCC","CC2","CC3")

b1<-baseF %>% filter(patent_year<=2001)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n3b<-decompose(n1,min.vertices =gcc)
n3b<-n3b[[1]]
diam3<-diameter(n3b)
v3<-as.numeric(length(V(n3b)))
e3<-gsize(n3b)
gcc3<-c1
names(gcc3)[1]<-"size"
gcc3$year<-2001
gcc3$gp<-c("GCC","CC2","CC3")

b1<-baseF %>% filter(patent_year<=2002)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n4b<-decompose(n1,min.vertices =gcc)
n4b<-n4b[[1]]
diam4<-diameter(n4b)
v4<-as.numeric(length(V(n4b)))
e4<-gsize(n4b)
gcc4<-c1
names(gcc4)[1]<-"size"
gcc4$year<-2002
gcc4$gp<-c("GCC","CC2","CC3")

b1<-baseF %>% filter(patent_year<=2003)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n5b<-decompose(n1,min.vertices =gcc)
n5b<-n5b[[1]]
diam5<-diameter(n5b)
v5<-as.numeric(length(V(n5b)))
e5<-gsize(n5b)
gcc5<-c1
names(gcc5)[1]<-"size"
gcc5$year<-2003
gcc5$gp<-c("GCC","CC2","CC3")

b1<-baseF %>% filter(patent_year<=2004)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n6b<-decompose(n1,min.vertices =gcc)
n6b<-n6b[[1]]
diam6<-diameter(n6b)
v6<-as.numeric(length(V(n6b)))
e6<-gsize(n6b)
gcc6<-c1
names(gcc6)[1]<-"size"
gcc6$year<-2004
gcc6$gp<-c("GCC","CC2","CC3")

b1<-baseF %>% filter(patent_year<=2005)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n7b<-decompose(n1,min.vertices =gcc)
n7b<-n7b[[1]]
diam7<-diameter(n7b)
v7<-as.numeric(length(V(n7b)))
e7<-gsize(n7b)
gcc7<-c1
names(gcc7)[1]<-"size"
gcc7$year<-2005
gcc7$gp<-c("GCC","CC2","CC3")

b1<-baseF %>% filter(patent_year<=2006)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n8b<-decompose(n1,min.vertices =gcc)
n8b<-n8b[[1]]
diam8<-diameter(n8b)
v8<-as.numeric(length(V(n8b)))
e8<-gsize(n8b)
gcc8<-c1
names(gcc8)[1]<-"size"
gcc8$year<-2006
gcc8$gp<-c("GCC","CC2","CC3")

b1<-baseF %>% filter(patent_year<=2007)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n9b<-decompose(n1,min.vertices =gcc)
n9b<-n9b[[1]]
diam9<-diameter(n9b)
v9<-as.numeric(length(V(n9b)))
e9<-gsize(n9b)
gcc9<-c1
names(gcc9)[1]<-"size"
gcc9$year<-2007
gcc9$gp<-c("GCC","CC2","CC3")

b1<-baseF %>% filter(patent_year<=2008)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n10b<-decompose(n1,min.vertices =gcc)
n10b<-n10b[[1]]
diam10<-diameter(n10b)
v10<-as.numeric(length(V(n10b)))
e10<-gsize(n10b)
gcc10<-c1
names(gcc10)[1]<-"size"
gcc10$year<-2008
gcc10$gp<-c("GCC","CC2","CC3")

b1<-baseF %>% filter(patent_year<=2009)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n11b<-decompose(n1,min.vertices =gcc)
n11b<-n11b[[1]]
diam11<-diameter(n11b)
v11<-as.numeric(length(V(n11b)))
e11<-gsize(n11b)
gcc11<-c1
names(gcc11)[1]<-"size"
gcc11$year<-2009
gcc11$gp<-c("GCC","CC2","CC3")
n2009F<-n11b

b1<-baseF %>% filter(patent_year<=2010)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n12b<-decompose(n1,min.vertices =gcc)
n12b<-n12b[[1]]
diam12<-diameter(n12b)
v12<-as.numeric(length(V(n12b)))
e12<-gsize(n12b)
gcc12<-c1
names(gcc12)[1]<-"size"
gcc12$year<-2010
gcc12$gp<-c("GCC","CC2","CC3")

b1<-baseF %>% filter(patent_year<=2011)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n13b<-decompose(n1,min.vertices =gcc)
n13b<-n13b[[1]]
diam13<-diameter(n13b)
v13<-as.numeric(length(V(n13b)))
e13<-gsize(n13b)
gcc13<-c1
names(gcc13)[1]<-"size"
gcc13$year<-2011
gcc13$gp<-c("GCC","CC2","CC3")

b1<-baseF %>% filter(patent_year<=2012)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n14b<-decompose(n1,min.vertices =gcc)
n14b<-n14b[[1]]
diam14<-diameter(n14b)
v14<-as.numeric(length(V(n14b)))
e14<-gsize(n14b)
gcc14<-c1
names(gcc14)[1]<-"size"
gcc14$year<-2012
gcc14$gp<-c("GCC","CC2","CC3")

b1<-baseF %>% filter(patent_year<=2013)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n15b<-decompose(n1,min.vertices =gcc)
n15b<-n15b[[1]]
diam15<-diameter(n15b)
v15<-as.numeric(length(V(n15b)))
e15<-gsize(n15b)
gcc15<-c1
names(gcc15)[1]<-"size"
gcc15$year<-2013
gcc15$gp<-c("GCC","CC2","CC3")

b1<-baseF %>% filter(patent_year<=2014)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n16b<-decompose(n1,min.vertices =gcc)
n16b<-n16b[[1]]
diam16<-diameter(n16b)
v16<-as.numeric(length(V(n16b)))
e16<-gsize(n16b)
gcc16<-c1
names(gcc16)[1]<-"size"
gcc16$year<-2014
gcc16$gp<-c("GCC","CC2","CC3")

b1<-baseF %>% filter(patent_year<=2015)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n17b<-decompose(n1,min.vertices =gcc)
n17b<-n17b[[1]]
diam17<-diameter(n17b)
v17<-as.numeric(length(V(n17b)))
e17<-gsize(n17b)
gcc17<-c1
names(gcc17)[1]<-"size"
gcc17$year<-2015
gcc17$gp<-c("GCC","CC2","CC3")

b1<-baseF %>% filter(patent_year<=2016)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n18b<-decompose(n1,min.vertices =gcc)
n18b<-n18b[[1]]
diam18<-diameter(n18b)
v18<-as.numeric(length(V(n18b)))
e18<-gsize(n18b)
gcc18<-c1
names(gcc18)[1]<-"size"
gcc18$year<-2016
gcc18$gp<-c("GCC","CC2","CC3")

b1<-baseF %>% filter(patent_year<=2017)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n19b<-decompose(n1,min.vertices =gcc)
n19b<-n19b[[1]]
diam19<-diameter(n19b)
v19<-as.numeric(length(V(n19b)))
e19<-gsize(n19b)
gcc19<-c1
names(gcc19)[1]<-"size"
gcc19$year<-2017
gcc19$gp<-c("GCC","CC2","CC3")

b1<-baseF %>% filter(patent_year<=2018)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n20b<-decompose(n1,min.vertices =gcc)
n20b<-n20b[[1]]
diam20<-diameter(n20b)
v20<-as.numeric(length(V(n20b)))
e20<-gsize(n20b)
gcc20<-c1
names(gcc20)[1]<-"size"
gcc20$year<-2018
gcc20$gp<-c("GCC","CC2","CC3")

b1<-baseF %>% filter(patent_year<=2019)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n21b<-decompose(n1,min.vertices =gcc)
n21b<-n21b[[1]]
diam21<-diameter(n21b)
v21<-as.numeric(length(V(n21b)))
e21<-gsize(n21b)
gcc21<-c1
names(gcc21)[1]<-"size"
gcc21$year<-2019
gcc21$gp<-c("GCC","CC2","CC3")
n2019F<-n21b

year<-c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
FF<-c(diam1,diam2,diam3,diam4,diam5,diam6,diam7,diam8,diam9,diam10,diam11,diam12,diam13,diam14,diam15,diam16,diam17,diam18,diam19,diam20,diam21)
node<-c(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,v19,v20,v21)
edge<-c(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18,e19,e20,e21)
diamF<-data.frame(year,FF)
neF<-data.frame(year,node,edge)
gpF<-Reduce(rbind.fill,list(gcc1,gcc2,gcc3,gcc4,gcc5,gcc6,gcc7,gcc8,gcc9,gcc10,gcc11,gcc12,gcc13,gcc14,gcc15,gcc16,gcc17,gcc18,gcc19,gcc20,gcc21))

#G
b1<-baseG %>% filter(patent_year<=1999)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n1b<-decompose(n1,min.vertices =gcc)
n1b<-n1b[[1]]
diam1<-diameter(n1b)
v1<-as.numeric(length(V(n1b)))
e1<-gsize(n1b)
gcc1<-c1
names(gcc1)[1]<-"size"
gcc1$year<-1999
gcc1$gp<-c("GCC","CC2","CC3")
n1999G<-n1b

b1<-baseG %>% filter(patent_year<=2000)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n2b<-decompose(n1,min.vertices =gcc)
n2b<-n2b[[1]]
diam2<-diameter(n2b)
v2<-as.numeric(length(V(n2b)))
e2<-gsize(n2b)
gcc2<-c1
names(gcc2)[1]<-"size"
gcc2$year<-2000
gcc2$gp<-c("GCC","CC2","CC3")

b1<-baseG %>% filter(patent_year<=2001)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n3b<-decompose(n1,min.vertices =gcc)
n3b<-n3b[[1]]
diam3<-diameter(n3b)
v3<-as.numeric(length(V(n3b)))
e3<-gsize(n3b)
gcc3<-c1
names(gcc3)[1]<-"size"
gcc3$year<-2001
gcc3$gp<-c("GCC","CC2","CC3")

b1<-baseG %>% filter(patent_year<=2002)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n4b<-decompose(n1,min.vertices =gcc)
n4b<-n4b[[1]]
diam4<-diameter(n4b)
v4<-as.numeric(length(V(n4b)))
e4<-gsize(n4b)
gcc4<-c1
names(gcc4)[1]<-"size"
gcc4$year<-2002
gcc4$gp<-c("GCC","CC2","CC3")

b1<-baseG %>% filter(patent_year<=2003)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n5b<-decompose(n1,min.vertices =gcc)
n5b<-n5b[[1]]
diam5<-diameter(n5b)
v5<-as.numeric(length(V(n5b)))
e5<-gsize(n5b)
gcc5<-c1
names(gcc5)[1]<-"size"
gcc5$year<-2003
gcc5$gp<-c("GCC","CC2","CC3")

b1<-baseG %>% filter(patent_year<=2004)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n6b<-decompose(n1,min.vertices =gcc)
n6b<-n6b[[1]]
diam6<-diameter(n6b)
v6<-as.numeric(length(V(n6b)))
e6<-gsize(n6b)
gcc6<-c1
names(gcc6)[1]<-"size"
gcc6$year<-2004
gcc6$gp<-c("GCC","CC2","CC3")

b1<-baseG %>% filter(patent_year<=2005)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n7b<-decompose(n1,min.vertices =gcc)
n7b<-n7b[[1]]
diam7<-diameter(n7b)
v7<-as.numeric(length(V(n7b)))
e7<-gsize(n7b)
gcc7<-c1
names(gcc7)[1]<-"size"
gcc7$year<-2005
gcc7$gp<-c("GCC","CC2","CC3")

b1<-baseG %>% filter(patent_year<=2006)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n8b<-decompose(n1,min.vertices =gcc)
n8b<-n8b[[1]]
diam8<-diameter(n8b)
v8<-as.numeric(length(V(n8b)))
e8<-gsize(n8b)
gcc8<-c1
names(gcc8)[1]<-"size"
gcc8$year<-2006
gcc8$gp<-c("GCC","CC2","CC3")

b1<-baseG %>% filter(patent_year<=2007)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n9b<-decompose(n1,min.vertices =gcc)
n9b<-n9b[[1]]
diam9<-diameter(n9b)
v9<-as.numeric(length(V(n9b)))
e9<-gsize(n9b)
gcc9<-c1
names(gcc9)[1]<-"size"
gcc9$year<-2007
gcc9$gp<-c("GCC","CC2","CC3")

b1<-baseG %>% filter(patent_year<=2008)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n10b<-decompose(n1,min.vertices =gcc)
n10b<-n10b[[1]]
diam10<-diameter(n10b)
v10<-as.numeric(length(V(n10b)))
e10<-gsize(n10b)
gcc10<-c1
names(gcc10)[1]<-"size"
gcc10$year<-2008
gcc10$gp<-c("GCC","CC2","CC3")

b1<-baseG %>% filter(patent_year<=2009)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n11b<-decompose(n1,min.vertices =gcc)
n11b<-n11b[[1]]
diam11<-diameter(n11b)
v11<-as.numeric(length(V(n11b)))
e11<-gsize(n11b)
gcc11<-c1
names(gcc11)[1]<-"size"
gcc11$year<-2009
gcc11$gp<-c("GCC","CC2","CC3")
n2009G<-n11b

b1<-baseG %>% filter(patent_year<=2010)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n12b<-decompose(n1,min.vertices =gcc)
n12b<-n12b[[1]]
diam12<-diameter(n12b)
v12<-as.numeric(length(V(n12b)))
e12<-gsize(n12b)
gcc12<-c1
names(gcc12)[1]<-"size"
gcc12$year<-2010
gcc12$gp<-c("GCC","CC2","CC3")

b1<-baseG %>% filter(patent_year<=2011)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n13b<-decompose(n1,min.vertices =gcc)
n13b<-n13b[[1]]
diam13<-diameter(n13b)
v13<-as.numeric(length(V(n13b)))
e13<-gsize(n13b)
gcc13<-c1
names(gcc13)[1]<-"size"
gcc13$year<-2011
gcc13$gp<-c("GCC","CC2","CC3")

b1<-baseG %>% filter(patent_year<=2012)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n14b<-decompose(n1,min.vertices =gcc)
n14b<-n14b[[1]]
diam14<-diameter(n14b)
v14<-as.numeric(length(V(n14b)))
e14<-gsize(n14b)
gcc14<-c1
names(gcc14)[1]<-"size"
gcc14$year<-2012
gcc14$gp<-c("GCC","CC2","CC3")

b1<-baseG %>% filter(patent_year<=2013)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n15b<-decompose(n1,min.vertices =gcc)
n15b<-n15b[[1]]
diam15<-diameter(n15b)
v15<-as.numeric(length(V(n15b)))
e15<-gsize(n15b)
gcc15<-c1
names(gcc15)[1]<-"size"
gcc15$year<-2013
gcc15$gp<-c("GCC","CC2","CC3")

b1<-baseG %>% filter(patent_year<=2014)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n16b<-decompose(n1,min.vertices =gcc)
n16b<-n16b[[1]]
diam16<-diameter(n16b)
v16<-as.numeric(length(V(n16b)))
e16<-gsize(n16b)
gcc16<-c1
names(gcc16)[1]<-"size"
gcc16$year<-2014
gcc16$gp<-c("GCC","CC2","CC3")

b1<-baseG %>% filter(patent_year<=2015)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n17b<-decompose(n1,min.vertices =gcc)
n17b<-n17b[[1]]
diam17<-diameter(n17b)
v17<-as.numeric(length(V(n17b)))
e17<-gsize(n17b)
gcc17<-c1
names(gcc17)[1]<-"size"
gcc17$year<-2015
gcc17$gp<-c("GCC","CC2","CC3")

b1<-baseG %>% filter(patent_year<=2016)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n18b<-decompose(n1,min.vertices =gcc)
n18b<-n18b[[1]]
diam18<-diameter(n18b)
v18<-as.numeric(length(V(n18b)))
e18<-gsize(n18b)
gcc18<-c1
names(gcc18)[1]<-"size"
gcc18$year<-2016
gcc18$gp<-c("GCC","CC2","CC3")

b1<-baseG %>% filter(patent_year<=2017)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n19b<-decompose(n1,min.vertices =gcc)
n19b<-n19b[[1]]
diam19<-diameter(n19b)
v19<-as.numeric(length(V(n19b)))
e19<-gsize(n19b)
gcc19<-c1
names(gcc19)[1]<-"size"
gcc19$year<-2017
gcc19$gp<-c("GCC","CC2","CC3")

b1<-baseG %>% filter(patent_year<=2018)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n20b<-decompose(n1,min.vertices =gcc)
n20b<-n20b[[1]]
diam20<-diameter(n20b)
v20<-as.numeric(length(V(n20b)))
e20<-gsize(n20b)
gcc20<-c1
names(gcc20)[1]<-"size"
gcc20$year<-2018
gcc20$gp<-c("GCC","CC2","CC3")

b1<-baseG %>% filter(patent_year<=2019)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n21b<-decompose(n1,min.vertices =gcc)
n21b<-n21b[[1]]
diam21<-diameter(n21b)
v21<-as.numeric(length(V(n21b)))
e21<-gsize(n21b)
gcc21<-c1
names(gcc21)[1]<-"size"
gcc21$year<-2019
gcc21$gp<-c("GCC","CC2","CC3")
n2019G<-n21b

year<-c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
G<-c(diam1,diam2,diam3,diam4,diam5,diam6,diam7,diam8,diam9,diam10,diam11,diam12,diam13,diam14,diam15,diam16,diam17,diam18,diam19,diam20,diam21)
node<-c(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,v19,v20,v21)
edge<-c(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18,e19,e20,e21)
diamG<-data.frame(year,G)
neG<-data.frame(year,node,edge)
gpG<-Reduce(rbind.fill,list(gcc1,gcc2,gcc3,gcc4,gcc5,gcc6,gcc7,gcc8,gcc9,gcc10,gcc11,gcc12,gcc13,gcc14,gcc15,gcc16,gcc17,gcc18,gcc19,gcc20,gcc21))


#H
b1<-baseH %>% filter(patent_year<=1999)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n1b<-decompose(n1,min.vertices =gcc)
n1b<-n1b[[1]]
diam1<-diameter(n1b)
v1<-as.numeric(length(V(n1b)))
e1<-gsize(n1b)
gcc1<-c1
names(gcc1)[1]<-"size"
gcc1$year<-1999
gcc1$gp<-c("GCC","CC2","CC3")
n1999H<-n1b

b1<-baseH %>% filter(patent_year<=2000)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n2b<-decompose(n1,min.vertices =gcc)
n2b<-n2b[[1]]
diam2<-diameter(n2b)
v2<-as.numeric(length(V(n2b)))
e2<-gsize(n2b)
gcc2<-c1
names(gcc2)[1]<-"size"
gcc2$year<-2000
gcc2$gp<-c("GCC","CC2","CC3")

b1<-baseH %>% filter(patent_year<=2001)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n3b<-decompose(n1,min.vertices =gcc)
n3b<-n3b[[1]]
diam3<-diameter(n3b)
v3<-as.numeric(length(V(n3b)))
e3<-gsize(n3b)
gcc3<-c1
names(gcc3)[1]<-"size"
gcc3$year<-2001
gcc3$gp<-c("GCC","CC2","CC3")

b1<-baseH %>% filter(patent_year<=2002)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n4b<-decompose(n1,min.vertices =gcc)
n4b<-n4b[[1]]
diam4<-diameter(n4b)
v4<-as.numeric(length(V(n4b)))
e4<-gsize(n4b)
gcc4<-c1
names(gcc4)[1]<-"size"
gcc4$year<-2002
gcc4$gp<-c("GCC","CC2","CC3")

b1<-baseH %>% filter(patent_year<=2003)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n5b<-decompose(n1,min.vertices =gcc)
n5b<-n5b[[1]]
diam5<-diameter(n5b)
v5<-as.numeric(length(V(n5b)))
e5<-gsize(n5b)
gcc5<-c1
names(gcc5)[1]<-"size"
gcc5$year<-2003
gcc5$gp<-c("GCC","CC2","CC3")

b1<-baseH %>% filter(patent_year<=2004)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n6b<-decompose(n1,min.vertices =gcc)
n6b<-n6b[[1]]
diam6<-diameter(n6b)
v6<-as.numeric(length(V(n6b)))
e6<-gsize(n6b)
gcc6<-c1
names(gcc6)[1]<-"size"
gcc6$year<-2004
gcc6$gp<-c("GCC","CC2","CC3")

b1<-baseH %>% filter(patent_year<=2005)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n7b<-decompose(n1,min.vertices =gcc)
n7b<-n7b[[1]]
diam7<-diameter(n7b)
v7<-as.numeric(length(V(n7b)))
e7<-gsize(n7b)
gcc7<-c1
names(gcc7)[1]<-"size"
gcc7$year<-2005
gcc7$gp<-c("GCC","CC2","CC3")

b1<-baseH %>% filter(patent_year<=2006)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n8b<-decompose(n1,min.vertices =gcc)
n8b<-n8b[[1]]
diam8<-diameter(n8b)
v8<-as.numeric(length(V(n8b)))
e8<-gsize(n8b)
gcc8<-c1
names(gcc8)[1]<-"size"
gcc8$year<-2006
gcc8$gp<-c("GCC","CC2","CC3")

b1<-baseH %>% filter(patent_year<=2007)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n9b<-decompose(n1,min.vertices =gcc)
n9b<-n9b[[1]]
diam9<-diameter(n9b)
v9<-as.numeric(length(V(n9b)))
e9<-gsize(n9b)
gcc9<-c1
names(gcc9)[1]<-"size"
gcc9$year<-2007
gcc9$gp<-c("GCC","CC2","CC3")

b1<-baseH %>% filter(patent_year<=2008)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n10b<-decompose(n1,min.vertices =gcc)
n10b<-n10b[[1]]
diam10<-diameter(n10b)
v10<-as.numeric(length(V(n10b)))
e10<-gsize(n10b)
gcc10<-c1
names(gcc10)[1]<-"size"
gcc10$year<-2008
gcc10$gp<-c("GCC","CC2","CC3")

b1<-baseH %>% filter(patent_year<=2009)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n11b<-decompose(n1,min.vertices =gcc)
n11b<-n11b[[1]]
diam11<-diameter(n11b)
v11<-as.numeric(length(V(n11b)))
e11<-gsize(n11b)
gcc11<-c1
names(gcc11)[1]<-"size"
gcc11$year<-2009
gcc11$gp<-c("GCC","CC2","CC3")
n2009H<-n11b

b1<-baseH %>% filter(patent_year<=2010)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n12b<-decompose(n1,min.vertices =gcc)
n12b<-n12b[[1]]
diam12<-diameter(n12b)
v12<-as.numeric(length(V(n12b)))
e12<-gsize(n12b)
gcc12<-c1
names(gcc12)[1]<-"size"
gcc12$year<-2010
gcc12$gp<-c("GCC","CC2","CC3")

b1<-baseH %>% filter(patent_year<=2011)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n13b<-decompose(n1,min.vertices =gcc)
n13b<-n13b[[1]]
diam13<-diameter(n13b)
v13<-as.numeric(length(V(n13b)))
e13<-gsize(n13b)
gcc13<-c1
names(gcc13)[1]<-"size"
gcc13$year<-2011
gcc13$gp<-c("GCC","CC2","CC3")

b1<-baseH %>% filter(patent_year<=2012)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n14b<-decompose(n1,min.vertices =gcc)
n14b<-n14b[[1]]
diam14<-diameter(n14b)
v14<-as.numeric(length(V(n14b)))
e14<-gsize(n14b)
gcc14<-c1
names(gcc14)[1]<-"size"
gcc14$year<-2012
gcc14$gp<-c("GCC","CC2","CC3")

b1<-baseH %>% filter(patent_year<=2013)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n15b<-decompose(n1,min.vertices =gcc)
n15b<-n15b[[1]]
diam15<-diameter(n15b)
v15<-as.numeric(length(V(n15b)))
e15<-gsize(n15b)
gcc15<-c1
names(gcc15)[1]<-"size"
gcc15$year<-2013
gcc15$gp<-c("GCC","CC2","CC3")

b1<-baseH %>% filter(patent_year<=2014)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n16b<-decompose(n1,min.vertices =gcc)
n16b<-n16b[[1]]
diam16<-diameter(n16b)
v16<-as.numeric(length(V(n16b)))
e16<-gsize(n16b)
gcc16<-c1
names(gcc16)[1]<-"size"
gcc16$year<-2014
gcc16$gp<-c("GCC","CC2","CC3")

b1<-baseH %>% filter(patent_year<=2015)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n17b<-decompose(n1,min.vertices =gcc)
n17b<-n17b[[1]]
diam17<-diameter(n17b)
v17<-as.numeric(length(V(n17b)))
e17<-gsize(n17b)
gcc17<-c1
names(gcc17)[1]<-"size"
gcc17$year<-2015
gcc17$gp<-c("GCC","CC2","CC3")

b1<-baseH %>% filter(patent_year<=2016)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n18b<-decompose(n1,min.vertices =gcc)
n18b<-n18b[[1]]
diam18<-diameter(n18b)
v18<-as.numeric(length(V(n18b)))
e18<-gsize(n18b)
gcc18<-c1
names(gcc18)[1]<-"size"
gcc18$year<-2016
gcc18$gp<-c("GCC","CC2","CC3")

b1<-baseH %>% filter(patent_year<=2017)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n19b<-decompose(n1,min.vertices =gcc)
n19b<-n19b[[1]]
diam19<-diameter(n19b)
v19<-as.numeric(length(V(n19b)))
e19<-gsize(n19b)
gcc19<-c1
names(gcc19)[1]<-"size"
gcc19$year<-2017
gcc19$gp<-c("GCC","CC2","CC3")

b1<-baseH %>% filter(patent_year<=2018)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n20b<-decompose(n1,min.vertices =gcc)
n20b<-n20b[[1]]
diam20<-diameter(n20b)
v20<-as.numeric(length(V(n20b)))
e20<-gsize(n20b)
gcc20<-c1
names(gcc20)[1]<-"size"
gcc20$year<-2018
gcc20$gp<-c("GCC","CC2","CC3")

b1<-baseH %>% filter(patent_year<=2019)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1:3)
c1<-as.data.frame(c1)
gcc<-c1[1,1]
n21b<-decompose(n1,min.vertices =gcc)
n21b<-n21b[[1]]
diam21<-diameter(n21b)
v21<-as.numeric(length(V(n21b)))
e21<-gsize(n21b)
gcc21<-c1
names(gcc21)[1]<-"size"
gcc21$year<-2019
gcc21$gp<-c("GCC","CC2","CC3")
n2019H<-n21b

year<-c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
H<-c(diam1,diam2,diam3,diam4,diam5,diam6,diam7,diam8,diam9,diam10,diam11,diam12,diam13,diam14,diam15,diam16,diam17,diam18,diam19,diam20,diam21)
node<-c(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,v19,v20,v21)
edge<-c(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18,e19,e20,e21)
diamH<-data.frame(year,H)
neH<-data.frame(year,node,edge)
gpH<-Reduce(rbind.fill,list(gcc1,gcc2,gcc3,gcc4,gcc5,gcc6,gcc7,gcc8,gcc9,gcc10,gcc11,gcc12,gcc13,gcc14,gcc15,gcc16,gcc17,gcc18,gcc19,gcc20,gcc21))


###SUB 2005GRAPH

#TOTAL
b1<-base %>% filter(patent_year>=2005 & patent_year<=2005)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc1<-c1
gcc1$year<-2005
gcc1$gp<-"GCC Post 2005 subgraph, no past"

b1<-base %>% filter(patent_year>=2005 & patent_year<=2006)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc2<-c1
gcc2$year<-2006
gcc2$gp<-"GCC Post 2005 subgraph, no past"

b1<-base %>% filter(patent_year>=2005 & patent_year<=2007)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc3<-c1
gcc3$year<-2007
gcc3$gp<-"GCC Post 2005 subgraph, no past"

b1<-base %>% filter(patent_year>=2005 & patent_year<=2008)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc4<-c1
gcc4$year<-2008
gcc4$gp<-"GCC Post 2005 subgraph, no past"

b1<-base %>% filter(patent_year>=2005 & patent_year<=2009)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc5<-c1
gcc5$year<-2009
gcc5$gp<-"GCC Post 2005 subgraph, no past"


b1<-base %>% filter(patent_year>=2005 & patent_year<=2010)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc6<-c1
gcc6$year<-2010
gcc6$gp<-"GCC Post 2005 subgraph, no past"


b1<-base %>% filter(patent_year>=2005 & patent_year<=2011)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc7<-c1
gcc7$year<-2011
gcc7$gp<-"GCC Post 2005 subgraph, no past"


b1<-base %>% filter(patent_year>=2005 & patent_year<=2012)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc8<-c1
gcc8$year<-2012
gcc8$gp<-"GCC Post 2005 subgraph, no past"

b1<-base %>% filter(patent_year>=2005 & patent_year<=2013)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc9<-c1
gcc9$year<-2013
gcc9$gp<-"GCC Post 2005 subgraph, no past"

b1<-base %>% filter(patent_year>=2005 & patent_year<=2014)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc10<-c1
gcc10$year<-2014
gcc10$gp<-"GCC Post 2005 subgraph, no past"

b1<-base %>% filter(patent_year>=2005 & patent_year<=2015)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc11<-c1
gcc11$year<-2015
gcc11$gp<-"GCC Post 2005 subgraph, no past"

b1<-base %>% filter(patent_year>=2005 & patent_year<=2016)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc12<-c1
gcc12$year<-2016
gcc12$gp<-"GCC Post 2005 subgraph, no past"


b1<-base %>% filter(patent_year>=2005 & patent_year<=2017)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc13<-c1
gcc13$year<-2017
gcc13$gp<-"GCC Post 2005 subgraph, no past"


b1<-base %>% filter(patent_year>=2005 & patent_year<=2018)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc14<-c1
gcc14$year<-2018
gcc14$gp<-"GCC Post 2005 subgraph, no past"

b1<-base %>% filter(patent_year>=2005 & patent_year<=2019)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc15<-c1
gcc15$year<-2019
gcc15$gp<-"GCC Post 2005 subgraph, no past"

gpL<-Reduce(rbind.fill,list(gpL,gcc1,gcc2,gcc3,gcc4,gcc5,gcc6,gcc7,gcc8,gcc9,gcc10,gcc11,gcc12,gcc13,gcc14,gcc15))

#A
b1<-baseA %>% filter(patent_year>=2005 & patent_year<=2005)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc1<-c1
gcc1$year<-2005
gcc1$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseA %>% filter(patent_year>=2005 & patent_year<=2006)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc2<-c1
gcc2$year<-2006
gcc2$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseA %>% filter(patent_year>=2005 & patent_year<=2007)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc3<-c1
gcc3$year<-2007
gcc3$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseA %>% filter(patent_year>=2005 & patent_year<=2008)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc4<-c1
gcc4$year<-2008
gcc4$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseA %>% filter(patent_year>=2005 & patent_year<=2009)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc5<-c1
gcc5$year<-2009
gcc5$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseA %>% filter(patent_year>=2005 & patent_year<=2010)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc6<-c1
gcc6$year<-2010
gcc6$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseA %>% filter(patent_year>=2005 & patent_year<=2011)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc7<-c1
gcc7$year<-2011
gcc7$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseA %>% filter(patent_year>=2005 & patent_year<=2012)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc8<-c1
gcc8$year<-2012
gcc8$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseA %>% filter(patent_year>=2005 & patent_year<=2013)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc9<-c1
gcc9$year<-2013
gcc9$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseA %>% filter(patent_year>=2005 & patent_year<=2014)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc10<-c1
gcc10$year<-2014
gcc10$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseA %>% filter(patent_year>=2005 & patent_year<=2015)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc11<-c1
gcc11$year<-2015
gcc11$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseA %>% filter(patent_year>=2005 & patent_year<=2016)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc12<-c1
gcc12$year<-2016
gcc12$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseA %>% filter(patent_year>=2005 & patent_year<=2017)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc13<-c1
gcc13$year<-2017
gcc13$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseA %>% filter(patent_year>=2005 & patent_year<=2018)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc14<-c1
gcc14$year<-2018
gcc14$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseA %>% filter(patent_year>=2005 & patent_year<=2019)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc15<-c1
gcc15$year<-2019
gcc15$gp<-"GCC Post 2005 subgraph, no past"

gpA<-Reduce(rbind.fill,list(gpA,gcc1,gcc2,gcc3,gcc4,gcc5,gcc6,gcc7,gcc8,gcc9,gcc10,gcc11,gcc12,gcc13,gcc14,gcc15))


#B
b1<-baseB %>% filter(patent_year>=2005 & patent_year<=2005)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc1<-c1
gcc1$year<-2005
gcc1$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseB %>% filter(patent_year>=2005 & patent_year<=2006)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc2<-c1
gcc2$year<-2006
gcc2$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseB %>% filter(patent_year>=2005 & patent_year<=2007)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc3<-c1
gcc3$year<-2007
gcc3$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseB %>% filter(patent_year>=2005 & patent_year<=2008)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc4<-c1
gcc4$year<-2008
gcc4$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseB %>% filter(patent_year>=2005 & patent_year<=2009)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc5<-c1
gcc5$year<-2009
gcc5$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseB %>% filter(patent_year>=2005 & patent_year<=2010)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc6<-c1
gcc6$year<-2010
gcc6$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseB %>% filter(patent_year>=2005 & patent_year<=2011)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc7<-c1
gcc7$year<-2011
gcc7$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseB %>% filter(patent_year>=2005 & patent_year<=2012)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc8<-c1
gcc8$year<-2012
gcc8$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseB %>% filter(patent_year>=2005 & patent_year<=2013)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc9<-c1
gcc9$year<-2013
gcc9$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseB %>% filter(patent_year>=2005 & patent_year<=2014)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc10<-c1
gcc10$year<-2014
gcc10$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseB %>% filter(patent_year>=2005 & patent_year<=2015)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc11<-c1
gcc11$year<-2015
gcc11$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseB %>% filter(patent_year>=2005 & patent_year<=2016)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc12<-c1
gcc12$year<-2016
gcc12$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseB %>% filter(patent_year>=2005 & patent_year<=2017)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc13<-c1
gcc13$year<-2017
gcc13$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseB %>% filter(patent_year>=2005 & patent_year<=2018)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc14<-c1
gcc14$year<-2018
gcc14$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseB %>% filter(patent_year>=2005 & patent_year<=2019)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc15<-c1
gcc15$year<-2019
gcc15$gp<-"GCC Post 2005 subgraph, no past"

gpB<-Reduce(rbind.fill,list(gpB,gcc1,gcc2,gcc3,gcc4,gcc5,gcc6,gcc7,gcc8,gcc9,gcc10,gcc11,gcc12,gcc13,gcc14,gcc15))


##C
#TOTAL
b1<-baseC %>% filter(patent_year>=2005 & patent_year<=2005)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc1<-c1
gcc1$year<-2005
gcc1$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseC %>% filter(patent_year>=2005 & patent_year<=2006)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc2<-c1
gcc2$year<-2006
gcc2$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseC %>% filter(patent_year>=2005 & patent_year<=2007)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc3<-c1
gcc3$year<-2007
gcc3$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseC %>% filter(patent_year>=2005 & patent_year<=2008)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc4<-c1
gcc4$year<-2008
gcc4$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseC %>% filter(patent_year>=2005 & patent_year<=2009)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc5<-c1
gcc5$year<-2009
gcc5$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseC %>% filter(patent_year>=2005 & patent_year<=2010)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc6<-c1
gcc6$year<-2010
gcc6$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseC %>% filter(patent_year>=2005 & patent_year<=2011)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc7<-c1
gcc7$year<-2011
gcc7$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseC %>% filter(patent_year>=2005 & patent_year<=2012)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc8<-c1
gcc8$year<-2012
gcc8$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseC %>% filter(patent_year>=2005 & patent_year<=2013)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc9<-c1
gcc9$year<-2013
gcc9$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseC %>% filter(patent_year>=2005 & patent_year<=2014)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc10<-c1
gcc10$year<-2014
gcc10$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseC %>% filter(patent_year>=2005 & patent_year<=2015)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc11<-c1
gcc11$year<-2015
gcc11$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseC %>% filter(patent_year>=2005 & patent_year<=2016)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc12<-c1
gcc12$year<-2016
gcc12$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseC %>% filter(patent_year>=2005 & patent_year<=2017)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc13<-c1
gcc13$year<-2017
gcc13$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseC %>% filter(patent_year>=2005 & patent_year<=2018)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc14<-c1
gcc14$year<-2018
gcc14$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseC %>% filter(patent_year>=2005 & patent_year<=2019)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc15<-c1
gcc15$year<-2019
gcc15$gp<-"GCC Post 2005 subgraph, no past"

gpC<-Reduce(rbind.fill,list(gpC,gcc1,gcc2,gcc3,gcc4,gcc5,gcc6,gcc7,gcc8,gcc9,gcc10,gcc11,gcc12,gcc13,gcc14,gcc15))


##D
#TOTAL
b1<-baseD %>% filter(patent_year>=2005 & patent_year<=2005)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc1<-c1
gcc1$year<-2005
gcc1$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseD %>% filter(patent_year>=2005 & patent_year<=2006)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc2<-c1
gcc2$year<-2006
gcc2$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseD %>% filter(patent_year>=2005 & patent_year<=2007)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc3<-c1
gcc3$year<-2007
gcc3$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseD %>% filter(patent_year>=2005 & patent_year<=2008)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc4<-c1
gcc4$year<-2008
gcc4$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseD %>% filter(patent_year>=2005 & patent_year<=2009)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc5<-c1
gcc5$year<-2009
gcc5$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseD %>% filter(patent_year>=2005 & patent_year<=2010)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc6<-c1
gcc6$year<-2010
gcc6$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseD %>% filter(patent_year>=2005 & patent_year<=2011)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc7<-c1
gcc7$year<-2011
gcc7$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseD %>% filter(patent_year>=2005 & patent_year<=2012)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc8<-c1
gcc8$year<-2012
gcc8$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseD %>% filter(patent_year>=2005 & patent_year<=2013)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc9<-c1
gcc9$year<-2013
gcc9$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseD %>% filter(patent_year>=2005 & patent_year<=2014)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc10<-c1
gcc10$year<-2014
gcc10$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseD %>% filter(patent_year>=2005 & patent_year<=2015)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc11<-c1
gcc11$year<-2015
gcc11$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseD %>% filter(patent_year>=2005 & patent_year<=2016)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc12<-c1
gcc12$year<-2016
gcc12$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseD %>% filter(patent_year>=2005 & patent_year<=2017)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc13<-c1
gcc13$year<-2017
gcc13$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseD %>% filter(patent_year>=2005 & patent_year<=2018)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc14<-c1
gcc14$year<-2018
gcc14$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseD %>% filter(patent_year>=2005 & patent_year<=2019)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc15<-c1
gcc15$year<-2019
gcc15$gp<-"GCC Post 2005 subgraph, no past"

gpD<-Reduce(rbind.fill,list(gpD,gcc1,gcc2,gcc3,gcc4,gcc5,gcc6,gcc7,gcc8,gcc9,gcc10,gcc11,gcc12,gcc13,gcc14,gcc15))

##E
b1<-baseE %>% filter(patent_year>=2005 & patent_year<=2005)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc1<-c1
gcc1$year<-2005
gcc1$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseE %>% filter(patent_year>=2005 & patent_year<=2006)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc2<-c1
gcc2$year<-2006
gcc2$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseE %>% filter(patent_year>=2005 & patent_year<=2007)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc3<-c1
gcc3$year<-2007
gcc3$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseE %>% filter(patent_year>=2005 & patent_year<=2008)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc4<-c1
gcc4$year<-2008
gcc4$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseE %>% filter(patent_year>=2005 & patent_year<=2009)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc5<-c1
gcc5$year<-2009
gcc5$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseE %>% filter(patent_year>=2005 & patent_year<=2010)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc6<-c1
gcc6$year<-2010
gcc6$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseE %>% filter(patent_year>=2005 & patent_year<=2011)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc7<-c1
gcc7$year<-2011
gcc7$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseE %>% filter(patent_year>=2005 & patent_year<=2012)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc8<-c1
gcc8$year<-2012
gcc8$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseE %>% filter(patent_year>=2005 & patent_year<=2013)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc9<-c1
gcc9$year<-2013
gcc9$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseE %>% filter(patent_year>=2005 & patent_year<=2014)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc10<-c1
gcc10$year<-2014
gcc10$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseE %>% filter(patent_year>=2005 & patent_year<=2015)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc11<-c1
gcc11$year<-2015
gcc11$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseE %>% filter(patent_year>=2005 & patent_year<=2016)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc12<-c1
gcc12$year<-2016
gcc12$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseE %>% filter(patent_year>=2005 & patent_year<=2017)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc13<-c1
gcc13$year<-2017
gcc13$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseE %>% filter(patent_year>=2005 & patent_year<=2018)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc14<-c1
gcc14$year<-2018
gcc14$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseE %>% filter(patent_year>=2005 & patent_year<=2019)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc15<-c1
gcc15$year<-2019
gcc15$gp<-"GCC Post 2005 subgraph, no past"

gpE<-Reduce(rbind.fill,list(gpE,gcc1,gcc2,gcc3,gcc4,gcc5,gcc6,gcc7,gcc8,gcc9,gcc10,gcc11,gcc12,gcc13,gcc14,gcc15))


##F
#TOTAL
b1<-baseF %>% filter(patent_year>=2005 & patent_year<=2005)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc1<-c1
gcc1$year<-2005
gcc1$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseF %>% filter(patent_year>=2005 & patent_year<=2006)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc2<-c1
gcc2$year<-2006
gcc2$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseF %>% filter(patent_year>=2005 & patent_year<=2007)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc3<-c1
gcc3$year<-2007
gcc3$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseF %>% filter(patent_year>=2005 & patent_year<=2008)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc4<-c1
gcc4$year<-2008
gcc4$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseF %>% filter(patent_year>=2005 & patent_year<=2009)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc5<-c1
gcc5$year<-2009
gcc5$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseF %>% filter(patent_year>=2005 & patent_year<=2010)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc6<-c1
gcc6$year<-2010
gcc6$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseF %>% filter(patent_year>=2005 & patent_year<=2011)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc7<-c1
gcc7$year<-2011
gcc7$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseF %>% filter(patent_year>=2005 & patent_year<=2012)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc8<-c1
gcc8$year<-2012
gcc8$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseF %>% filter(patent_year>=2005 & patent_year<=2013)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc9<-c1
gcc9$year<-2013
gcc9$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseF %>% filter(patent_year>=2005 & patent_year<=2014)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc10<-c1
gcc10$year<-2014
gcc10$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseF %>% filter(patent_year>=2005 & patent_year<=2015)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc11<-c1
gcc11$year<-2015
gcc11$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseF %>% filter(patent_year>=2005 & patent_year<=2016)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc12<-c1
gcc12$year<-2016
gcc12$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseF %>% filter(patent_year>=2005 & patent_year<=2017)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc13<-c1
gcc13$year<-2017
gcc13$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseF %>% filter(patent_year>=2005 & patent_year<=2018)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc14<-c1
gcc14$year<-2018
gcc14$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseF %>% filter(patent_year>=2005 & patent_year<=2019)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc15<-c1
gcc15$year<-2019
gcc15$gp<-"GCC Post 2005 subgraph, no past"

gpF<-Reduce(rbind.fill,list(gpF,gcc1,gcc2,gcc3,gcc4,gcc5,gcc6,gcc7,gcc8,gcc9,gcc10,gcc11,gcc12,gcc13,gcc14,gcc15))


##G
b1<-baseG %>% filter(patent_year>=2005 & patent_year<=2005)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc1<-c1
gcc1$year<-2005
gcc1$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseG %>% filter(patent_year>=2005 & patent_year<=2006)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc2<-c1
gcc2$year<-2006
gcc2$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseG %>% filter(patent_year>=2005 & patent_year<=2007)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc3<-c1
gcc3$year<-2007
gcc3$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseG %>% filter(patent_year>=2005 & patent_year<=2008)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc4<-c1
gcc4$year<-2008
gcc4$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseG %>% filter(patent_year>=2005 & patent_year<=2009)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc5<-c1
gcc5$year<-2009
gcc5$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseG %>% filter(patent_year>=2005 & patent_year<=2010)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc6<-c1
gcc6$year<-2010
gcc6$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseG %>% filter(patent_year>=2005 & patent_year<=2011)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc7<-c1
gcc7$year<-2011
gcc7$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseG %>% filter(patent_year>=2005 & patent_year<=2012)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc8<-c1
gcc8$year<-2012
gcc8$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseG %>% filter(patent_year>=2005 & patent_year<=2013)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc9<-c1
gcc9$year<-2013
gcc9$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseG %>% filter(patent_year>=2005 & patent_year<=2014)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc10<-c1
gcc10$year<-2014
gcc10$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseG %>% filter(patent_year>=2005 & patent_year<=2015)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc11<-c1
gcc11$year<-2015
gcc11$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseG %>% filter(patent_year>=2005 & patent_year<=2016)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc12<-c1
gcc12$year<-2016
gcc12$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseG %>% filter(patent_year>=2005 & patent_year<=2017)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc13<-c1
gcc13$year<-2017
gcc13$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseG %>% filter(patent_year>=2005 & patent_year<=2018)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc14<-c1
gcc14$year<-2018
gcc14$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseG %>% filter(patent_year>=2005 & patent_year<=2019)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc15<-c1
gcc15$year<-2019
gcc15$gp<-"GCC Post 2005 subgraph, no past"

gpG<-Reduce(rbind.fill,list(gpG,gcc1,gcc2,gcc3,gcc4,gcc5,gcc6,gcc7,gcc8,gcc9,gcc10,gcc11,gcc12,gcc13,gcc14,gcc15))


##H
b1<-baseH %>% filter(patent_year>=2005 & patent_year<=2005)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc1<-c1
gcc1$year<-2005
gcc1$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseH %>% filter(patent_year>=2005 & patent_year<=2006)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc2<-c1
gcc2$year<-2006
gcc2$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseH %>% filter(patent_year>=2005 & patent_year<=2007)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc3<-c1
gcc3$year<-2007
gcc3$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseH %>% filter(patent_year>=2005 & patent_year<=2008)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc4<-c1
gcc4$year<-2008
gcc4$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseH %>% filter(patent_year>=2005 & patent_year<=2009)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc5<-c1
gcc5$year<-2009
gcc5$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseH %>% filter(patent_year>=2005 & patent_year<=2010)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc6<-c1
gcc6$year<-2010
gcc6$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseH %>% filter(patent_year>=2005 & patent_year<=2011)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc7<-c1
gcc7$year<-2011
gcc7$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseH %>% filter(patent_year>=2005 & patent_year<=2012)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc8<-c1
gcc8$year<-2012
gcc8$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseH %>% filter(patent_year>=2005 & patent_year<=2013)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc9<-c1
gcc9$year<-2013
gcc9$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseH %>% filter(patent_year>=2005 & patent_year<=2014)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc10<-c1
gcc10$year<-2014
gcc10$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseH %>% filter(patent_year>=2005 & patent_year<=2015)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc11<-c1
gcc11$year<-2015
gcc11$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseH %>% filter(patent_year>=2005 & patent_year<=2016)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc12<-c1
gcc12$year<-2016
gcc12$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseH %>% filter(patent_year>=2005 & patent_year<=2017)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc13<-c1
gcc13$year<-2017
gcc13$gp<-"GCC Post 2005 subgraph, no past"


b1<-baseH %>% filter(patent_year>=2005 & patent_year<=2018)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc14<-c1
gcc14$year<-2018
gcc14$gp<-"GCC Post 2005 subgraph, no past"

b1<-baseH %>% filter(patent_year>=2005 & patent_year<=2019)
z<-data.frame(b1$ff,b1$tt)
n1<-graph.data.frame(z,directed = F)
n1<-simplify(n1)
c1<-components(n1)
c1<-table(c1$csize)
c1<-as.data.frame(c1)
c1<-select(c1,Var1)
c1$Var1<-as.character(c1$Var1)
c1$Var1<-as.numeric(c1$Var1)
c1<-rev(c1$Var1)
c1<-as.data.frame(c1)
c1<-c1 %>% slice(1)
c1<-as.data.frame(c1)
names(c1)[1]<-"size"
gcc15<-c1
gcc15$year<-2019
gcc15$gp<-"GCC Post 2005 subgraph, no past"

gpH<-Reduce(rbind.fill,list(gpH,gcc1,gcc2,gcc3,gcc4,gcc5,gcc6,gcc7,gcc8,gcc9,gcc10,gcc11,gcc12,gcc13,gcc14,gcc15))



###PL

#TOTAL
deg1<-table(degree(n1999L))
deg1<-as.data.frame(deg1)
deg1$cat<-1999
deg2<-table(degree(n2009L))
deg2<-as.data.frame(deg2)
deg2$cat<-2009
deg3<-table(degree(n2019L))
deg3<-as.data.frame(deg3)
deg3$cat<-2019
degL<-Reduce(rbind.fill,list(deg1,deg2,deg3))
degL$cat<-as.factor(degL$cat)
names(degL)[1]<-"degree"

degL$degree<-as.character(degL$degree)
degL$degree<-as.numeric(degL$degree)
degL$Freq<-as.character(degL$Freq)
degL$Freq<-as.numeric(degL$Freq)

#A
deg1<-table(degree(n1999A))
deg1<-as.data.frame(deg1)
deg1$cat<-1999
deg2<-table(degree(n2009A))
deg2<-as.data.frame(deg2)
deg2$cat<-2009
deg3<-table(degree(n2019A))
deg3<-as.data.frame(deg3)
deg3$cat<-2019
degA<-Reduce(rbind.fill,list(deg1,deg2,deg3))
degA$cat<-as.factor(degA$cat)
names(degA)[1]<-"degree"

degA$degree<-as.character(degA$degree)
degA$degree<-as.numeric(degA$degree)
degA$Freq<-as.character(degA$Freq)
degA$Freq<-as.numeric(degA$Freq)


#B
deg1<-table(degree(n1999B))
deg1<-as.data.frame(deg1)
deg1$cat<-1999
deg2<-table(degree(n2009B))
deg2<-as.data.frame(deg2)
deg2$cat<-2009
deg3<-table(degree(n2019B))
deg3<-as.data.frame(deg3)
deg3$cat<-2019
degB<-Reduce(rbind.fill,list(deg1,deg2,deg3))
degB$cat<-as.factor(degB$cat)
names(degB)[1]<-"degree"

degB$degree<-as.character(degB$degree)
degB$degree<-as.numeric(degB$degree)
degB$Freq<-as.character(degB$Freq)
degB$Freq<-as.numeric(degB$Freq)

##C
deg1<-table(degree(n1999C))
deg1<-as.data.frame(deg1)
deg1$cat<-1999
deg2<-table(degree(n2009C))
deg2<-as.data.frame(deg2)
deg2$cat<-2009
deg3<-table(degree(n2019C))
deg3<-as.data.frame(deg3)
deg3$cat<-2019
degC<-Reduce(rbind.fill,list(deg1,deg2,deg3))
degC$cat<-as.factor(degC$cat)
names(degC)[1]<-"degree"

degC$degree<-as.character(degC$degree)
degC$degree<-as.numeric(degC$degree)
degC$Freq<-as.character(degC$Freq)
degC$Freq<-as.numeric(degC$Freq)

##D
deg1<-table(degree(n1999D))
deg1<-as.data.frame(deg1)
deg1$cat<-1999
deg2<-table(degree(n2009D))
deg2<-as.data.frame(deg2)
deg2$cat<-2009
deg3<-table(degree(n2019D))
deg3<-as.data.frame(deg3)
deg3$cat<-2019
degD<-Reduce(rbind.fill,list(deg1,deg2,deg3))
degD$cat<-as.factor(degD$cat)
names(degD)[1]<-"degree"

degD$degree<-as.character(degD$degree)
degD$degree<-as.numeric(degD$degree)
degD$Freq<-as.character(degD$Freq)
degD$Freq<-as.numeric(degD$Freq)

##E
deg1<-table(degree(n1999E))
deg1<-as.data.frame(deg1)
deg1$cat<-1999
deg2<-table(degree(n2009E))
deg2<-as.data.frame(deg2)
deg2$cat<-2009
deg3<-table(degree(n2019E))
deg3<-as.data.frame(deg3)
deg3$cat<-2019
degE<-Reduce(rbind.fill,list(deg1,deg2,deg3))
degE$cat<-as.factor(degE$cat)
names(degE)[1]<-"degree"

degE$degree<-as.character(degE$degree)
degE$degree<-as.numeric(degE$degree)
degE$Freq<-as.character(degE$Freq)
degE$Freq<-as.numeric(degE$Freq)

##F
deg1<-table(degree(n1999F))
deg1<-as.data.frame(deg1)
deg1$cat<-1999
deg2<-table(degree(n2009F))
deg2<-as.data.frame(deg2)
deg2$cat<-2009
deg3<-table(degree(n2019F))
deg3<-as.data.frame(deg3)
deg3$cat<-2019
degF<-Reduce(rbind.fill,list(deg1,deg2,deg3))
degF$cat<-as.factor(degF$cat)
names(degF)[1]<-"degree"

degF$degree<-as.character(degF$degree)
degF$degree<-as.numeric(degF$degree)
degF$Freq<-as.character(degF$Freq)
degF$Freq<-as.numeric(degF$Freq)

##G
deg1<-table(degree(n1999G))
deg1<-as.data.frame(deg1)
deg1$cat<-1999
deg2<-table(degree(n2009G))
deg2<-as.data.frame(deg2)
deg2$cat<-2009
deg3<-table(degree(n2019G))
deg3<-as.data.frame(deg3)
deg3$cat<-2019
degG<-Reduce(rbind.fill,list(deg1,deg2,deg3))
degG$cat<-as.factor(degG$cat)
names(degG)[1]<-"degree"

degG$degree<-as.character(degG$degree)
degG$degree<-as.numeric(degG$degree)
degG$Freq<-as.character(degG$Freq)
degG$Freq<-as.numeric(degG$Freq)

##H
deg1<-table(degree(n1999H))
deg1<-as.data.frame(deg1)
deg1$cat<-1999
deg2<-table(degree(n2009H))
deg2<-as.data.frame(deg2)
deg2$cat<-2009
deg3<-table(degree(n2019H))
deg3<-as.data.frame(deg3)
deg3$cat<-2019
degH<-Reduce(rbind.fill,list(deg1,deg2,deg3))
degH$cat<-as.factor(degH$cat)
names(degH)[1]<-"degree"

degH$degree<-as.character(degH$degree)
degH$degree<-as.numeric(degH$degree)
degH$Freq<-as.character(degH$Freq)
degH$Freq<-as.numeric(degH$Freq)


##PL-BOOTSTRAP
#TOTAL
data<-degree(n1999L)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)

data<-degree(n2009L)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)

data<-degree(n2019L)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)


#A
data<-degree(n1999A)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)

data<-degree(n2009A)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)

data<-degree(n2019A)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)

#B
data<-degree(n1999B)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)

data<-degree(n2009B)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)

data<-degree(n2019B)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)

#C
data<-degree(n1999C)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)

data<-degree(n2009C)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)

data<-degree(n2019C)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)


#D
data<-degree(n1999D)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)

data<-degree(n2009D)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)

data<-degree(n2019D)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)


#E
data<-degree(n1999E)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)

data<-degree(n2009E)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)

data<-degree(n2019E)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)


#F
data<-degree(n1999F)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)

data<-degree(n2009F)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)

data<-degree(n2019F)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)


#G
data<-degree(n1999G)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)

data<-degree(n2009G)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)

data<-degree(n2019G)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)


#H
data<-degree(n1999H)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)

data<-degree(n2009H)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)

data<-degree(n2019H)
data<-data[data>0]
m_pl<-dispois$new(data)
est_pl<-estimate_xmin(m_pl)
est_pl$xmin
est_pl$pars
est_pl$gof
m_pl$setXmin(est_pl)
bs_pl<-bootstrap_p(m_pl,no_of_sims = 1000,threads = 8,seed=123)




### pfit most important component Total
big <- decompose(n2019L, min.vertices=gcc)
big<-big[[1]]
bb<-as_data_frame(big)
bb<-PAFit::to_igraph(Pnet)
bb <- decompose(bb, min.vertices=gcc)
bb<-bb[[1]]
Pbig<-as.PAFit_net(bb,type="undirected")
View(bb)
com<-components(Pnet)
ba<-as_data_frame(bb)
View(ba)
z<-data.frame(ba$from,ba$to,ba$time)
names(z)[1]<-"ff_inv"
names(z)[2]<-"to_inv"
names(z)[3]<-"t_pat"
Pbig<-as.PAFit_net(z,type="undirected")
sPbig<-get_statistics(Pbig)
z$ff_inv<-as.numeric(z$ff_inv)
Z$to_inv<-as.numeric(z$to_inv)
Pbig<-as.PAFit_net(z,type="undirected")
sPbig<-get_statistics(Pbig)
result_Pbig<-only_A_estimate(Pbig,sPbig)
pPAF_L<-plot(result_Pbig,sPbig,line="TRUE",cex=2,cex.axis=2,cex.lab=2)
summPAF_L<-summary(sPbig)


### pfit most important component A
big <- decompose(n2019A, min.vertices=gcc)
big<-big[[1]]
bb<-as_data_frame(big)
bb<-PAFit::to_igraph(Pnet)
bb <- decompose(bb, min.vertices=gcc)
bb<-bb[[1]]
Pbig<-as.PAFit_net(bb,type="undirected")
View(bb)
com<-components(Pnet)
ba<-as_data_frame(bb)
View(ba)
z<-data.frame(ba$from,ba$to,ba$time)
names(z)[1]<-"ff_inv"
names(z)[2]<-"to_inv"
names(z)[3]<-"t_pat"
Pbig<-as.PAFit_net(z,type="undirected")
sPbig<-get_statistics(Pbig)
z$ff_inv<-as.numeric(z$ff_inv)
Z$to_inv<-as.numeric(z$to_inv)
Pbig<-as.PAFit_net(z,type="undirected")
sPbig<-get_statistics(Pbig)
result_Pbig<-only_A_estimate(Pbig,sPbig)
pPAF_A<-plot(result_Pbig,sPbig,line="TRUE",cex=2,cex.axis=2,cex.lab=2)
summPAF_A<-summary(sPbig)

### pfit most important component B
big <- decompose(n2019B, min.vertices=gcc)
big<-big[[1]]
bb<-as_data_frame(big)
bb<-PAFit::to_igraph(Pnet)
bb <- decompose(bb, min.vertices=gcc)
bb<-bb[[1]]
Pbig<-as.PAFit_net(bb,type="undirected")
View(bb)
com<-components(Pnet)
ba<-as_data_frame(bb)
View(ba)
z<-data.frame(ba$from,ba$to,ba$time)
names(z)[1]<-"ff_inv"
names(z)[2]<-"to_inv"
names(z)[3]<-"t_pat"
Pbig<-as.PAFit_net(z,type="undirected")
sPbig<-get_statistics(Pbig)
z$ff_inv<-as.numeric(z$ff_inv)
Z$to_inv<-as.numeric(z$to_inv)
Pbig<-as.PAFit_net(z,type="undirected")
sPbig<-get_statistics(Pbig)
result_Pbig<-only_A_estimate(Pbig,sPbig)
pPAF_B<-plot(result_Pbig,sPbig,line="TRUE",cex=2,cex.axis=2,cex.lab=2)
summPAF_B<-summary(sPbig)


### pfit most important component C
big <- decompose(n2019C, min.vertices=gcc)
big<-big[[1]]
bb<-as_data_frame(big)
bb<-PAFit::to_igraph(Pnet)
bb <- decompose(bb, min.vertices=gcc)
bb<-bb[[1]]
Pbig<-as.PAFit_net(bb,type="undirected")
View(bb)
com<-components(Pnet)
ba<-as_data_frame(bb)
View(ba)
z<-data.frame(ba$from,ba$to,ba$time)
names(z)[1]<-"ff_inv"
names(z)[2]<-"to_inv"
names(z)[3]<-"t_pat"
Pbig<-as.PAFit_net(z,type="undirected")
sPbig<-get_statistics(Pbig)
z$ff_inv<-as.numeric(z$ff_inv)
Z$to_inv<-as.numeric(z$to_inv)
Pbig<-as.PAFit_net(z,type="undirected")
sPbig<-get_statistics(Pbig)
result_Pbig<-only_A_estimate(Pbig,sPbig)
pPAF_C<-plot(result_Pbig,sPbig,line="TRUE",cex=2,cex.axis=2,cex.lab=2)
summPAF_C<-summary(sPbig)


### pfit most important component D
big <- decompose(n2019D, min.vertices=gcc)
big<-big[[1]]
bb<-as_data_frame(big)
bb<-PAFit::to_igraph(Pnet)
bb <- decompose(bb, min.vertices=gcc)
bb<-bb[[1]]
Pbig<-as.PAFit_net(bb,type="undirected")
View(bb)
com<-components(Pnet)
ba<-as_data_frame(bb)
View(ba)
z<-data.frame(ba$from,ba$to,ba$time)
names(z)[1]<-"ff_inv"
names(z)[2]<-"to_inv"
names(z)[3]<-"t_pat"
Pbig<-as.PAFit_net(z,type="undirected")
sPbig<-get_statistics(Pbig)
z$ff_inv<-as.numeric(z$ff_inv)
Z$to_inv<-as.numeric(z$to_inv)
Pbig<-as.PAFit_net(z,type="undirected")
sPbig<-get_statistics(Pbig)
result_Pbig<-only_A_estimate(Pbig,sPbig)
pPAF_D<-plot(result_Pbig,sPbig,line="TRUE",cex=2,cex.axis=2,cex.lab=2)
summPAF_D<-summary(sPbig)


### pfit most important component E
big <- decompose(n2019E, min.vertices=gcc)
big<-big[[1]]
bb<-as_data_frame(big)
bb<-PAFit::to_igraph(Pnet)
bb <- decompose(bb, min.vertices=gcc)
bb<-bb[[1]]
Pbig<-as.PAFit_net(bb,type="undirected")
View(bb)
com<-components(Pnet)
ba<-as_data_frame(bb)
View(ba)
z<-data.frame(ba$from,ba$to,ba$time)
names(z)[1]<-"ff_inv"
names(z)[2]<-"to_inv"
names(z)[3]<-"t_pat"
Pbig<-as.PAFit_net(z,type="undirected")
sPbig<-get_statistics(Pbig)
z$ff_inv<-as.numeric(z$ff_inv)
Z$to_inv<-as.numeric(z$to_inv)
Pbig<-as.PAFit_net(z,type="undirected")
sPbig<-get_statistics(Pbig)
result_Pbig<-only_A_estimate(Pbig,sPbig)
pPAF_E<-plot(result_Pbig,sPbig,line="TRUE",cex=2,cex.axis=2,cex.lab=2)
summPAF_E<-summary(sPbig)


### pfit most important component F
big <- decompose(n2019F, min.vertices=gcc)
big<-big[[1]]
bb<-as_data_frame(big)
bb<-PAFit::to_igraph(Pnet)
bb <- decompose(bb, min.vertices=gcc)
bb<-bb[[1]]
Pbig<-as.PAFit_net(bb,type="undirected")
View(bb)
com<-components(Pnet)
ba<-as_data_frame(bb)
View(ba)
z<-data.frame(ba$from,ba$to,ba$time)
names(z)[1]<-"ff_inv"
names(z)[2]<-"to_inv"
names(z)[3]<-"t_pat"
Pbig<-as.PAFit_net(z,type="undirected")
sPbig<-get_statistics(Pbig)
z$ff_inv<-as.numeric(z$ff_inv)
Z$to_inv<-as.numeric(z$to_inv)
Pbig<-as.PAFit_net(z,type="undirected")
sPbig<-get_statistics(Pbig)
result_Pbig<-only_A_estimate(Pbig,sPbig)
pPAF_F<-plot(result_Pbig,sPbig,line="TRUE",cex=2,cex.axis=2,cex.lab=2)
summPAF_F<-summary(sPbig)


### pfit most important component G
big <- decompose(n2019G, min.vertices=gcc)
big<-big[[1]]
bb<-as_data_frame(big)
bb<-PAFit::to_igraph(Pnet)
bb <- decompose(bb, min.vertices=gcc)
bb<-bb[[1]]
Pbig<-as.PAFit_net(bb,type="undirected")
View(bb)
com<-components(Pnet)
ba<-as_data_frame(bb)
View(ba)
z<-data.frame(ba$from,ba$to,ba$time)
names(z)[1]<-"ff_inv"
names(z)[2]<-"to_inv"
names(z)[3]<-"t_pat"
Pbig<-as.PAFit_net(z,type="undirected")
sPbig<-get_statistics(Pbig)
z$ff_inv<-as.numeric(z$ff_inv)
Z$to_inv<-as.numeric(z$to_inv)
Pbig<-as.PAFit_net(z,type="undirected")
sPbig<-get_statistics(Pbig)
result_Pbig<-only_A_estimate(Pbig,sPbig)
pPAF_G<-plot(result_Pbig,sPbig,line="TRUE",cex=2,cex.axis=2,cex.lab=2)
summPAF_G<-summary(sPbig)


### pfit most important component H
big <- decompose(n2019H, min.vertices=gcc)
big<-big[[1]]
bb<-as_data_frame(big)
bb<-PAFit::to_igraph(Pnet)
bb <- decompose(bb, min.vertices=gcc)
bb<-bb[[1]]
Pbig<-as.PAFit_net(bb,type="undirected")
View(bb)
com<-components(Pnet)
ba<-as_data_frame(bb)
View(ba)
z<-data.frame(ba$from,ba$to,ba$time)
names(z)[1]<-"ff_inv"
names(z)[2]<-"to_inv"
names(z)[3]<-"t_pat"
Pbig<-as.PAFit_net(z,type="undirected")
sPbig<-get_statistics(Pbig)
z$ff_inv<-as.numeric(z$ff_inv)
Z$to_inv<-as.numeric(z$to_inv)
Pbig<-as.PAFit_net(z,type="undirected")
sPbig<-get_statistics(Pbig)
result_Pbig<-only_A_estimate(Pbig,sPbig)
pPAF_H<-plot(result_Pbig,sPbig,line="TRUE",cex=2,cex.axis=2,cex.lab=2)
summPAF_H<-summary(sPbig)


#Figure 2


ggplot(degL, aes(x=degree, y=Freq, col=cat)) + 
  geom_point(aes(colour=cat))+
  scale_y_continuous("Frequency", trans = "log10",labels = trans_format("log10", math_format(10^.x)))+
  scale_x_continuous("Degree", trans = "log10",labels = trans_format("log10", math_format(10^.x)))+
  scale_color_manual(values = c("gray75", "gray45", "gray10"))+
  theme_bw()+
  theme(legend.position = "none",panel.grid.major=element_blank(),panel.grid.minor=element_blank())

ggplot(degA, aes(x=degree, y=Freq, col=cat)) + 
  geom_point(aes(colour=cat))+
  scale_y_continuous("Frequency", trans = "log10",labels = trans_format("log10", math_format(10^.x)))+
  scale_x_continuous("Degree", trans = "log10",labels = trans_format("log10", math_format(10^.x)))+
  scale_color_manual(values = c("gray75", "gray45", "gray10"))+
  theme_bw()+
  theme(legend.position = "none",panel.grid.major=element_blank(),panel.grid.minor=element_blank())

ggplot(degB, aes(x=degree, y=Freq, col=cat)) + 
  geom_point(aes(colour=cat))+
  scale_y_continuous("Frequency", trans = "log10",labels = trans_format("log10", math_format(10^.x)))+
  scale_x_continuous("Degree", trans = "log10",labels = trans_format("log10", math_format(10^.x)))+
  scale_color_manual(values = c("gray75", "gray45", "gray10"))+
  theme_bw()+
  theme(legend.position = "none",panel.grid.major=element_blank(),panel.grid.minor=element_blank())

ggplot(degC, aes(x=degree, y=Freq, col=cat)) + 
  geom_point(aes(colour=cat))+
  scale_y_continuous("Frequency", trans = "log10",labels = trans_format("log10", math_format(10^.x)))+
  scale_x_continuous("Degree", trans = "log10",labels = trans_format("log10", math_format(10^.x)))+
  scale_color_manual(values = c("gray75", "gray45", "gray10"))+
  theme_bw()+
  theme(legend.position = "none",panel.grid.major=element_blank(),panel.grid.minor=element_blank())

ggplot(degD, aes(x=degree, y=Freq, col=cat)) + 
  geom_point(aes(colour=cat))+
  scale_y_continuous("Frequency", trans = "log10",labels = trans_format("log10", math_format(10^.x)))+
  scale_x_continuous("Degree", trans = "log10",labels = trans_format("log10", math_format(10^.x)))+
  scale_color_manual(values = c("gray75", "gray45", "gray10"))+
  theme_bw()+
  theme(legend.position = "none",panel.grid.major=element_blank(),panel.grid.minor=element_blank())

ggplot(degE, aes(x=degree, y=Freq, col=cat)) + 
  geom_point(aes(colour=cat))+
  scale_y_continuous("Frequency", trans = "log10",labels = trans_format("log10", math_format(10^.x)))+
  scale_x_continuous("Degree", trans = "log10",labels = trans_format("log10", math_format(10^.x)))+
  scale_color_manual(values = c("gray75", "gray45", "gray10"))+
  theme_bw()+
  theme(legend.position = "none",panel.grid.major=element_blank(),panel.grid.minor=element_blank())

ggplot(degF, aes(x=degree, y=Freq, col=cat)) + 
  geom_point(aes(colour=cat))+
  scale_y_continuous("Frequency", trans = "log10",labels = trans_format("log10", math_format(10^.x)))+
  scale_x_continuous("Degree", trans = "log10",labels = trans_format("log10", math_format(10^.x)))+
  scale_color_manual(values = c("gray75", "gray45", "gray10"))+
  theme_bw()+
  theme(legend.position = "none",panel.grid.major=element_blank(),panel.grid.minor=element_blank())

ggplot(degG, aes(x=degree, y=Freq, col=cat)) + 
  geom_point(aes(colour=cat))+
  scale_y_continuous("Frequency", trans = "log10",labels = trans_format("log10", math_format(10^.x)))+
  scale_x_continuous("Degree", trans = "log10",labels = trans_format("log10", math_format(10^.x)))+
  scale_color_manual(values = c("gray75", "gray45", "gray10"))+
  theme_bw()+
  theme(legend.position = "none",panel.grid.major=element_blank(),panel.grid.minor=element_blank())

ggplot(degH, aes(x=degree, y=Freq, col=cat)) + 
  geom_point(aes(colour=cat))+
  scale_y_continuous("Frequency", trans = "log10",labels = trans_format("log10", math_format(10^.x)))+
  scale_x_continuous("Degree", trans = "log10",labels = trans_format("log10", math_format(10^.x)))+
  scale_color_manual(values = c("gray75", "gray45", "gray10"))+
  theme_bw()+
  theme(legend.position = "none",panel.grid.major=element_blank(),panel.grid.minor=element_blank())


#Figure 4





# Figure 5

ggplot(neL, aes(x=lnode, y=ledge)) + 
  geom_point(size=2.5)+
  scale_y_continuous("Number of edges")+
  scale_x_continuous("Number of nodes")+
  theme_bw()+
  theme(legend.position = "none",panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_smooth(method="lm",se=FALSE,color="black")

ggplot(neA, aes(x=lnode, y=ledge)) + 
  geom_point(size=2.5)+
  scale_y_continuous("Number of edges")+
  scale_x_continuous("Number of nodes")+
  theme_bw()+
  theme(legend.position = "none",panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_smooth(method="lm",se=FALSE,color="black")

ggplot(neB, aes(x=lnode, y=ledge)) + 
  geom_point(size=2.5)+
  scale_y_continuous("Number of edges")+
  scale_x_continuous("Number of nodes")+
  theme_bw()+
  theme(legend.position = "none",panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_smooth(method="lm",se=FALSE,color="black")

ggplot(neC, aes(x=lnode, y=ledge)) + 
  geom_point(size=2.5)+
  scale_y_continuous("Number of edges")+
  scale_x_continuous("Number of nodes")+
  theme_bw()+
  theme(legend.position = "none",panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_smooth(method="lm",se=FALSE,color="black")

ggplot(neD, aes(x=lnode, y=ledge)) + 
  geom_point(size=2.5)+
  scale_y_continuous("Number of edges")+
  scale_x_continuous("Number of nodes")+
  theme_bw()+
  theme(legend.position = "none",panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_smooth(method="lm",se=FALSE,color="black")

ggplot(neE, aes(x=lnode, y=ledge)) + 
  geom_point(size=2.5)+
  scale_y_continuous("Number of edges")+
  scale_x_continuous("Number of nodes")+
  theme_bw()+
  theme(legend.position = "none",panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_smooth(method="lm",se=FALSE,color="black")

ggplot(neF, aes(x=lnode, y=ledge)) + 
  geom_point(size=2.5)+
  scale_y_continuous("Number of edges")+
  scale_x_continuous("Number of nodes")+
  theme_bw()+
  theme(legend.position = "none",panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_smooth(method="lm",se=FALSE,color="black")

ggplot(neG, aes(x=lnode, y=ledge)) + 
  geom_point(size=2.5)+
  scale_y_continuous("Number of edges")+
  scale_x_continuous("Number of nodes")+
  theme_bw()+
  theme(legend.position = "none",panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_smooth(method="lm",se=FALSE,color="black")

ggplot(neH, aes(x=lnode, y=ledge)) + 
  geom_point(size=2.5)+
  scale_y_continuous("Number of edges")+
  scale_x_continuous("Number of nodes")+
  theme_bw()+
  theme(legend.position = "none",panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_smooth(method="lm",se=FALSE,color="black")


lmL = lm(ledge~lnode, data = neL)
lmA = lm(ledge~lnode, data = neA)
lmB = lm(ledge~lnode, data = neB)
lmC = lm(ledge~lnode, data = neC)
lmD = lm(ledge~lnode, data = neD)
lmE = lm(ledge~lnode, data = neE)
lmF = lm(ledge~lnode, data = neF)
lmG = lm(ledge~lnode, data = neG)
lmH = lm(ledge~lnode, data = neH)


summary(lmL)
summary(lmA)
summary(lmB)
summary(lmC)
summary(lmD)
summary(lmE)
summary(lmF)
summary(lmG)
summary(lmH)

# Figure 6

gpL$class<-"Total"
gpA$class<-"A"
gpB$class<-"B"
gpC$class<-"C"
gpD$class<-"D"
gpE$class<-"E"
gpF$class<-"F"
gpG$class<-"G"
gpH$class<-"H"

gR<-Reduce(rbind.fill,list(gpL,gpA,gpB,gpC,gpD,gpE,gpF,gpG,gpH))

p<-ggplot(gR, aes(x=year, y=size, col=gp,group=gp)) + 
  geom_line(aes(linetype=gp,colour=gp))+
  scale_y_continuous("CC Size (log lin)", trans = "log10")+
  scale_x_continuous("Year",breaks = seq(from = 1999, to = 2019, by = 1))+
  scale_linetype_manual(values=c("solid", "dotdash", "solid", "solid"))+
  theme_bw()+scale_color_manual(values = c("gray25", "gray25", "gray45","gray75"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

p + facet_wrap( ~ class, nrow = 3) + 
  theme(legend.title= element_blank(),legend.position = "bottom")


# Diversification analysis


t1 <- table(base1$inventor_id,base1$ipc_section,base1$year_fil)
t2 <- ifelse(t1>0,1,0)
t3 <- table(base1$patent_id,base1$ipc_section,base1$year_fil)
t4 <- ifelse(t3>0,1,0)
sec <- lapply(1:dim(t2)[3], function(x) tcrossprod(t(t2[ , ,x])))
prod <- lapply(1:dim(t2)[3], function(x) colSums(t2[ , ,x]))
prod <- t(do.call(rbind.data.frame,prod))
rownames(prod) <- LETTERS[1:8]
colnames(prod) <- 1999:2019



sec1 <- lapply(1:length(sec), function(x) sec[[x]]/rowSums(sec[[x]]))
secT <- Reduce('+',sec)
secT1 <- secT/rowSums(secT)

secp1 <- Reduce('+',sec[1:6])
secp2 <- Reduce('+',sec[7:11])
secp3 <- Reduce('+',sec[12:16])
secp4 <- Reduce('+',sec[16:21])

secp1.1 <- secp1/rowSums(secp1)
secp2.1 <- secp2/rowSums(secp2)
secp3.1 <- secp3/rowSums(secp3)
secp4.1 <- secp4/rowSums(secp4)

chisq.test(secp4,secp3)
chisq.test(secp2,secp3)
chisq.test(secp2,secp1)

plots <- list()

plots[[1]] <-  ggplot(reshape2::melt(secp1.1),aes(Var2,Var1,value,label=sprintf("%0.3f", round(value, digits = 3))))+geom_tile(aes(fill=value),color="black",size=.3)+scale_fill_gradient(low = "white", high = "blue")+ labs(x=NULL, y=NULL,title=NULL)+geom_text(size=4) +theme(axis.title=element_text(size=16,face="bold"),axis.text = element_text(size=14), axis.line = element_line())+ylim(rev(levels(reshape2::melt(secp1.1)$Var1)))+ggtitle("1999-2004")
plots[[2]] <-  ggplot(reshape2::melt(secp2.1),aes(Var2,Var1,value,label=sprintf("%0.3f", round(value, digits = 3))))+geom_tile(aes(fill=value),color="black",size=.3)+scale_fill_gradient(low = "white", high = "blue")+ labs(x=NULL, y=NULL,title=NULL)+geom_text(size=4) +theme(axis.title=element_text(size=16,face="bold"),axis.text = element_text(size=14), axis.line = element_line())+ylim(rev(levels(reshape2::melt(secp2.1)$Var1)))+ggtitle("2005-2009")
plots[[3]] <-  ggplot(reshape2::melt(secp3.1),aes(Var2,Var1,value,label=sprintf("%0.3f", round(value, digits = 3))))+geom_tile(aes(fill=value),color="black",size=.3)+scale_fill_gradient(low = "white", high = "blue")+ labs(x=NULL, y=NULL,title=NULL)+geom_text(size=4) +theme(axis.title=element_text(size=16,face="bold"),axis.text = element_text(size=14), axis.line = element_line())+ylim(rev(levels(reshape2::melt(secp3.1)$Var1)))+ggtitle("2010-2014")
plots[[4]] <-  ggplot(reshape2::melt(secp4.1),aes(Var2,Var1,value,label=sprintf("%0.3f", round(value, digits = 3))))+geom_tile(aes(fill=value),color="black",size=.3)+scale_fill_gradient(low = "white", high = "blue")+ labs(x=NULL, y=NULL,title=NULL)+geom_text(size=4) +theme(axis.title=element_text(size=16,face="bold"),axis.text = element_text(size=14), axis.line = element_line())+ylim(rev(levels(reshape2::melt(secp4.1)$Var1)))+ggtitle("2015-2019")
plots[[5]] <-  ggplot(reshape2::melt(secT1),aes(Var2,Var1,value,label=sprintf("%0.3f", round(value, digits = 3))))+geom_tile(aes(fill=value),color="black",size=.3)+scale_fill_gradient(low = "white", high = "blue")+ labs(x=NULL, y=NULL,title=NULL)+geom_text(size=4) +theme(axis.title=element_text(size=16,face="bold"),axis.text = element_text(size=14), axis.line = element_line())+ylim(rev(levels(reshape2::melt(secT1)$Var1)))+ggtitle("Total")


#fig 7

a <- gridExtra::grid.arrange(grobs=plots,col=7)


d1 <- melt(diamL)
colnames(d1) <- c("Section","Year","Diameter")


tca <- (diam[,3:22]-diam[,2:21])/diam[,2:21]
tca <- as.data.frame(tca)

d2 <- melt(tca)
colnames(d2) <- c("Section","Year","tca")

t.min <- matrix(colMeans(tca)-apply(tca,2,sd),nrow=8,ncol=20,byrow =T)
t.max <- matrix(colMeans(tca)+apply(tca,2,sd),nrow=8,ncol=20,byrow =T)
t.min <- as.data.frame(t.min)
t.max <- as.data.frame(t.max)
d3 <- d2
min.t <- melt(t.min)
max.t <- melt(t.max)
colnames(d3) <- c("Section","Year","tca")
d3["low"] <- min.t[,3]
d3["max"] <- max.t[,3]


# fig 4

grid.col=c("A"="orange2","B"="magenta3","C"="cyan4","D"="mediumpurple","E"="red3","F"="gold2","H"="blue","G"="green3")
ggplot(d1,aes(x=Year,y=Diameter,color=Section,group=Section))+geom_line()+geom_point()+scale_color_manual(values=c(grid.col))
ggplot(subset(d3),aes(x=Year,y=tca,color=Section,group=Section))+geom_line()+geom_point()+scale_color_manual(values=c(grid.col))+ylab("Diameter rate of absolute growth")


p.sec <- sapply(1:length(sec1),function(x) 1-diag(sec1[[x]]))
colnames(p.sec) <- 1999:2019

#Spearman's cor

lapply(1:nrow(prod),function(x) cor.test(p.sec[x,],prod[x,],method = "spearman"))






