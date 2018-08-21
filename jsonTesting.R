


  usr='Public'
  pwd='Public'
  siteid = 'op36073'
  siteid = 'cosmoz.site.10.plat'
  sensortype = 'Soil-Moisture'
  aggperiod = timeSteps$days
  startdate = '01-01-2018'
  enddate = '05-01-2018'

  sensorid = 'cosmoz.site.2.soil_moisture_filtered'


sd <- getSensorDataStreams(usr=usr, pwd=pwd, siteID=siteid, sensorType=sensortype, startDate=startdate, endDate=enddate,  outFormat = 'nestedTS')
DF <- to.DF(sd)



x <- data.frame(matrix(NA, nrow= ncol(DF)-1, ncol=4))
x$X1<- siteid
x$X2 <- sensortype
x$X3 <- names(DF)[2:3]

TS <- vector("list", ncol(DF)-1)

for (i in 1 : (ncol(DF)-1)) {
  rdf <- data.frame(t=DF$DateTime, v=DF[i+1])
  colnames(rdf) <- c('t','v')
  TS[[i]] <- rdf
}
x$X4 <- I(TS)
colnames(x)<- c('siteid', 'sensortype', 'SensorID', 'Values')
jsn <- prettify(toJSON(x))
cat(jsn, file='c:/temp/aj.json')

xin <- fromJSON('c:/temp/aj.json')




x$DataStream <-I(list(data.frame(t=c("A","C", "X"), v=c("B","D", "G")), data.frame(t=c(44,55, 77), v=c("8","9", "G"))))



x$DataStream[1] <-I(data.frame(t=c("A","C", "X"), v=c("B","D", "G")))
x

jsn <- prettify(toJSON(x))
cat(jsn, file='c:/temp/aj.json')

xin <- fromJSON('c:/temp/aj.json')

x == xin





a <- data.frame(c1 = c(1,2,3))

a$c2 <- seq(4:6)
a$c2[1] <-I(list(list("a", "b", "c"), list(1,2,3)))






l1 <- data.frame(t=as.character(c('2017-08-02 23:30:09', '2017-08-03 20:00:05', '2017-08-03 20:30:05')), v=c(1,2,3))
l2 <- data.frame(t=as.character(c('2017-08-02 23:30:09', '2017-08-03 20:00:05', '2017-08-03 20:30:05')), v=c(4,5,6))


names <- c('s1', 's2')
vals <- list(data.frame(l1),data.frame(l2))

tdf <- data.frame(names, vals)

ts <- fromJSON('c:/temp/tesing.json', flatten = F, simplifyVector = F, simplifyDataFrame = F)

ids <- fromJSON('c:/temp/tesing.json')
ids <- fromJSON('c:/temp/tesing.json')[[1]][[1]]
depths <- fromJSON('c:/temp/tesing.json')[[1]][[2]]
ts <- fromJSON('c:/temp/tesing.json')[[1]][[3]]

j1 <- flatten(ts$recs[[1]])
Sensors <-  vector("list", 8)

for(i in 1:8){

  SensorID = c("test1")
  SensorDepth = c("Depth1")
  values <- vector("list", 3)

}





makeDFList <- function(DF) {

  l<- vector("list", ncol(DF)-1)
  for(i in 1:(ncol(DF)-1)){
    nd <-  cbind(DF[1], DF[i+1])
    names(nd) <- c('T', 'V')
    l[[i]] <-nd
  }
  names(l) <- names(DF)[-1]
  l

}










x
#             X1         X2           X3
# 1     1:2, 1:2        2:3      a, b, c
# 2 1, 2, 3,.... 1, 2, 3, 4 1, 2, 3,....
x[1, 1]
# [[1]]
#   a b
# 1 A B
# 2 C D
#
x[2, 1]
# [[1]]
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    1    3    5    7    9
# [2,]    2    4    6    8   10



















