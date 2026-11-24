df<-read.csv("SaratogaHouses.csv",sep=",")
df<-df[,-1]

df$livingArea[c(10,150,420,760,1100,1500)]<-NA
df$lotSize[c(25,300,650,980,1350,1650)]<-NA
df$heating[c(40,210,530,875,1200,1600)]<-NA
df$fuel[c(55,260,590,910,1280,1700)]<-NA

df$bedrooms[c(70,480,1020)]<-df$bedrooms[c(70,480,1020)]+c(0.4,-0.3,0.2)
df$rooms[c(90,510,1400)]<-df$rooms[c(90,510,1400)]+c(-0.2,0.3,0.4)
df$centralAir[c(100,600,1150)]<-c("no","YES","yes")
df$sewer[c(120,700,1450)]<-toupper(df$sewer[c(120,700,1450)])

df$age[c(130,820)]<-c(-5,-20)
df$pctCollege[c(140,940)]<-c(120,150)

write.csv(df,"SaratogaHouses_updated.csv",row.names = FALSE)