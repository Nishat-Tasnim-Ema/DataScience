df<-read.csv("SaratogaHouses_updated.csv",sep=",")
head(df)
dim(df)
str(df)
summary(df)


colSums(is.na(df))
df_discard<-na.omit(df)
dim(df_discard)
df$livingArea[is.na(df$livingArea)]<-mean(df$livingArea,na.rm = TRUE)
df$lotSize[is.na(df$lotSize)]<-mean(df$lotSize,na.rm = TRUE)
df$heating[is.na(df$heating)]<-names(which.max(table(df$heating)))
df$fuel[is.na(df$fuel)]<-names(which.max(table(df$fuel)))
colSums(is.na(df))


table(df$centralAir)
table(df$sewer)
df$bedrooms[df$bedrooms%%1>0]
df$rooms[df$rooms%%1>0]
df$centralAir<-ifelse(tolower(df$centralAir)=="yes","Yes","No")
df$sewer<-tolower(df$sewer)
df$bedrooms<-floor(df$bedrooms+0.5)
df$rooms<-floor(df$rooms+0.5)
table(df$centralAir)
table(df$sewer)


str(df)
df$age<-ifelse(df$age<0,NA,df$age)
df$pctCollege<-ifelse(df$pctCollege>100,NA,df$pctCollege)
df$lotSize<-ifelse(df$lotSize==0,NA,df$lotSize)
df$bathrooms<-ifelse(df$bathrooms==0,NA,df$bathrooms)
df$landValue<-ifelse(df$landValue>df$price,NA,df$landValue)
colSums(is.na(df))
df<-na.omit(df)
dim(df)


df_model<-df
df_model$waterfront<-ifelse(df_model$waterfront=="Yes",1,0)
df_model$newConstruction<-ifelse(df_model$newConstruction=="Yes",1,0)
df_model$centralAir<-ifelse(df_model$centralAir=="Yes",1,0)
df_model$heating<-ifelse(df_model$heating=="hot air",1,
                         ifelse(df_model$heating=="hot water/steam",2,3))
df_model$fuel<-ifelse(df_model$fuel=="gas",1,
                      ifelse(df_model$fuel=="electric",2,3))
df_model$sewer<-ifelse(df_model$sewer=="public/commercial",1,
                       ifelse(df_model$sewer=="septic",2,3))
str(df_model)
minmax<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
df_model$lotSize<-minmax(df_model$lotSize)
df_model$age<-minmax(df_model$age)
df_model$landValue<-minmax(df_model$landValue)
df_model$livingArea<-minmax(df_model$livingArea)
df_model$pctCollege<-minmax(df_model$pctCollege)
df_model$bedrooms<-minmax(df_model$bedrooms)
df_model$fireplaces<-minmax(df_model$fireplaces)
df_model$bathrooms<-minmax(df_model$bathrooms)
df_model$rooms<-minmax(df_model$rooms)
summary(df_model)


par(mfrow=c(2,2))
boxplot(df$price,main="price")
boxplot(df$livingArea,main="livingArea")
boxplot(df$lotSize,main="lotSize")
boxplot(df$landValue,main="landValue")
par(mfrow=c(1,1))
q1<-summary(df$price)[2]
q3<-summary(df$price)[5]
iqr<-q3-q1
lower<-q1-1.5*iqr
upper<-q3+1.5*iqr
lower
upper
df$price<-ifelse(df$price<lower,NA,df$price)
df$price<-ifelse(df$price>upper,NA,df$price)
df_model$price<-ifelse(df_model$price<lower,NA,df_model$price)
df_model$price<-ifelse(df_model$price>upper,NA,df_model$price)
colSums(is.na(df))
df<-na.omit(df)
df_model<-na.omit(df_model)
dim(df)
dim(df_model)
boxplot(df$price,main="price after removing outliers")


summary(df)
mean(df$price)
sd(df$price)
var(df$price)
min(df$price)
max(df$price)
range(df$price)
table(df$heating)
names(which.max(table(df$heating)))
par(mfrow=c(2,2))
hist(df$price,main="price",col="red")
hist(df$livingArea,main="livingArea",col="red")
hist(df$age,main="age",col="red")
hist(df$landValue,main="landValue",col="red")
par(mfrow=c(1,1))
barplot(table(df$heating),main="heating")
pie(table(df$fuel),main="fuel")
plot(df$livingArea,df$price,main="livingArea vs price")
d<-density(df$price)
plot(d,main="Density of price")
polygon(d,col="red",border="blue")


set.seed(123)
n<-dim(df_model)[1]
train_index<-sample(1:n,floor(0.8*n))
train<-df_model[train_index,]
test<-df_model[-train_index,]
dim(train)
dim(test)


correlation<-function(x,y){
  return(sum((x-mean(x))*(y-mean(y)))/sqrt(sum((x-mean(x))^2)*sum((y-mean(y))^2)))
}
feature<-c("lotSize","age","landValue","livingArea","pctCollege","bedrooms",
           "fireplaces","bathrooms","rooms","heating","fuel","sewer",
           "waterfront","newConstruction","centralAir")
cor_value<-c(correlation(train$lotSize,train$price),
             correlation(train$age,train$price),
             correlation(train$landValue,train$price),
             correlation(train$livingArea,train$price),
             correlation(train$pctCollege,train$price),
             correlation(train$bedrooms,train$price),
             correlation(train$fireplaces,train$price),
             correlation(train$bathrooms,train$price),
             correlation(train$rooms,train$price),
             correlation(train$heating,train$price),
             correlation(train$fuel,train$price),
             correlation(train$sewer,train$price),
             correlation(train$waterfront,train$price),
             correlation(train$newConstruction,train$price),
             correlation(train$centralAir,train$price))
cor_df<-data.frame(feature,cor_value)
cor_df$selected<-ifelse(abs(cor_df$cor_value)>=0.3,"Yes","No")
cor_df
x<-data.frame(train$landValue,train$livingArea,train$bedrooms,train$fireplaces,
              train$bathrooms,train$rooms,train$centralAir)
y<-data.frame(train$price)
head(x)
head(y)


