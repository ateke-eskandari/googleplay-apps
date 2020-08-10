df<-read.csv("D:/ateke/datas excel/googleplayapps-c.csv",header = TRUE,stringsAsFactors = F)
summary(df)
str(df)
df$Category=factor(df$Category)
levels(df$Category)=seq(33)
unique(df$Type)
df$Type[df$Type==""]=NA
df$Type=factor(df$Type)
levels(df$Type)
summary(df)
class(df$Last.Updated)
df$Last.Updated<-as.Date(df$Last.Updated,format = "%m/%d/%Y")
Updates=Sys.Date()-df$Last.Updated
df$Updates=Updates
df$Updates<-as.numeric(df$Updates)
str(df)
head(df$Installs,5)
spinst=strsplit(df$Installs[1],"")
spinst=unlist(spinst)
spinst=spinst[-length(spinst)]
spinst=spinst[spinst!=","]
paste(spinst,collapse="")
F=function(x){a=paste(unlist(strsplit(x,""))[-length(unlist(strsplit(x,"")))][unlist(strsplit(x,""))[-length(unlist(strsplit(x,"")))]!=","],collapse="");return(a)}
df$Installs=as.matrix(df$Installs)
df$Installs=apply(df$Installs,1,F)
df$Installs=as.numeric(df$Installs)
df=df[c('Category','Rating','Reviews','Type','Updates')]
library(mice)
library(VIM)
imput=mice(df,m=5,method=c('','rf','pmm','logreg',''))
aggr<- aggr(df, col=c('black','red'), numbers=TRUE, sortVars=TRUE,
            labels=names(df), cex.axis=.7, gap=1, ylab=c("Barplot of missing data","Patterns"))
marginplot(df[,c(1,2)])
marginplot(df[,c(1,3)])
marginplot(df[,c(1,4)])
marginplot(df[,c(1,5)])
marginplot(df[,c(2,3)])
marginplot(df[,c(2,4)])
marginplot(df[,c(2,5)])
marginplot(df[,c(3,4)])
marginplot(df[,c(3,5)])
marginplot(df[,c(5,4)])
xyplot(imput, Reviews ~ Type+Rating| .imp)
densityplot(imput)
com=complete(imput,1)
summary(com)

