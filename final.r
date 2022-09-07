csvFile <- file.path("D:/Study/R_Language/final_tset/Bing-COVID19-Data.csv") #取得檔案
data <- read.csv(csvFile, sep=",") #讀取檔案內容，用逗號分隔欄位

head(data,5)

summary(data)

library(Hmisc) #使用 Hmisc library
describe(data) #使用 Hmisc 的內建函數來查看 data summary

cor.all <- by(data[,c(3, 5, 7)], INDICES=data$ISO2,cor)
print(cor.all)

norm <- function()
{
	par(mfrow=c(1,2)) # 圖片呈現一列兩欄的排列
    # Confirmed
    qqnorm(data$Confirmed, main="Confirmed") #常態機率圖
    qqline(data$Confirmed,col="Red") #畫出最佳斜線
    print(shapiro.test(data$Confirmed[0:500])) #shapiro-wilk 檢定

    # ConfirmedChange
    qqnorm(data$ConfirmedChange ,main="ConfirmedChange")
    qqline(data$ConfirmedChange ,col="Blue")
    print(shapiro.test(data$ConfirmedChange[0:500]))
    
    # Deaths
    qqnorm(data$Deaths ,main="Deaths")
    qqline(data$Deaths ,col="Blue")
    print(shapiro.test(data$Deaths[0:500]))
    
    # DeathsChange
    qqnorm(data$DeathsChange ,main="DeathsChange")
    qqline(data$DeathsChange ,col="Blue")
    print(shapiro.test(data$DeathsChange[0:500]))
    
    # Recovered
    qqnorm(data$Recovered ,main="Recovered")
    qqline(data$Recovered ,col="Blue")
    print(shapiro.test(data$Recovered[0:500]))
    
    # RecoveredChange
    qqnorm(data$RecoveredChange ,main="RecoveredChange")
    qqline(data$RecoveredChange ,col="Blue")
    print(shapiro.test(data$RecoveredChange[0:500]))
}
norm()

# 使用 ggplot2 套件
library(ggplot2)

# 取出資料中的 死亡數(行) 與 確診數(行)
death_column = file$Deaths
confirm_column = file$Confirmed

# 繪製分佈圖
ggplot(file, aes(x = confirm_column, y = time_column)) + geom_point(shape = 10, size = 5) + geom_smooth(formula = y ~ x, method = "lm") + labs(x = "Confirmed", y = "Time")

LM <- lm(death_column~confirm_column, data=file)
summary(LM)

# 決策樹
require(rpart)
set.seed(3)

# set 1 to "Yes", 0 to "No"
data$y = "Yes"
for(i in c(1:nrow(data))){
    if(data[i,]$whether == 1)
        data[i,]$y = "Yes"
    else
        data[i,]$y = "No" 
}
data <- data[,-5]

# train = 80%, test = 20%
train.index = sample(x = 1:nrow(data), size = ceiling(0.8 * nrow(data)))
train = data[train.index,]
test = data[-train.index,]

cart.model<- rpart(whether2007~. , data=train)
#cart.model<- rpart(y~. , data=train)

require(rpart.plot)
prp(cart.model,
    faclen=0,
    fallen.leaves=TRUE,
    shadow.col="gray",
    extra=1)

pred <- predict(cart.model, newdata=test, type = "class")
table(real=test$y, predict=pred)

confus.matrix <- table(real=test$Survived, predict=pred)
sum(diag(confus.matrix))/sum(confus.matrix)

# 群集
# 將資料換回原先的版本
csvFile <- file.path("./transfusion.data")
data <- read.csv(csvFile, sep=",")

E.dist <- dist(data, method = "euclidean")
M.dist <- dist(data, method = "manhattan")

par(mfrow=c(1,2))
h.E.cluster <- hclust(E.dist)
plot(h.E.cluster, xlab="euclidean")

h.M.cluster <- hclust(M.dist) 
plot(h.M.cluster, xlab="manhattan")

E.dist <- dist(data[1:50,], method = "euclidean")
M.dist <- dist(data[1:50,], method = "manhattan")
par(mfrow=c(1,2))
h.E.cluster <- hclust(E.dist)
plot(h.E.cluster, xlab="euclidean")
h.M.cluster <- hclust(M.dist) 
plot(h.M.cluster, xlab="manhattan")

h.cluster <- hclust(E.dist, method="ward.D2")
plot(h.cluster)
abline(h=15000, col="red")

kmeans.cluster <- kmeans(data, centers=3)
kmeans.cluster$withinss

#install.packages("factoextra")
require(factoextra)

fviz_cluster(kmeans.cluster,
            data = data,
            geom = c("point", "text"),
            frame.type = "norm")

fviz_nbclust(data, 
            FUNcluster = hcut,
            method = "wss",
            k.max = 12
) + 

labs(title="Elbow Method for HC") +

geom_vline(xintercept = 3,
            linetype = 2)