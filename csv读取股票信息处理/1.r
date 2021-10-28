
# 在这将工作区设置为stock.csv 的当前目录 必须
setwd("D:/Users/20109444/Desktop")
# read the data from stock.csv
stock <- read.csv("stock.csv", encoding = "UTF-8")
# 7月数据
serven.data <- subset(stock, substring(date, 6, 6) == "7")
x <- serven.data$colsing.price
y <- c(23:1)
# 线性关系
relation <- lm(x ~ y)
# 预测8月1号的值
print(predict(relation, data.frame(y = 24)))
# 每个月份的中位数画图
# 月份数据
date.data <- as.Date(stock$date, "%Y/%m/%d")
sort(date.data)
# 月份字符串
date.data.string <- format(date.data, "%Y-%m")
x2 <- c()
temp <- "x"
#  将重复的月份去重
for (i in date.data.string) {
    if (temp == i) {
        next
    } else {
        temp <- i
        x2 <- c(x2, i)
    }
}
y2 <- c()
for (i in x2) {
    # 计算当前月份的中位数
    y2 <- c(y2, median(subset(stock$minimum.price, format(as.Date(stock$date, "%Y/%m/%d"), "%Y-%m") == i))) # nolint
}

# 画图
plot(1:12, y2, type = "l")
axis(side = 1, at = 1:length(x2), labels = x2)

# 连续三天为高开日
#选出所有是高开日的数据
high.open <- subset(stock, stock$previous.closing.price < stock$opening.price)
#提取日期
high.open.date <- as.Date(high.open$date, "%Y/%m/%d")

count <- 3
result <- c()
# 满足高开日的数量
length <- length(high.open.date)

while (count < length) {
    count <- count + 1
    #判断是否是连续的三天
    if ((high.open.date[count - 1] - high.open.date[count] == 1) &&
        (high.open.date[count - 2] - high.open.date[count] == 2)) {
        q <- c(count - 2, count - 1, count)
        #将值放到结果集合中去
        result <- c(result, format(high.open.date[q]))
    }
}

print(result)