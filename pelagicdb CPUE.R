data <- read.csv("pelagicdb cpue.csv")

data=subset(data, cpue != "NULL" | scpue != "NULL")



str(data)

data$cpue <- as.numeric(data$cpue)
data$scpue <- as.numeric(data$scpue)

hist(data$cpue)
hist(data$scpue)

par(mfrow=c(2,1))

table <- aggregate()
