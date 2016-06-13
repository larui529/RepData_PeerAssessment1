unzip("activity.zip")
df <- read.csv("activity.csv")
stepMean <- mean(df$steps, na.rm = TRUE)
print(stepMean)