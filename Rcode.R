data <- read.csv("./activity.csv", colClasses = c("numeric","Date","numeric"))

# Histogram of mean total number of steps per day
stepsxday <- aggregate( steps ~ date, data = data, 
                        FUN = "sum", na.action = na.omit )
hist(stepsxday$steps, xlab = "Number of Steps", breaks = 10, 
     col="blue", main = "Total number of steps taken each day")
steps_mean <- mean(stepsxday$steps)
steps_median <- median(stepsxday$steps)

# Average daily activity pattern
library(lattice)
nintervals <- 24 * 60 / 5
ndays <- nrow(data) / nintervals
stepsxinterval <- aggregate( steps ~ interval, data = data, FUN = "sum" )
names(stepsxinterval) <- c("interval","mean_steps")
stepsxinterval$mean_steps <- stepsxinterval$mean_steps / ndays

ylabspec <- list(label="Average number of steps", cex=1.5)
xlabspec <- list(label="Interval", cex=1.5)
mspec <- list(label="Average Daily Activity Pattern", cex=1.5)
xyplot(mean_steps ~ interval, data = stepsxinterval, type="l", 
       xlab = xlabspec, ylab = ylabspec, col = "black", 
       main = mspec, scales = list(cex=1.25))

max_interval <- stepsxinterval$interval[which.max(stepsxinterval$mean_steps)]

# Inputing missing values with mean of 5-minute interval 
idx <- is.na(data$steps)
n_missval <- sum(idx)

data2 <- data
for (i in 1:length(idx)) {
  if (idx[i] == TRUE) {
    intval <- data2$interval[i]
    data2$steps[i] <- stepsxinterval$mean_steps[
                          which(stepsxinterval$interval == intval)]
  }
}
stepsxday2 <- aggregate( steps ~ date, data = data2, FUN = "sum")
hist(stepsxday2$steps, xlab = "Number of Steps", breaks = 10, 
     col="green", main = "Total number of steps taken each day")
steps_mean2 <- mean(stepsxday2$steps)
steps_median2 <- median(stepsxday2$steps)

# Difference in pattern between weekdays and weekends
data2 <- cbind(data2, 
               factor(as.numeric(weekdays(data2$date) %in% c("Saturday","Sunday")),
                        labels = c("weekday","weekend")))
names(data2)[4] <- "daytype"
n_weekdays <- sum(data2$daytype == "weekday") / nintervals
n_weekends <- sum(data2$daytype == "weekend") / nintervals
steps_weend <- aggregate( steps ~ interval + daytype, data = data2, FUN = "sum" )

idx <- steps_weend$daytype == "weekday"
steps_weend$steps[idx] <- steps_weend$steps[idx] / n_weekdays
idx <- !idx
steps_weend$steps[idx] <- steps_weend$steps[idx] / n_weekends

library(ggplot2)
g <- qplot(x = interval, y = steps, data = steps_weend, facets = daytype ~ .)
g + theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18), 
          strip.text = element_text(size = 18)) + geom_line() 
