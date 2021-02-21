setwd("~/Documents/UOW/INFO411 Knowledge Discov/Labs/Project/FiveCitiePMData")

pacman::p_load(pacman, caret, dplyr, e1071, ggplot2, keras, lubridate, mlbench, party, rpart)

data <- read.csv("BeijingPM20100101_20151231.csv")

# Deleting columns, changing columns to proper classes
beijing.data <- data
beijing.data$PM_Dongsi <- NULL
beijing.data$PM_Nongzhanguan <- NULL
beijing.data$PM_US.Post <- NULL
beijing.data$No <- NULL
beijing.data$season <- as.factor(beijing.data$season)
beijing.data$DEWP <- as.numeric(beijing.data$DEWP)
beijing.data$PM_Dongsihuan <- as.numeric(beijing.data$PM_Dongsihuan)

str(beijing.data)
summary(beijing.data)

# Print NA count in each column
for (i in colnames(beijing.data)) {
  cat(i, sum(is.na(beijing.data[[i]])), "\n")
}
print(sum(is.na(beijing.data)))


# Split data into complete and containing-NAs sets
beijing <- beijing.data[complete.cases(beijing.data), ]
beijing.data <- beijing.data[!complete.cases(beijing.data), ]
summary(beijing)
str(beijing)


# Plot distribution
for (i in 6:14) {
  if(is.factor(beijing[, i])) {
    print(ggplot(beijing, aes(beijing[, i])) +
          geom_bar(stat = "count") +
          xlab(colnames(beijing)[i]))
  } else {
    print(ggplot(beijing, aes(beijing[, i])) +
          geom_histogram(binwidth = 2) +
          xlab(colnames(beijing)[i]))
  } 
}

# Correlations between pm2.5 and other variables
for (i in 6:14) {
  if(!is.factor(beijing[, i])) {
      print(paste("pm2.5 & ", colnames(beijing)[i], sep = ""))
      print(cor(beijing$PM_Dongsihuan, beijing[, i], use = "complete.obs"))
      print(ggplot(beijing, aes(beijing[, i], PM_Dongsihuan)) +
            geom_point() +
            xlab(colnames(beijing)[i]))
  } else {
      print(ggplot(beijing, aes(beijing[, i], PM_Dongsihuan)) +
            geom_boxplot() +
            xlab(colnames(beijing)[i]))
  }
}

for(i in 6:14) {
  for(j in (i+1):14) {
    if(j <= 14 & j > i & !is.factor(beijing[, i]) & !is.factor(beijing[, j])) {
      print(paste(colnames(beijing)[i], "&", colnames(beijing)[j], sep = " "))
      print(cor(beijing[, i], beijing[, j]))
      print(ggplot(beijing, aes(beijing[, i], beijing[, j])) + 
              geom_point() +
              xlab(colnames(beijing)[i]) +
              ylab(colnames(beijing)[j]))
    }
  }
}

for(i in 6:14) {
  if(!is.factor(beijing[, i])) {
    print(ggplot(beijing, aes(cbwd, beijing[, i])) +
            geom_boxplot() +
            xlab("cbwd") +
            ylab(colnames(beijing)[i]))
  }
}

# Plot time series
ggplot(beijing, aes(datetime, PM_Dongsihuan)) +
  geom_line() + 
  scale_x_datetime(date_breaks = "6 months", limits = c(as.POSIXct("2013-03-05"), as.POSIXct("2015-12-31"))) +
  xlab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### PLEASE run AFTER plotting
beijing <- beijing %>%
  mutate(date = make_date(year, month, day)) %>%
  # mutate(datetime = make_datetime(year, month, day, hour)) %>%
  arrange(date)
str(beijing)

beijing$year <- NULL
beijing$month <- NULL
beijing$day <- NULL
# beijing.data$hour <- NULL

# Create lists with weekend days and working hours
weekdays1 <- c('Saturday', 'Sunday')
workhours1 <- c(8:17)
beijing$wEnd <- as.factor(c('weekend', 'weekday')[(!weekdays(beijing$date) %in% weekdays1)+1L])
beijing$WH <- as.factor(c('not', 'workhour')[((beijing$hour) %in% workhours1)+1L])
str(beijing)
### Can run the code below this


# These two plot correlation between week days and working hours with pm2.5
if(!is.factor(beijing[, 13])) {
  print(paste("pm2.5 & ", colnames(beijing)[13], sep = ""))
  print(cor(beijing$PM_Dongsihuan, beijing[, 13], use = "complete.obs"))
  print(ggplot(beijing, aes(beijing[, 13], PM_Dongsihuan)) +
          geom_point() +
          xlab(colnames(beijing)[13]))
  } else {
    print(ggplot(beijing, aes(beijing[, 13], PM_Dongsihuan)) +
          geom_boxplot() +
          xlab(colnames(beijing)[13]))
  }

if(!is.factor(beijing[, 14])) {
  print(paste("pm2.5 & ", colnames(beijing)[14], sep = ""))
  print(cor(beijing$PM_Dongsihuan, beijing[, 14], use = "complete.obs"))
  print(ggplot(beijing, aes(beijing[, 14], PM_Dongsihuan)) +
          geom_point() +
          xlab(colnames(beijing)[14]))
} else {
  print(ggplot(beijing, aes(beijing[, 14], PM_Dongsihuan)) +
          geom_boxplot() +
          xlab(colnames(beijing)[14]))
}

# subset data weekday-weekend, workhours-not
beijing.weekend <- subset(beijing, beijing[,13] == 'weekend')
beijing.weekday <- subset(beijing, beijing[,13] == 'weekday')
beijing.wh <- subset(beijing, beijing[,14] == 'workhour')
beijing.not <- subset(beijing, beijing[,14] == 'not')

print(ggplot(beijing.weekend, aes(beijing.weekend[, 3])) +
      geom_bar(stat = "density") +
      xlab(colnames(beijing.weekend)[3]) +
      ggtitle("Weekend"))

print(ggplot(beijing.weekday, aes(beijing.weekday[, 3])) +
        geom_bar(stat = "density") +
        xlab(colnames(beijing.weekday)[3]) +
        ggtitle("Weekday"))

print(ggplot(beijing.wh, aes(beijing.wh[, 3])) +
        geom_bar(stat = "count") +
        xlab(colnames(beijing.wh)[3]) +
        ggtitle("Working Hours"))

print(ggplot(beijing.not, aes(beijing.not[, 3])) +
        geom_bar(stat = "count") +
        xlab(colnames(beijing.not)[3]) +
        ggtitle("Not Working Hours"))


# Train-test split. Assign all factors numerical values
set.seed(345)
smp.size <- floor(0.70 * nrow(beijing))
train_ind <- sample(seq_len(nrow(beijing)), size = smp.size)

beijing.train <- beijing[train_ind, ]
beijing.train$date <- NULL
for(i in 1:13) {
  if(is.factor(beijing.train[[i]])) {
    beijing.train[[i]] <- as.numeric(as.factor(beijing.train[[i]]))
  }
}
beijing.test <- beijing[-train_ind, ]
beijing.test$date <- NULL
for(i in 1:13) {
  if(is.factor(beijing.test[[i]])) {
    beijing.test[[i]] <- as.numeric(as.factor(beijing.test[[i]]))
  }
}

# Split features and labels
train_data <- beijing.train[, c(-3)]
train_labels <- beijing.train[, 3]
test_data <- beijing.test[, c(-3)]
test_labels <- beijing.test[, 3]

# Normalize data
train_data <- scale(train_data)
col_means_train <- attr(train_data, "scaled:center") 
col_stddevs_train <- attr(train_data, "scaled:scale")
test_data <- scale(test_data, center = col_means_train, scale = col_stddevs_train)

# Let's create the model
model <- keras_model_sequential()
model %>% layer_dense(units = 36, activation = 'relu', input_shape = dim(train_data)[2], 
                      kernel_regularizer = regularizer_l1(0.02)) %>% 
          layer_dropout(rate = 0.2) %>%
          layer_dense(units = 72, activation = 'relu', 
                      kernel_regularizer = regularizer_l1(0.02)) %>% 
          layer_dropout(rate = 0.2) %>%
          layer_dense(units = 24, activation = 'relu', 
                      kernel_regularizer = regularizer_l1(0.02)) %>% 
          layer_dropout(rate = 0.2) %>%
          layer_dense(units = 1)

summary(model)
# You can add more hidden layers

model %>% compile(loss = 'mse',
                  optimizer = optimizer_rmsprop(),
                  metrics = list("mean_absolute_error"))

print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    

epochs <- 300

history <- model %>% fit(train_data, train_labels,
                         epoch = epochs, batch_size = 32,
                         validation_split = 0.2, verbose = 0,
                         callbacks = print_dot_callback)

plot(history)

test_predictions <- model %>% predict(test_data)
test_predictions[ , 1]

summary(beijing)


# Standard Linear Model
beijing.train$PM_Dongsihuan <- log(beijing.train$PM_Dongsihuan)
beijing.test$PM_Dongsihuan <- log(beijing.test$PM_Dongsihuan)
history2 <- lm(PM_Dongsihuan ~ hour+HUMI+PRES+TEMP+cbwd+Iws+Iprec+wEnd+WH, data = beijing.train)
summary(history2)
plot(history2)

RSS <- c(crossprod(history2$residuals))
MSE <- RSS / length(history2$residuals)
RMSE <- sqrt(MSE)

test_predictions2 <- predict(history2, beijing.test)
test_predictions2 <- 2.718282^(test_predictions2)
test_predictions2
summary(test_predictions2)


# CLEAN UP #################################################

# Clear environment
rm(list = ls())

# Clear packages
p_unload(all)  # Remove all add-ons
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)