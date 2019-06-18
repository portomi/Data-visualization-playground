library(rvest)
library(tidyverse)
library(magrittr)
library(scales)
library(knitr)
library(lubridate)
library(eeptools)
library(zoo)



cars <- read.csv('Jautkhu2.csv', na.strings = c("", "NA"))
cars <- cars[c(4, 6)]
cars <- cars %>% na.omit()
cars$Field5 <- sapply(cars$Field5, function(x) gsub("[\\skm, Â, [:space:]]", "", x), simplify = TRUE)
cars$Time <- sapply(cars$Time, function(x) gsub("[\\s.[:space:], .]", "", x), simplify = FALSE)

#write.csv(cars, file="cars.CSV")

cars <- cars %>% mutate(age = age_calc(as.Date(paste0(cars$Time, '01'), format='%Y%m%d'), units = "years"))

cars[, 3] <- round(cars[, 3], digits = 1)

cars <- cars %>% group_by(age) %>% summarise(km = median(as.numeric(Field5), na.rm = TRUE))

new_Cars <- filter(cars, age <= 3)
old_cars <- filter(cars, age > 3 & age < 16)

cars <- mutate(cars, rm = rollmean(cars$km, 25, na.pad=TRUE, align = 'right'))
plot <- ggplot(cars)

plot + geom_col(aes(x=cars$age, y=km), color = "Red") + geom_line(aes(x=cars$age, y=rm)) + geom_line(aes(x=cars$age, y=100000), linetype=2, color='Black') + geom_abline(intercept = -8180, slope = 24946, linetype=4, color='Blue')+ geom_abline(intercept = 93590, slope = 6987, linetype=5, color='Blue')+ annotate('text', x=1, y=110000, label='100000 km', color='Black', size=3) + scale_x_continuous(breaks = c(1, 5, 10, 15, 20), labels = c(1, 5, 10, 15, 20), limits=c(0.1, 20)) + scale_y_continuous(breaks = c(100000, 150000, 200000, 250000, 300000, 350000), labels = comma)+ labs(title = "Használt autók átlagos futásteljesítménye Magyarországon", x = "cars kora (év)", y = "bevallott km")
