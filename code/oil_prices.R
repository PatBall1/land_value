### Look at variation in oil price
library(dplyr)
library(readxl)
library(lubridate)
library(blscrapeR)


data <- read_excel("./data/SerieCifrasPetroleras.xlsx", range="C5:FB138", 
                   col_names=FALSE, col_types="numeric")
data1 <- as_tibble(t(data))
variables <- read_excel("./data/SerieCifrasPetroleras.xlsx", range="B5:B138", col_names = FALSE)
variables <- as_tibble(t(variables))

names(data1) <- variables

dates.start <- seq(ymd('2007-01-01'),ymd('2019-12-01'),by='months')
dates.end <- dates.start + months(1) - days(1)
dates <- bind_cols(start.date=dates.start, end.date=dates.end)

dates <- mutate(dates, interval=interval(start.date,end.date))
#dates <- mutate(dates, month=paste0(month(start.date, label = TRUE),"-",year(start.date)))

data <- bind_cols(dates, data1)
rm(dates, data1, dates.end, dates.start, variables)


# Correct for inflation


# Function to adjust prices for inflation
infl <- inflation_adjust(2016)
adjust <- function(date){
  infl <- infl
  year.n <- year(date)
  adj <-
    infl %>%
    filter(year==year.n) %>%
    select(adj_value)
  return(as.numeric(adj))
}


#price.corrected <- `TOTAL NACIONAL`*adjust(infl, as_date(end.date))

data <- 
  data %>%
  mutate(price.corrected=sapply(end.date, adjust)*`Precio Crudo Oriente (USD por barril)`) #%>%


#data %>%
#  filter(year(end)==2016) %>%
#  summarise(avg=mean())


plot(data$end.date, data$price.corrected,
     type='l')                
plot(data$end.date, data$`Precio Crudo Oriente (USD por barril)`, type='l', xlab="Year", ylab="Price USD 2016 corrected")

p1 <- mean(data$price.corrected)
p2 <- mean(data$price.corrected) + sd(data$price.corrected)
p3 <- 34*0.9*adjust("2020-03-16")
mean(data$`Precio Crudo Oriente (USD por barril)`) + sd(data$`Precio Crudo Oriente (USD por barril)`)

rm(infl) 
