# load libraries
library(dplyr)
library(ggplot2)
library(reshape2)

#import data
home <- '/Users/xing'
project <- paste0(home, '/Desktop/Budget_R/Data')

dat1 <- read.csv(paste0(project, '/budget_2016_2017.csv'))

#remove white spaces
trim <- function(x) gsub("^\\s+|\\s+$", "", x)
dat1$description <- trim(dat1$description)
dat1$details <- trim(dat1$details)

# change description categories and names
dat1$description <- ifelse(dat1$description == "travel", "vacation", dat1$description)
dat1$description <- ifelse(dat1$description == "tea", "snacks", dat1$description)
dat1$description <- ifelse(dat1$description == "medications", "medicine", dat1$description)
dat1$description <- ifelse(dat1$description == "vacation", "travel", dat1$description)

dat1$description <- as.factor(dat1$description)
dat1$details <- as.factor(dat1$details)
dat1$date <- as.Date(dat1$date, format = '%m/%d')


# see how much we've spent on each category so far
total_expenditure <- dat1 %>% 
  group_by(description) %>%
  summarise(total_exp = sum(expenditure))

ggplot(total_expenditure, aes(description, total_exp, group = description)) +
        geom_bar(stat = "identity") + 
        xlab("Description") + ylab("Expenditure") +
        ggtitle("1st Month expenditure")
  
# import budget 2015/2016 data
dat2 <- read.csv(paste0(project, "/budget1.csv"))

#trim white space
dat2$person <- trim(dat2$person)
dat2$description <- trim(dat2$description)
dat2$details <- trim(dat2$details)

#change snack into snacks for consistency
dat2$description <- ifelse(dat2$description == "snack", "snacks", dat2$description)

dat2$person <- as.factor(dat2$person)
dat2$description <- as.factor(dat2$description)
dat2$details <- as.factor(dat2$details)
dat2$date <- as.Date(dat2$date, format = '%m/%d')

#remove year from date to compare first month
dat1$date <-format(dat1$date, format="%m-%d")
dat2$date <-format(dat2$date, format="%m-%d")

# group by date to show spending per day
nov1 <- dat1 %>%
  group_by(date) %>%
  summarise(daily_exp = sum(expenditure))

nov2 <- dat2 %>%
  group_by(date) %>%
  summarise(daily_exp = sum(expenditure))

# make dataframe for nov 2015 and 2016 expenditure
nov1516 <- inner_join(nov1, nov2, by="date")
names(nov1516) <- c("date", "nov2016", "nov2015")

#make column with cumulative sum
nov1516$cumsum2016 <- cumsum(nov1516$nov2016)
nov1516$cumsum2015 <- cumsum(nov1516$nov2015)

#remove unecessary columns
nov1516$nov2016 <- NULL
nov1516$nov2015 <- NULL

#melt 2015 and 2016 cumsum into one column
melt <- melt(nov1516, "date")

# plot this 
ggplot(melt, aes(x=date, y=value, group=variable, colour=variable)) +
  geom_line(size=1) +
  xlab("Date") + ylab("Cumulative Expenditure") +
  ggtitle("Nov 2015 vs Nov 2016 Expenditure") +
  scale_colour_discrete(name = "Year", 
                       breaks=c("cumsum2016", "cumsum2015"), 
                       labels=c("2016", "2015"))

# compare nov 2015 and 2016 spending by category

#group by description
nov_cat1 <- dat1 %>%
  group_by(description) %>%
  summarise(exp_description = sum(expenditure))

# subset for 2015 dates until dec 2
dat2short <- dat2[1:118,]

nov_cat2 <- dat2short %>%
  group_by(description) %>%
  summarise(exp_description = sum(expenditure))


by_cat <- inner_join(nov_cat1, nov_cat2, by="description")
melt2 <- melt(by_cat, "description")
melt2$description <- as.factor(melt2$description)

# plot by description
ggplot(melt2, aes(x=description, y=value, group = variable, fill = variable)) +
  geom_bar(stat="identity", position = "dodge") +
  xlab("Description") +
  ylab("Spending") +
  ggtitle("Spending by Category Nov 2015 vs Nov 2016") +
  scale_fill_discrete(name = "Year",
                        breaks = c("exp_description.x", "exp_description.y"),
                        labels = c("2016", "2015"))

