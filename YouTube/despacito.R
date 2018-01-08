library(XML)
library(lubridate)
library(dplyr)

# Cleaning ----
despacito_table = readHTMLTable("despacito_table.html")
despacito_df = despacito_table[[1]]
despacito_df$date = strptime(as.character(despacito_df$Date), "%b %d, %Y") %>% as.Date
despacito_df$views = gsub(",", "", despacito_df$value) %>% as.numeric
despacito_df$cumviews = cumsum(despacito_df$views)

# Add year, month, day of week

despacito_df$year = year(despacito_df$date)
despacito_df$month = month(despacito_df$date, label = TRUE)
despacito_df$wday = factor(weekdays(despacito_df$date),
                           levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Special days ----
# Fridays
fridays = subset(despacito_df, wday == "Friday")

# Holidays
holidays_str = list(valentines = "Feb 14, 2017",
                    shrove.tue = "Feb 28, 2017",
                    ash.wed = "Mar 1, 2017",
                    good.fri = "Apr 14, 2017",
                    easter = "Apr 16, 2017",
                    mem.day = "May 29, 2017",
                    july.4 = "Jul 4, 2017",
                    labor.day = "Sep 4, 2017",
                    thanksgiving = "Nov 23, 2017",
                    xmas = "Dec 25, 2017",
                    nye = "Dec 31, 2017")
holidays = lapply(holidays_str, function(x) which(despacito_df$Date == x))

# View milestones
milestones = list(t_100M = which(despacito_df$cumviews >= 1e8)[1],
                  t_1B = which(despacito_df$cumviews >= 1e9)[1],
                  t_2B = which(despacito_df$cumviews >= 2e9)[1],
                  t_3B = which(despacito_df$cumviews >= 3e9)[1],
                  t_4B = which(despacito_df$cumviews >= 4e9)[1])

# Other events
beiber = which(despacito_df$Date == "Apr 16, 2017")
latin.grammys = which(despacito_df$Date == "Nov 16, 2017")

# Plots ----

# Views per day
plot(despacito_df$date, despacito_df$views, type = "l")
abline(v = despacito_df$date[unlist(milestones)])

# Cumulative views
plot(despacito_df$date, cumsum(despacito_df$views),
     type = "l",
     ylim = c(0, 5e9),
     frame.plot = FALSE)
abline(h = seq(1e9, 4e9, 1e9), col = "gray")
abline(v = despacito_df$date[unlist(milestones)])

# Average views per weekday
avg_views_per_day  =  despacito_df %>%
    group_by(wday) %>%
    summarize(avgviews = mean(views))
barplot(height = avg_views_per_day$avgviews,
        names.arg = avg_views_per_day$wday)

# Average views per month
avg_views_per_month  =  despacito_df %>%
    group_by(year, month) %>%
    summarize(avgviews = mean(views))
barplot(height = avg_views_per_month$avgviews,
        names.arg = avg_views_per_month$month)

# Average view per weekday per month (joyplot)

# Total views per month
sum_views_per_month  =  despacito_df %>%
    group_by(year, month) %>%
    summarize(sumviews = sum(views))
barplot(height = sum_views_per_month$sumviews,
        names.arg = sum_views_per_month$month)

