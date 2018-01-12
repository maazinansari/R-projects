# Save plots as png in content/static/despacito
# with other images
source("RMD/despacito/despacito.R")
output = "static/despacito/"
if (!dir.exists(output)) dir.create(output)
# Views per day
png(paste0(output, "views-per-day.png"), width = 720)
plot_vpd()
mark_days(milestones, labels = names(milestones))
dev.off()

png(paste0(output, "vpd-holiday.png"), width = 720)
plot_vpd()
mark_days(holidays, labels = seq_along(holidays), col = "darkorange")
legend("topright", legend = paste(seq_along(holidays), " ", names(holidays)), bty = "n")
dev.off()

png(paste0(output, "vpd-saturday.png"), width = 720)
plot_vpd()
mark_days(saturdays, labels = "", type = "p")
dev.off()

png(paste0(output, "vpd-bieber.png"), width = 720)
plot_vpd()
mark_days(beiber, labels = "", type = "p", col = "purple")
dev.off()

png(paste0(output, "change-vpd.png"), width = 720)
plot_change()
dev.off()

# Cumulative views
png(paste0(output, "total.png"), width = 720)
plot_cvpd()
abline(h = seq(0, 5e9, 1e9), lty = 3, col = "lightgray")
mark_days(milestones, column = "cumviews", labels = "")
dev.off()

# Average views per weekday
avg_views_per_day  =  despacito_df %>%
    group_by(wday) %>%
    summarize(avgviews = mean(views))
png(paste0(output, "avg-wkday.png"), width = 720)
boxplot(views ~ wday,
        data = despacito_df,
        main = "Views per day of week",
        ylim = c(0, 30e6),
        yaxt = "n",
        col = wday_col,
        frame.plot = FALSE)
axis(side = 2, at = seq(0, 30e6, 5e6), labels = paste(seq(0, 30, 5), "M", sep = ""), las = 1)
points(avg_views_per_day$avgviews, col="white", pch = 3)
dev.off()

# Average views per month
avg_views_per_month  =  despacito_df %>%
    group_by(year, month) %>%
    summarize(avgviews = mean(views))
png(paste0(output, "avg-month.png"), width = 720)
barplot(height = avg_views_per_month$avgviews,
        names.arg = avg_views_per_month$month,
        main = "Average views per month",
        ylim = c(0, 25e6),
        yaxt = "n")
axis(side = 2, at = seq(0, 25e6, 5e6), labels = paste(seq(0, 25, 5), "M", sep = ""), las = 1)
dev.off()

# Total views per month
sum_views_per_month  =  despacito_df %>%
    group_by(year, month) %>%
    summarize(sumviews = sum(views))
png(paste0(output, "total-month.png"), width = 720)
barplot(height = sum_views_per_month$sumviews,
        names.arg = sum_views_per_month$month,
        main = "Total views per month",
        ylim = c(0, 7e8),
        yaxt = "n")
axis(side = 2, at = seq(0, 7e8, 1e8), labels = paste(seq(0, 70, 10), "M", sep = ""), las = 1)
dev.off()
