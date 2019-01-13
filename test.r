library(ggplot2)
library(zoo)  # for rollmean
library(reshape2)  # for melt
library(plyr)  # for ddply
library(scales)

Sys.setlocale(category = "LC_TIME", locale = "en_US")  # For Date conversions

df <- read.csv(file="raw_data.csv", header = TRUE, na.strings = c("---", "-", "--"))

# Remove rows with NAs in the CLOSED or OPENED columns
df <- subset(df, !(is.na(CLOSED) | is.na(OPENED)) )

# WINTER (string -> int)
year.from.winter <- function (x) {
  stop <- regexpr("-", x, fixed = TRUE)[1] -1
  value <- as.integer(substr(x, 1, stop))
  return(value)
}
df$WINTER <- apply(df["WINTER"], 1, year.from.winter)

# CLOSED | OPENED (string -> Date)
convert.date <- function (start.year, day.closed, day.opened) {
  # Parse the dates using locale and the following date format
  format <- "%Y-%e-%b"  # eg. 1855-18-Dec
  parse <- function (year, s) {  # Convert the string to a date on given year
    date.string <- paste0(year, "-", s)
    return (as.Date.character(date.string, format))
  }
  winter.start <- as.Date.character(paste0(start.year, "-07-01"))
  
  # The closing date should be during the winter of said year, which stretches to the beginning of the following year
  closed <- parse(start.year, day.closed)
  if (closed < winter.start) {
    closed <- parse(start.year +1, day.closed)
  }
  
  # The opening date should be after the closing one
  opened <- parse(start.year, day.opened)
  if (opened < closed) {
    opened <- parse(start.year +1, day.opened)
  }
  
  return (data.frame(CLOSED = closed, OPENED = opened))
}
df <- mdply(df, function (WINTER, CLOSED, OPENED, ...) { return (convert.date (WINTER, CLOSED, OPENED)) })
# Dunno why this doesn't create a df with 2 columns instead of 4…

# Compute the number of days
df$DAYS <- as.integer(df$OPENED - df$CLOSED)


## DATA FOR PLOT #0

# Sum days for each year
sum.on.days <- function (df) {
  days <- sum(df$DAYS)
  return (data.frame(WINTER = df$WINTER[1], DAYS = days))
}
#df1 <- do.call(rbind, by(df, df$WINTER, sum.on.days))
df1 <- ddply (df, .(WINTER), sum.on.days)

# Data for the first plot
df2 <- data.frame (WINTER = df1$WINTER,
                   ROLLMEAN = rollmean (df1$DAYS, 5, fill = NA, align = "right"),
                   ROLLMEAN15 = rollmean (df1$DAYS, 15, fill = NA, align = "right"))
mdf <- melt(df2, "WINTER")

max.days <- max(df1$DAYS)  # For the scale


p <- ggplot() +
  geom_point (data = df1, aes (x = WINTER, y = DAYS)) +
  geom_line (data = mdf, aes (x = WINTER, y = value, colour = variable), na.rm = TRUE) +
  expand_limits (y = 0) +
  scale_colour_manual(values = c("ROLLMEAN" = "lightskyblue", "ROLLMEAN15" = "blue"),
                      name = "Average over…",
                      labels = c("ROLLMEAN" = "5 years", "ROLLMEAN15" = "15 years")) +
  scale_y_continuous (breaks = seq(0, max.days, by = 60),
                      minor_breaks = seq(0, max.days, by = 30)) +
  labs (x = "Winter", y = "Days of ice coverage",
        title = "Lake Mendota ice coverage between 1855 and 2017")

## DATA FOR PLOT #1

# Normalize all dates to Winter 2000
norm.winter.start <- as.Date.character("2000-07-01")
days.in.year <- 365.2425
same.year <- function (date) {
  years.diff <- ceiling(as.numeric(norm.winter.start - date) / days.in.year)
  return(date + years.diff * days.in.year)
}

df3 <- subset(df, select = -c(DAYS))
df3 <- mdply(df3, function (CLOSED, ...) { return (data.frame(CLOSED = same.year(CLOSED))) })
df3 <- mdply(df3, function (OPENED, ...) { return (data.frame(OPENED = same.year(OPENED))) })

df4 <- merge(df3, df1)


p1 <- ggplot() +
  geom_linerange(data = df4, aes (x = WINTER, ymin = CLOSED, ymax = OPENED, colour = DAYS)) +
  scale_colour_viridis_c(begin = 1, end = 0, option = "plasma",
                         name = "Days of\nice coverage",
                         breaks = seq(0, max.days, by = 30)) +
  labs(x = "Winter",
       title = "Lake Mendota ice coverage between 1855 and 2017")

# Now let's invert the time y axis
# cf. https://stackoverflow.com/questions/43625341/reverse-datetime-posixct-data-axis-in-ggplot
# I have no idea what it does though.
c_trans <- function(a, b, breaks = b$breaks, format = b$format) {
  a <- as.trans(a)
  b <- as.trans(b)
  
  name <- paste(a$name, b$name, sep = "-")
  
  trans <- function(x) a$trans(b$trans(x))
  inv <- function(x) b$inverse(a$inverse(x))
  
  trans_new(name, trans, inverse = inv, breaks = breaks, format=format)
  
}
rev_date <- c_trans("reverse", "date")
p1 <- p1 + scale_y_continuous(trans = rev_date)


## DATA FOR PLOT #2

df5 <- df

# Get extremities of opening/closing for each year
extreme.days <- function (df) {
  closed <- min(df$CLOSED)
  opened <- max(df$OPENED)
  return (data.frame(CLOSED = closed, OPENED = opened))
}
df5 <- ddply(df, .(WINTER), extreme.days)

df6 <- df5
df6 <- mdply(df6, function (CLOSED, ...) { return (data.frame(CLOSED = same.year(CLOSED))) })
df6 <- mdply(df6, function (OPENED, ...) { return (data.frame(OPENED = same.year(OPENED))) })
df6 <- melt(df6, "WINTER")

# Group by 50 years (and by variable)
df6$GROUP <- floor(df6$WINTER / 50)
name.group <- function (df) {
  df$GROUP <- rep (paste0 (df$variable[1], df$GROUP[1]), nrow(df))
  return (df)
}
df6 <- ddply(df6, .(variable, GROUP), name.group)
df6$GROUP <- factor(df6$GROUP)

# Identify outliers by ALPHA
alpha.outlier <- function (df) {
  default.alpha <- 0
  outliers <- boxplot.stats(as.integer(df$value))$out
  print(outliers)
  df$ALPHA <- ifelse (as.integer(df$value) %in% outliers, 1, default.alpha)
  return (df)
}
df6 <- ddply(df6, .(GROUP), alpha.outlier)

min.date <- min(df6$value)
middle.date <- (max(df6$value) - min.date) / 2 + min.date


p2 <- ggplot() +
  geom_vline(xintercept = 1855, colour = "palegreen") +
  geom_vline(xintercept = 2017, colour = "palegreen") +
  geom_boxplot (data = df6,
                aes (x = WINTER, y = value, group = GROUP, colour = variable),
                varwidth = TRUE,
                position = position_identity(),
                outlier.alpha = 0) +
  geom_point(data = df6,
             aes (x = WINTER, y = value, colour = variable, alpha = ALPHA)) +
  labs (x = "Years", y = "",
        title = "Freezing and thawing of lake Mendota between 1855 and 2017") +
  scale_colour_manual(values = c("CLOSED" = "#00BFC4", "OPENED" = "#F8766D"),
                      name = "Date the lake…",
                      labels = c("CLOSED" = "Froze", "OPENED" = "Thawed")) +
  scale_alpha(range = c(0.3, 1),
              name = "",
              breaks = c(0.5, 1),
              labels = c("Data point", "Outliers")) +
  # scale_x_continuous(breaks = c(seq(1850, 2017, by = 50)),
  #                    minor_breaks = seq(1850, 2017, by = 25)) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))
  annotate("label", x = c(1855, 2017), y = rep(middle.date, 2),
           label = c(1855, 2017))

  
# group = cut_width(WINTER, 20),
p2 <- p2 + scale_y_continuous(trans = rev_date)