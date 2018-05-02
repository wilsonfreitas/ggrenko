
library(dplyr)
library(ggplot2)

IBOV <- rbcb::get_series(c(IBOV = 7), start_date = "2016-01-01")

ggplot(IBOV, aes(x = date, y = IBOV)) + geom_line()

# ----

points <- 2000
brick <- numeric(dim(IBOV)[1])
x1 <- IBOV$IBOV[1]
n_brick <- 1
for (ix in seq_len(dim(IBOV)[1])) {
  if (is.na(IBOV$IBOV[ix])) {
    brick[ix] <- n_brick
    next
  }
  if (IBOV$IBOV[ix] > (x1 + points) | IBOV$IBOV[ix] < (x1 - points)) {
    x1 <- IBOV$IBOV[ix+1]
    brick[ix] <- n_brick
    n_brick <- n_brick + 1
    next
  }
  brick[ix] <- n_brick
}

brick
table(brick)

# ----

IBOV$renko_bricks <- brick
IBOV$renko_bricks_eo <- brick %% 2

ggplot(IBOV, aes(x = date, y = IBOV, colour = renko_bricks_eo)) + geom_line()

# ----

IBOV_renko <- IBOV %>% group_by(renko_bricks) %>%
  summarise(first = first(IBOV), last = last(IBOV), date = first(date)) %>%
  mutate(change = last - first, up_change = change > 0, 
         xmin = renko_bricks, xmax = xmin + 1)

ymin = numeric(dim(IBOV_renko)[1])
ymin[1] = IBOV_renko$first[1]
up_change_i = as.integer(IBOV_renko$up_change)
change = up_change_i + (up_change_i - 1)
for (ix in seq_len(dim(IBOV_renko)[1])[-1]) {
  ymin[ix] = ymin[ix-1] + change[ix] * points
}

IBOV_renko$ymin = ymin
IBOV_renko$ymax = ymin + points

ix <- sample(59, 4)

ggplot(IBOV_renko, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = up_change)) +
  geom_rect(colour = "black", size = 0.25) +
  scale_fill_manual(values = c("TRUE" = "white", "FALSE" = "black")) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(
    breaks = IBOV_renko$xmin[ix],
    label = format(IBOV_renko$date[ix])
  )



# ----

qplot(diff(IBOV$IBOV), geom = "histogram")

