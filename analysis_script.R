library(rstatix)
library(tidyverse)

df <- read.delim('RExam.dat')
head(df)

mod1 <- lm(exam ~ ., data = df)
summary(mod1)

df %>%
  ggplot(aes(uni, exam)) +
  geom_point(color = 'blue2',
             alpha = .6,
             shape = 4,
             position = position_jitter(.05, 0)) +
  geom_smooth(method = 'lm', color = 'red') +
  theme_light() +
  scale_x_continuous(breaks = seq(0, 1, 1))

ggsave('first regression plot.jpeg', device = 'jpeg', units = 'cm')


# get functions from misc_functions.R
library(devtools)
url <- 'https://github.com/ryanetracy/misc_functions/blob/main/misc_functions.R?raw=TRUE'
source_url(url)

m1 <- mean(df$exam[df$uni == 0])
m2 <- mean(df$exam[df$uni == 1])
sd1 <- sd(df$exam[df$uni == 0])
sd2 <- sd(df$exam[df$uni == 1])

get_cohen_d(m1, m2, sd1, sd2)


df$uni <- factor(df$uni)

m2 <- lm(exam ~ uni * lectures, data = df)
summary(m2)

df %>%
  ggplot(aes(lectures, exam, color = uni)) +
  geom_point(alpha = .6,
             shape = 4,
             position = position_jitter(.05, 0)) +
  geom_smooth(method = 'lm') +
  scale_color_manual(values = c('turquoise2', 'black')) +
  theme_light()

ggsave('lectures dont work.jpeg', device = 'jpeg', units = 'cm')
