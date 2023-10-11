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
