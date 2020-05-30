# Summary statistics -- Feel Good Diaries

library(dplyr)
library(ggplot2)

df <- readRDS('data/refinery29_cols_time.rda')

df %>%
  filter(!is.na(age)) %>%
  mean(age)

round(mean(df$age, na.rm = T))
mean(df$salary, na.rm=T)
median(df$salary, na.rm=T)

df %>%
  filter(!is.na(age),
         !is.na(salary)) %>%
  ggplot(aes(x = salary, y = age)) + 
  geom_point() + 
  labs(title = "No strong relationship between age and salary")


df %>%
  select(salary, Pub_date, weekTotal) %>%
  filter(!is.na(salary),
         !is.na(weekTotal)) %>%
  mutate(sal_p_week = salary/52,
         above_budget = sal_p_week < weekTotal )%>%
  ggplot(aes(x = Pub_date,y= sal_p_week, color = above_budget
             )) + 
  geom_point() + 
  labs(title = 'Most women spend below their approx. weekly budget')

df %>% 
  select(salary, Pub_date, weekTotal) %>%
  mutate(covid = Pub_date > as.Date('3/23/2020', format = '%m/%d/%y')) %>%
  filter(!is.na(salary),
         !is.na(weekTotal)) %>%
  ggplot(aes(x=Pub_date, y = weekTotal, color = covid)) + 
  geom_point() + 
  labs(title = 'Women are spending less on wellness during COVID-19')

df %>%
  select(salary, Pub_date, weekTotal) %>%
  filter(!is.na(salary),
         !is.na(weekTotal)) %>%
  mutate(covid = Pub_date > as.Date('3/23/2020', format = '%m/%d/%y'), 
         sal_p_week = salary/52,
         above_budget = sal_p_week > weekTotal) %>%
  filter(covid ==T) %>%
  ggplot(aes(x = salary, y=weekTotal, color = above_budget)) + 
  geom_point() + 
  labs(title = 'Among the 7 authors writing during COVID, 5 are spending less than their weekly budget')
         
df %>%
  select(salary, Pub_date, weekTotal) %>%
  filter(!is.na(salary),
         !is.na(weekTotal)) %>%
  mutate(covid = Pub_date > as.Date('3/23/2020', format = '%m/%d/%y'),
         pct_wellness_exp = weekTotal/salary) %>%
  ggplot(aes(x = Pub_date, y = pct_wellness_exp, color = covid)) + 
  geom_point()

df %>%
  select(salary, Pub_date, weekTotal) %>%
  filter(!is.na(salary),
         !is.na(weekTotal)) %>%
  mutate(covid = Pub_date > as.Date('3/23/2020', format = '%m/%d/%y'),
         pct_wellness_exp = weekTotal/salary) %>%
  group_by(covid) %>%
  mutate(m_pct = mean(pct_wellness_exp),
         above_average = pct_wellness_exp > m_pct) %>%
  ungroup() %>% 
  ggplot(aes(x = Pub_date, y = pct_wellness_exp, color = above_average)) + 
  geom_point() + 
  labs(title = 'Some women are willing to spend more on wellness during covid than women before covid. ')

df %>%
  select(salary, Pub_date, weekTotal) %>%
  filter(!is.na(salary),
         !is.na(weekTotal)) %>%
  mutate(covid = Pub_date > as.Date('3/23/2020', format = '%m/%d/%y')) %>%
  ungroup() %>% 
  ggplot(aes(x = Pub_date, y = weekTotal, color = covid)) + 
  geom_point() + 
  labs(title = 'From July 2019 to January 2020, more women were frugal on wellness spending than women during covid-19.')

