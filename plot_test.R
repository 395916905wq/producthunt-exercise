library(tidyverse)
library(lubridate)
library(ggthemes)

AllTopics <- read.csv('./Data/AllTopics.csv') %>% as_tibble()
PostsForAnalysis <- read.csv('./Data/PostsForAnalysis.csv') %>% as_tibble()
PostsTopicsForAnalysis <- read.csv('./Data/PostsTopicsForAnalysis.csv') %>% as_tibble()
UsersForAnalysis <- read.csv('./Data/UsersForAnalysis.csv') %>% as_tibble()

colnames(PostsForAnalysis)
old <- theme_set(theme_tufte() + theme(text = element_text(family = "Menlo"), legend.position = "bottom"))

# first question ---------------------------------------------------------------

AllTopics %>% select(name,num_followers,num_posts) %>% 
  mutate(rk_posts = row_number(desc(num_posts))) %>% 
  filter(rk_posts <= 20) %>% 
  arrange(rk_posts) %>% 
  ggplot()+
  geom_point(aes(name,num_followers,size = num_posts,col='red'))+
  geom_bar(aes(name,num_followers),stat = 'identity',width = 0.1,alpha=0.1)+
  labs(x = "Topics", y = "Posts", num_posts = "Grade")


# Second question ---------------------------------------------------------

PostsForAnalysis %>% select(date,time_of_day,votes_count,product_state) %>% 
  #filter(product_state=='pre_launch') %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(year,time_of_day) %>% 
  summarise(votes=sum(votes_count)) %>% 
  ggplot()+
  geom_bar(aes(as.factor(year),votes,fill=time_of_day),
           stat = 'identity',width = 0.1,position = 'dodge',show.legend = FALSE)+
  labs(x = "Year", y = "Votes")

# Third question ----------------------------------------------------------

PostsForAnalysis %>% as_tibble() %>% 
  #filter(product_state=='pre_launch') %>% 
  select(id,user_id,votes_count) %>% 
  group_by(user_id) %>% 
  summarise(sum_votes = sum(votes_count),n=n()) %>% 
  mutate(avg_votes = sum_votes/n) %>% 
  filter(n >= 30) %>%
  mutate(rk = row_number(desc(avg_votes))) %>% 
  filter(rk <= 20) %>% 
  arrange(rk) %>% 
  left_join(select(UsersForAnalysis,user_id,name),by = "user_id") %>% 
  ggplot()+
  geom_bar(aes(reorder(name,avg_votes,fun=count),avg_votes), stat = 'identity',width = 0.1,show.legend = FALSE)+
  geom_point(aes(name,avg_votes,size = 2,col='green'),show.legend = FALSE)+
  coord_flip()+
  labs(x = "Name", y = "Avg_votes")
 
# Fourth question ----------------------------------------------------------

AllTopics %>% select(name,num_followers,num_posts) %>% 
  filter(name != "tech") %>% 
  mutate()


