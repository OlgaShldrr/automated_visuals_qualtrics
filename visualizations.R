library(tidyverse)
library(scales)

pie_data <- evaluations_tidy %>% 
  select(q3) %>% 
  na.omit() %>% 
  group_by(q3) %>% 
  summarise(n=n())

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

pie_chart <- ggplot(data=pie_data, aes(x="", y=n, fill=q3))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  labs(title = "", fill="", x="", y="") +
  scale_fill_manual(values=c("#95B15A", "#EBEED1")) +
  blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = n/2 + c(0, cumsum(n)[-length(n)]), 
                label = paste0(n," (",percent(n/21),")")), size=5)

most_valuable <- evaluations_tidy %>% 
  select(q4) %>% 
  na.omit() 

suggestions <- evaluations_tidy %>% 
  select(q5) %>% 
  na.omit() 


expectations_data <- evaluations_tidy %>% 
  select(starts_with("q2")) %>% 
  gather(key = "key", value="value") %>% 
  na.omit() %>% 
  mutate(key = recode(key,q2_1="The networking aspect \nof this meeting...",
               q2_2="The educational component/\nresource sharing \nof this meeting...",
               q2_3="The effectiveness of \nthe facilitator(s)...",
               q2_4="The style/format \nof this meeting...",
               q2_5="The learning \nenvironment",
               q2_6="The value I received \nby attending this meeting...")) %>% 
  group_by(key, value) %>% 
  summarise(n=n()) %>% 
  mutate(value= factor(value, levels=c("Did Not Meet My Expectations", "Met My Expectations","Exceeded My Expectations")))
                       

expectations_chart <- ggplot(expectations_data, aes(x=value, y=n, fill=value))+
  geom_bar(stat="identity")+
  facet_wrap(~ key)+
  labs(x="", fill="")+
  scale_fill_manual(values=c("#A8C273", "#EBEED1","#80BCBE")) +
  blank_theme +
  theme(axis.text.x=element_blank(),legend.position="bottom")+
  geom_text(label=expectations_data$n)

