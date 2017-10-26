library(tidyverse)
library(readr)

test <- read_csv("test.csv")

summary(test)


na_count <- sapply(test, function(x) sum(length(which(is.na(x)))))

na_df <- data.frame(keyName = names(na_count),
                    value = na_count, 
                    row.names = NULL)

#Visualizing the missing values
na_df%>%
  arrange(desc(value))%>%
  head(n=20)%>%
  ggplot(aes(x = reorder(keyName,value),
             y = value,
             fill = value))+
  labs(x = "Feature",
       y = "Missing Count",
       title = "Visualizing Missing Value Count")+
  geom_bar(stat = "identity")+
  coord_flip()

#Thus we see that there are a fairly large number of missing values in the dataset, almost equivalent to the number of observations in the dataset. 
# On closer observation of the dataset and it was observed that features that were not included in the house were listed as NA hence these features will be handled by replacing them with "None".




test%>%
  group_by(Neighborhood,MSZoning)%>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))%>%
  ggplot(aes(x = Neighborhood,
             y = freq,
             fill = freq))+
  geom_bar(stat = "identity")+
  labs(y = "Freq",
       title = "Zoning of the neighborhoods")+
  coord_flip()+
  facet_wrap(~MSZoning)


