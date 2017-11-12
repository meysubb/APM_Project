library(tidyverse)
library(treemapify)
library(ggridges)
train_df <- read_csv("store25.csv")
items_df <- read_csv("items.csv")
stores <- read_csv("stores.csv")


oil <- 
transc_df <- read_csv("transactions.csv")
transc_df <- transc_df %>% filter(store_nbr==25)
  
ggplot() +
  geom_line(data=transc_df,aes(x=date,y=transactions)) + 
  labs(y="Transactions",title="Salinas,Ecuador") + 
  theme_bw()

### Item Sales sold in Store 25 by class

itemsales <- train_df %>% group_by(item_nbr) %>% 
  summarize(sales=sum(unit_sales)) %>% inner_join(.,items_df)

itemsales %>% group_by(family,class) %>% count() %>%ungroup() %>% filter(n > 1) %>%
  mutate(n = log10(n+1)) %>% ggplot(aes(area = n, fill = class, label = class, subgroup = family)) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                               "black", fontface = "italic", min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
  theme(legend.position = "null") +
  labs(title="Item classes grouped by family - tree map (logarithmic scaling)")

