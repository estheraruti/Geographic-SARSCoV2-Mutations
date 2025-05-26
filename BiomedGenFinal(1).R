setwd("~/Biomedical Genomics/")

library(tidyverse)
library(ggpubr)
library(tibble)

dataset <- read_csv("group1_dataset(1).csv")
glimpse(dataset)
view(dataset)
str(dataset)

#1 More mutations correlated with date?
dataset %>% 
  ggplot(aes(x = Collection_date, y = Muts_counts, color = as.factor (Mut_type))) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  facet_wrap(~Mut_type) +
  labs(x = "Date", y= "# of Isolates", color = "Mutation Type", title = "Missense vs. Synonymous Mutations Over Time") + 
  theme(title = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"), axis.text.x = element_text(angle = 45, size = 7))

mis <- dataset %>% filter(Mut_type == "mis")
syn <- dataset %>% filter(Mut_type == "syn")

mislm = lm(data = mis, as.numeric(Collection_date)~Muts_counts)
summary(mislm)  # used as.numeric due to time data type

synlm = lm(data = syn, as.numeric(Collection_date)~Muts_counts)
summary(synlm)

#2a Mutation type vs mutation count
dataset %>% 
  ggplot(aes(x = Mut_type, y = Muts_counts, fill = as.factor (Mut_type))) +
  geom_boxplot() +
  labs(x = "Mutation Type", y= "Mutation Count", color = "Mutation Type", title = "Mutation Count vs. Mutation Type")+ 
  theme(title = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"))

#2b Are more mutations associated with more missense mutations?
dataset %>%
  group_by(Isolate) %>% 
  ggplot(aes(x = Muts_counts, fill = as.factor(Mut_type))) +
  geom_histogram() +
  facet_wrap(~Mut_type) +
  labs(x = "Mutation Count", y= "# of Isolates", fill = "Mutation Type", title = "Prevalence of Missense vs. Synonymous Mutations") + 
  theme(title = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"))

t.test(data = dataset, Muts_counts~Mut_type)

#3 Are there more mutations in US vs UK?
dataset %>%
  group_by(Isolate) %>% 
  ggplot(aes(x = Muts_counts, fill = as.factor(Geo_origin))) +
  geom_histogram() +
  facet_wrap(~Geo_origin) +
  labs(x = "Mutation Count", y= "# of Isolates", fill = "Country", title = "Number of Mutations in US vs. UK")+ 
  theme(title = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"))

dataset %>%
  group_by(Isolate) %>% 
  ggplot(aes(x = Geo_origin, y = Muts_counts, fill = as.factor(Geo_origin))) +
  geom_boxplot() +
  labs(x = "Country", y= "Number of Mutations", fill = "Country", title = "Number of Mutations in US vs. UK")+ 
  theme(title = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"))

t.test(data = dataset, Muts_counts~Geo_origin)
