#dalton anderson

rm(list=ls())

library(dplyr)
library(ggplot2)
library(readxl)
library(corrplot)
library(stargazer)
library(PerformanceAnalytics)
library(stargazer)
library(lme4)


#import data
df_master <- read_excel("BigMartSales.xlsx")

#lower case all columns
colnames(df_master)=tolower(make.names(colnames(df_master)))

#data wrangling

#after quick look it seems here is missing data for outlet info and item weight
View(df_master) 

dim(df_master)
str(df_master)

#check for na
df = df_master
colSums(is.na(df))
#item_weigth has 1463 nas
#outlet_size has 2410 nas

#feature engineering

#not needed for this dataset
#could create outlet age, but wouldn't change the model.

#descriptive statistics 

hist(df$item_sales)
#not a normal distriution 

#try log
hist(log(df$item_sales))
#somewhat normal



df$log_item_sales = log(df$item_sales)
#item_mrp units are in dollars so I am pretty sure it must also be log.
df$log_item_mrp = log(df$item_mrp)

as.factor(df$city_type)
as.factor(df$outlet_type)
as.factor(df$item_type)
#city type
ggplot(df, aes(x=city_type, y=item_sales)) +
  geom_boxplot() +
  labs(title="Item Sales by City",x="City Type", y = "Sales") +
  geom_boxplot(fill= 'darkgreen', outlier.shape=15,
               outlier.size=2)

#tier 3 has the highest sales, but that doesn't mean much.

#outlet type
ggplot(df, aes(x=outlet_type, y=item_sales)) +
  geom_boxplot() +
  labs(title="Item Sales by Outlet",x="Outlet Type", y = "Sales") +
  geom_boxplot(fill= 'gold3', outlier.shape=15,
               outlier.size=2)

#super market 3 has much higher sales than other supermarkets.
#i think it may because the the store is larger.

#item type
ggplot(df, aes(x=item_type, y=item_sales)) +
  geom_boxplot() +
  labs(title="Item Sales by Item",x="Item Type", y = "Sales") +
  geom_boxplot(fill= 'steelblue', outlier.shape=15,
               outlier.size=2)

#seems the median sales for most items are close.

#fat content
ggplot(df, aes(x=item_fat_content, y=item_sales)) +
  geom_boxplot() +
  labs(title="Item Sales by Item",x="Item Type", y = "Sales") +
  geom_boxplot(fill= 'gray', outlier.shape=15,
               outlier.size=2)

#plot correlation
df %>% select(df,item_sales)

cor_plot= df %>%
  dplyr::select(item_visibility,item_mrp,outlet_year,item_sales) %>%
  group_by(item_sales)


pairs(cor_plot)                                          # Pair plots
cor(df[1:4])                                       # Correlation matrix
chart.Correlation(cor(df[1:4]))

#item_mrp has a positive correlation with item_sales
#item_visibility looks like a mess

# scatter plot of item_visibility and log_item_sales
ggplot(df, aes(x=item_visibility, y=log_item_sales)) +
  geom_point(size=2, shape=17)

#there is a negative relationship between item visibility and item sales, weird.

#model creation

#osl models
#things to look for
#linearity 
#homoscedasticity
#independence
#normality

olm1 <- lm(log_item_sales ~ item_mrp + outlet_year + city_type + outlet_type, data=df)
summary(olm1)
plot(olm1)

olm <- lm(log_item_sales ~ item_visibility + log_item_mrp + outlet_year + city_type + outlet_type, data=df)
summary(olm)
plot(olm)
#failed assumptions 


#fixed effect model
fe1 <- lm(log_item_sales ~ item_visibility + log_item_mrp + outlet_year + city_type + item_type + outlet_type + outlet_id, data=df)
summary(fe1)

fe <- lm(log_item_sales ~ item_visibility + log_item_mrp + outlet_year + city_type + outlet_type + outlet_id, data=df)
summary(fe)

finef(fe)

#random effect model
re1 <- lmer(log_item_sales ~ item_visibility + log_item_mrp + outlet_year + city_type + item_type + outlet_type + 
             (1 | outlet_id), data=df,REML = FALSE)
summary(re1)

re <- lmer(log_item_sales ~ item_visibility + log_item_mrp + outlet_year + city_type + outlet_type + 
             (1 | outlet_id), data=df,REML = FALSE)
summary(re)
print(re, correlation=TRUE)

ranef(re)

AIC(olm,fe,re)
BIC(olm,fe,re)

#compare models
library(stargazer)
stargazer(olm, fe, re, type="text", 
          title="A5. Big Mart Model Output")


