# Data Application Lab - copyright

# Q1: find out which variable can affect conversion rate
# Q2: how to improve conversion rate

# rm(list = ls()) remove all current variables

library(tidyverse)

rev <- read.csv("rev30_weekly.csv")
View(rev)
names(rev)
str(rev)
summary(rev)
head(rev)

# Visualization Analysis
hist(rev$econv_rate)
summary(rev$econv_rate)

# create a new variable: cr
rev$cr <- factor(ifelse(rev$econv_rate >0.033, '2.high', 
                       ifelse(rev$econv_rate < 0.02, '0.low', '1.normal')))
table(rev$cr)
prop.table(table(rev$cr))
str(rev)

## 1. Impressions vs. CTR vs. Level of conversion rate
ggplot(rev, aes(x=impressions, y=CTR)) + geom_point(aes(colour=cr))

# no positive linear relationship with impressions and CTR.
# high impression won't result in high CTR
# most of high conversion rate points appear between 0 to 20000 impressions range.


## 2. Conversion rate vs. Impressions
rev %>% 
  group_by(cr) %>% 
  summarise(avg_imp=mean(impressions)) %>%
  ggplot( aes(x=cr, y=avg_imp)) + 
  geom_bar(stat='identity', aes(fill=cr)) +
  geom_text(aes(label=round(avg_imp,0), y=avg_imp+500), size=5)

# high impression wont' resuilt in high conversion rate
# low converation rate level has the highest average impressions

## 3. Conversion rate vs. rank position 
  rev %>% 
  group_by(cr) %>% 
  summarise(avg_pos = mean(avg_pos)) %>%
  ggplot(aes(x=cr, y=avg_pos)) +
  geom_bar(stat='identity', aes(fill=cr))
  
# low converation rate level has the highest average rank position

## 4. Conversion rate vs. unit_dp_pc
df4 = rev %>% 
  group_by(cr) %>% 
  summarise(avg_unit_dp_pc = mean(unit_dp_pc));df4

ggplot(df4, aes(x=cr, y=avg_unit_dp_pc)) +
  geom_bar(stat='identity', aes(fill=cr)) +
  geom_text(aes(label=round(avg_unit_dp_pc, 2), y=avg_unit_dp_pc))

## low converation rate has negative average unit depth %
## high converation rate has highest average unit depth %
## unit depth% will affect our converation rate


## 5. Conversion rate vs. Year
str(rev$Week)
year = as.numeric(substring(as.character(rev$Week),1,4)); year
rev$year = year
class(year)
ggplot(rev, aes(x=year,y=econv_rate)) + 
  geom_bar(stat='summary', fun.y='mean', aes(fill=factor(year)))

# conversion rate increased over years

## 6. Conversion rate vs. prod_br
summary(rev$prod_br)
df9 = rev %>% 
  group_by(cr) %>% 
  summarise(avg_prod_br = mean(prod_br));df9
ggplot(df9, aes(x=cr, y=avg_prod_br)) +
  geom_bar(stat='identity', aes(fill=cr)) +
  geom_text(aes(label=round(avg_prod_br, 2), y=avg_prod_br+5))

# product breadth will affect our converstion rate


### Correlation
library(corrplot)
names(rev)
str(rev)
col = c('pcode', 'Week', 'weeknum', 'cr')

M = cor(rev$econv_rate,rev[!names(rev) %in% col], use='complete.obs');M

# CTR: 0.088
# avg.position: -0.063
# prod.breadth: 0.149
# unit.depth: 0.147
# newness_style: -0.063
# newness_quantity: -0.038
# size avail overall: 0.075
# Size avail page view: 0.102
# year: 0.181

corrplot(M)
str(rev)
N = cor(rev[,!names(rev) %in% c('pcode','cr')], use='complete.obs');N
corrplot(N)

### linear regression
str(rev)
drop = c('pcode', 'cr','Week')
model = lm(econv_rate~., data=rev[,!names(rev) %in% drop])
summary(model)
sort(model$coefficients)

# Conclusion:
# how to improve conversion rate:
# 1. increase product breath
# 2. increase unit depth


