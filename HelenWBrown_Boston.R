--------------
## Question 1
--------------
## Show that the treatment and control groups are balanced with respect to the income variable (income) by comparing its distribution (mean, median, and the lower and upper quartiles) between those in the treatment group and those in the control group.

treat <-subset(boston,treatment==1)
control <-subset(boston,treatment==0)

summary(treat$income)
##  Min.  1st Qu.  Median  Mean   3rd Qu.  Max. 
## 42500   87500  135000  135182  135000  300000 

summary(control$income)
## Min.   1st Qu. Median   Mean   3rd Qu.   Max. 
## 23500  100625  135000  151154  250000  300000

## The control and treatment groups have identical median and max income levels. However, there is quite a large difference between the treatment and control 3rd quartiles, at 135,000 versus 250,000.

## Also, compare the proportion of males (male) in the treatment and control groups.

mean(treat$male==1) ## mean is equal to proportion here
## [1] 0.5272727

mean(control$male==1)
## [1] 0.5882353

## Interpret these two numbers (also in Word Doc)

## In the treatment group, 53% of participants are male. In the control group, 59% of participants are male. Although this is not a terribly large difference, in a randomised field experiment, you would want the treatment and control groups to have (near) identical attributes so that you are more certain that any differing end results were caused by the treatment rather than any differing makeup of participants. 

--------------
## Question 2
--------------
## Compute the average treatment effect (difference-in-differences) on the change in attitudes about immigration.

sum(is.na(treat$numberim.post))
## [1] 4

sum(is.na(treat$numberim.pre))
## [1] 0

sum(is.na(control$numberim.post))
## [1] 3

sum(is.na(control$numberim.pre))
## [1] 1

## Note: I used na.rm = true because the dataset's missing values were preventing me from calculating the means

immdiff <- ((mean(treat$numberim.post, na.rm = TRUE)-mean(treat$numberim.pre))) -((mean(control$numberim.post, na.rm = TRUE)-mean(control$numberim.pre, na.rm = TRUE))) 

immdiff
## [1] 0.2686543

## Be sure to state your assumptions and provide a brief interpretation of the results. (also in Word Doc)

## By analyzing the differences-in-differences result, we see what would have happened to those in the treatment group if they had not
## been exposed to a Spanish confederate at the train station.

## We see that average treatment effect of exposure to a Spanish confederate at that time on the change in attitudes about immigration was a 0.27 point increase in immigration opinion score. That is, on average, the DiD suggests that participants would have become more anti-immigration - so, more in favour of decreasing the number of immigrants allowed in the country than if they had not been treated.

## Differences-in-differences assumes a parallel time trend when determining the difference between the treatment group and what would have happened if the treatment group weren't treated.

--------------
## Question 3
--------------
## Calculate the necessary quantities to answer this question and interpret the results i.e., consider the difference-in-differences estimates for those who attended college and for those who did not

## Need to calculate two difference-in-difference estimates. One for those with college and another one for those without a college degree.

treat.college <- subset(boston, subset = (college == 1 & treatment == 1))

treat.no.college <- subset(boston, subset = (college == 0 & treatment == 1))

control.college <- subset(boston, subset = (college == 1 & treatment == 0))

control.no.college <- subset(boston, subset = (college == 0 & treatment == 0))

colldiff <- ((mean(treat.college$numberim.post, na.rm = TRUE)-mean(treat.college$numberim.pre, na.rm = TRUE))) -((mean(control.college$numberim.post, na.rm = TRUE)-mean(control.college$numberim.pre, na.rm = TRUE))) 

colldiff
## [1] 0.3050034

nocolldiff <- ((mean(treat.no.college$numberim.post, na.rm = TRUE)-mean(treat.no.college$numberim.pre, na.rm = TRUE))) -((mean(control.no.college$numberim.post, na.rm = TRUE)-mean(control.no.college$numberim.pre, na.rm = TRUE))) 

nocolldiff
## [1] 0.01785714

coll.nocoll.diff <- colldiff - nocolldiff
## [1] 0.2871463

## Is there evidence of a differential impact of treatment, conditional on attending college versus not attending college? (also in Word Doc)

## After calculating a difference-in-differences for those who attended college and then another one for the ones who did not, we can see a substantial difference between the average treatment effect on those who have attended college and those who have not. The ATE of those who attended college was 0.31, whereas the ATE of those who did not was 0.02, leading to a 0.29 point difference.
## That is, those who attended college experienced an average 0.31 points increase in their immigration opinion score (so more anti-immigration). In comparison, those who did not attend college experienced a much lower average 0.02 points increase in their immigration opinion score (so somewhat more anti-immigration).
--------------
## Question 4
--------------
## Repeat the same analysis as in the previous question but this time with respect ideology.

##  Compute the difference-in-difference estimate within each value of ideology. What patterns do you observe?

verylib.treat <- subset(boston, subset = (ideology == 1 & treatment == 1))
verylib.control <- subset(boston, subset = (ideology == 1 & treatment == 0))

verylib.diff <- ((mean(verylib.treat$numberim.post, na.rm = TRUE)-mean(verylib.treat$numberim.pre, na.rm = TRUE))) -((mean(verylib.control$numberim.post, na.rm = TRUE)-mean(verylib.control$numberim.pre, na.rm = TRUE))) 

verylib.diff
## [1] 0.3111111

lib.treat <- subset(boston, subset = (ideology == 2 & treatment == 1))
lib.control <- subset(boston, subset = (ideology == 2 & treatment == 0))

lib.diff <- ((mean(lib.treat$numberim.post, na.rm = TRUE)-mean(lib.treat$numberim.pre, na.rm = TRUE))) -((mean(lib.control$numberim.post, na.rm = TRUE)-mean(lib.control$numberim.pre, na.rm = TRUE))) 

lib.diff
## [1] 0.8033265

mod.treat <- subset(boston, subset = (ideology == 3 & treatment == 1))
mod.control <- subset(boston, subset = (ideology == 3 & treatment == 0))

mod.diff <- ((mean(mod.treat$numberim.post, na.rm = TRUE)-mean(mod.treat$numberim.pre, na.rm = TRUE))) -((mean(mod.control$numberim.post, na.rm = TRUE)-mean(mod.control$numberim.pre, na.rm = TRUE))) 

mod.diff
## [1] -0.35

con.treat <- subset(boston, subset = (ideology == 4 & treatment == 1))
con.control <- subset(boston, subset = (ideology == 4 & treatment == 0))

con.diff <- ((mean(con.treat$numberim.post, na.rm = TRUE)-mean(con.treat$numberim.pre, na.rm = TRUE))) -((mean(con.control$numberim.post, na.rm = TRUE)-mean(con.control$numberim.pre, na.rm = TRUE))) 

con.diff
## [1] 0.2363636

verycon.treat <- subset(boston, subset = (ideology == 5 & treatment == 1))
verycon.control <- subset(boston, subset = (ideology == 5 & treatment == 0))

verycon.diff <- ((mean(verycon.treat$numberim.post, na.rm = TRUE)-mean(verycon.treat$numberim.pre, na.rm = TRUE))) -((mean(verycon.control$numberim.post, na.rm = TRUE)-mean(verycon.control$numberim.pre, na.rm = TRUE))) 

verycon.diff
## [1] 0.75

## What patterns do you observe? Give a brief substantive interpretation of the results. (also in Word Doc)

## The differences in ATE by ideology suggests that, generally, the treatment of having a spanish-speaking confederate, increased the participants' anti-immigration stance/support for a reduction in immigration numbers.
## The positive ATE for all ideologies except moderate suggests that the treatment generally had a positive affect on participants, increasing their propensity to support anti-immigration. The strongest treatment effect was found in the liberal ideology type, with a 0.8 point average increase in immigration opinion score, closely followed by the very conservative ideology type, with an ATE of 0.75, so an average 0.75 point increase in immigration opinion score (so more anti-immigration).
## The only ideology which experienced a negative ATE and so  an average decrease in immigration opinion score due to the treatment was the moderate ideology type. On average, moderate participants experienced a 0.35 point decrease in immigration opinion score (so more pro-immigration).
  