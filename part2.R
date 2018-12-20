# 1 - Load the ToothGrowth data and perform some basic exploratory data analyses &
# 2 - Provide a basic summary of the data.
# Load ToothGrowth data
data("ToothGrowth")

# Summary of data
summary(ToothGrowth)

# First few rows
head(ToothGrowth)

# Exploratory Plot
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
ggplot(data = ToothGrowth, aes(x = dose, y = len)) + 
                geom_boxplot(aes(fill = dose)) + 
                        facet_grid( ~ supp) +
                                labs(x = "Dose Amount", y = "Tooth Length", title = "Tooth Length against Dose Amount") +
                                        theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


# ------------------
# run t-test
t.test(data = ToothGrowth, len ~ supp)

#The p-value of this test was 0.06.
#Since the p-value is greater than 0.05 and the confidence interval of the test contains zero we can say that supplement types seems to have no impact on Tooth growth based on this test.

#Now we'll compare tooth growth by dose, looking at the different pairs of dose values.
# run t-test using dose amounts 0.5 and 1.0
ToothGrowth_sub <- subset(ToothGrowth, ToothGrowth$dose %in% c(1.0,0.5))
t.test(len~dose,data=ToothGrowth_sub)
# run t-test using dose amounts 0.5 and 2.0
ToothGrowth_sub <- subset(ToothGrowth, ToothGrowth$dose %in% c(0.5,2.0))
t.test(len~dose,data=ToothGrowth_sub)
# run t-test using dose amounts 1.0 and 2.0
ToothGrowth_sub <- subset(ToothGrowth, ToothGrowth$dose %in% c(1.0,2.0))
t.test(len~dose,data=ToothGrowth_sub)
#As can be seen, the p-value of each test was essentially zero and the confidence interval of each test does not cross over zero (0).

#Based on this result we can assume that the average tooth length increases with an inceasing dose, and therefore the null hypothesis can be rejected.

#Conclusions
#Given the following assumptions:
        
#1.The sample is representative of the population
#2.The distribution of the sample means follows the Central Limit Theorem
#In reviewing our t-test analysis from above, we can conclude that supplement delivery method has no effect on tooth growth/length, however increased dosages do result in increased tooth length.