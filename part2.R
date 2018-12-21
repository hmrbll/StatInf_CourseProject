# 1 - Load the ToothGrowth data and perform some basic exploratory data analyses &
# 2 - Provide a basic summary of the data.
# Load ToothGrowth data
data("ToothGrowth")

# Summary of data
summary(ToothGrowth)

# First few rows
head(ToothGrowth)

# Exploratory Plot
library(ggplot2)
ggplot(data = ToothGrowth, aes(x = dose, y = len)) + 
                geom_boxplot(aes(fill = dose, group = dose)) + 
                        facet_grid( ~ supp) +
                                labs(x = "Dose Amount", y = "Tooth Length", title = "Tooth Length against Dose Amount") +
                                        theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# 3 - Run t-tests for different variables and look for significance
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
t1 <- t.test(data = ToothGrowth, len ~ supp)
ToothGrowth_0.5_1.0 <- subset(ToothGrowth, ToothGrowth$dose %in% c(0.5, 1.0))
t2 <- t.test(len ~ dose,data = ToothGrowth_0.5_1.0)
ToothGrowth_0.5_2.0 <- subset(ToothGrowth, ToothGrowth$dose %in% c(0.5, 2.0))
t3 <- t.test(len ~ dose,data = ToothGrowth_0.5_2.0)
ToothGrowth_1.0_2.0 <- subset(ToothGrowth, ToothGrowth$dose %in% c(1.0, 2.0))
t4 <- t.test(len ~ dose,data = ToothGrowth_1.0_2.0)

#Look at the results
t1$estimate
t1$p.value
t1$conf.int

t2$estimate
t2$p.value
t2$conf.int

t3$estimate
t3$p.value
t3$conf.int

t4$estimate
t4$p.value
t4$conf.int