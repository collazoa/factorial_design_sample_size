library(Superpower)
library(tidyverse)

# this first part was from one of your scripts I got from the old repository.

a1_b1 <- seq(from = 0,to = 2,by = 0.2)
a1_b2 <- seq(from = 0,to = 2,by = 0.2)
a2_b1 <- seq(from = 0,to = 2,by = 0.2)
a2_b2 <- seq(from = 0,to = 2,by = 0.2)
n <- c(10)
sd <- c(0.5)


report <- expand.grid(a1_b1 = a1_b1, 
                      a1_b2 = a1_b2, 
                      a2_b1 = a2_b1, 
                      a2_b2 = a2_b2, 
                      n = n, 
                      sd = sd)

report <- report %>%
  mutate(f_a = FALSE, 
         f_b = FALSE, 
         f_ab = FALSE, 
         pwr_a = FALSE, 
         pwr_b = FALSE, 
         pwr_ab = FALSE)


for (i in 1:nrow(report)) {
  
  design_result <- ANOVA_design(
    design = "2b*2b", n = report[i,5],
    mu = as.numeric(report[i,1:4]), sd = report[i,6],
    labelnames = c("a",
                   "treatment", "control",
                   "b",
                   "male", "female"), 
    plot = FALSE)
  
  result_exact <- ANOVA_exact(design_result)
  
  report$f_a[i] <- result_exact$main_results$cohen_f[1]
  report$f_b[i] <- result_exact$main_results$cohen_f[2]
  report$f_ab[i] <- result_exact$main_results$cohen_f[3]
  
  report$pwr_a[i] <- result_exact$main_results$power[1]
  report$pwr_b[i] <- result_exact$main_results$power[2]
  report$pwr_ab[i] <- result_exact$main_results$power[3]
  
}

# How does the effect size of the interaction influence the power 
# to detect a interaction effect? 

ggplot(report)+ 
  geom_point(aes(x = f_ab, y = pwr_ab)) + 
  geom_hline(yintercept = 80, linetype = "dashed", color = "red")

## now, I started wondering about why the shapes of interactions didn't have any influence in this calculation.
## my initial idea was that it might be easier that reversed interactions had larger interaction effect sizes, so that is what I tried to test.

# first, I removed all scenarios without interaction effects from the previous dataset
report2 = report %>% filter(f_ab!=0)

# then, I tried to create classification rules to name all interactions (taxonomy from Sommet et al.)
# this would be the most critical part to check, if the rules really encompass all possibilities...
report2 = report2 %>% mutate(type_int = case_when(
  (a1_b1<a1_b2 & a2_b1>a2_b2 & a2_b1!=a2_b2) ~ "reversed",
  (a1_b1>a1_b2 & a2_b1<a2_b2 & a1_b1!=a1_b2) ~ "reversed",
  (a1_b1==0 & a1_b2==a2_b1 & a2_b1==a2_b2 & a2_b2!=0) ~ "fully attenuated",
  (a1_b2==0 & a1_b1==a2_b1 & a2_b1==a2_b2 & a2_b2!=0) ~ "fully attenuated",
  (a2_b1==0 & a1_b2==a1_b1 & a1_b1==a2_b2 & a2_b2!=0) ~ "fully attenuated",
  (a2_b2==0 & a1_b2==a2_b1 & a2_b1==a1_b1 & a1_b1!=0) ~ "fully attenuated",
  (a1_b1!=0 & a1_b2==a2_b1 & a2_b1==a2_b2 & a2_b2==0) ~ "fully attenuated",
  (a1_b2!=0 & a1_b1==a2_b1 & a2_b1==a2_b2 & a2_b2==0) ~ "fully attenuated",
  (a2_b1!=0 & a1_b2==a1_b1 & a1_b1==a2_b2 & a2_b2==0) ~ "fully attenuated",
  (a2_b2!=0 & a1_b2==a2_b1 & a2_b1==a1_b1 & a1_b1==0) ~ "fully attenuated",
  TRUE ~ "partially attenuated"))

# finally, answering the question
# does the distribution of interaction effect sizes differ among the types of interactions?

ggplot(report2, aes(y=f_ab, x=type_int)) + 
  geom_violin(draw_quantiles = T)

report2 %>% group_by(type_int) %>% summarize(median(f_ab))
