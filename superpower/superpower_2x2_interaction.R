library(Superpower)

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

write.csv(report, file = "/cloud/project/report_superpower.csv")

ggplot(report) + geom_point(aes(x = f_a, y = pwr_a)) 
ggplot(report) + geom_point(aes(x = f_ab, y = pwr_ab)) 
ggplot(report) + geom_point(aes(x = f_b, y = pwr_b))


# How does the effect size of the interaction influence the power 
# to detect a interaction effect? 

ggplot(report)+ 
  geom_point(aes(x = f_ab, y = pwr_ab)) + 
  geom_hline(yintercept = 80, linetype = "dashed", color = "red")


# proportions of studies with achieved power for interaction effect of 80%: 
nrow(filter(report, report$pwr_ab > 80))/ nrow(report)

nrow(filter(report, report$pwr_a > 80))/nrow(report)

summary(report$pwr_ab)

sum(report$pwr_a == report$pwr_ab)



# scenario: no interaction 

design_result_no_interaction <- ANOVA_design(
  design = "2b*2b", n = 10,
  mu = c(0.1, 0.2, 0.3, 0.4), sd = 0.5,
  labelnames = c("a",
                 "control", "treatment",
                 "b",
                 "male", "female"), 
  plot = TRUE)

ANOVA_exact(design_result_no_interaction)

# scenario: partially attenuated interaction 

design_result_partial_interaction <- ANOVA_design(
  design = "2b*2b", n = 10, 
  mu = c(0.1, 0.2, 0.5, 0.3), sd = 0.5, 
  labelnames = c("a",
                 "control", "treatment",
                 "b",
                 "male", "female"), 
  plot = TRUE)

ANOVA_exact(design_result_partial_interaction)

# scenario: fully attenuated interaction 

design_result_fully_interaction <- ANOVA_design(
  design = "2b*2b", n = 10, 
  mu = c(0.1, 0.2, 0.6, 0.2), sd = 0.5, 
  labelnames = c("a",
                 "control", "treatment",
                 "b",
                 "male", "female"), 
  plot = TRUE)

ANOVA_exact(design_result_fully_interaction)

# scenario: cross-over interaction 

design_result_crossover_interaction <- ANOVA_design(
  design = "2b*2b", n = 10, 
  mu = c(0.3, 0.2, 0.1, 0.7), sd = 0.5, 
  labelnames = c("a",
                 "control", "treatment",
                 "b",
                 "male", "female"), 
  plot = TRUE)

ANOVA_exact(design_result_crossover_interaction)







                            