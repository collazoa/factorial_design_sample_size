library(Superpower)
library(tidyverse)
library(gridExtra)
library(grid)


source("./superpower/functions_superpower.R")

# scenario: no interaction 

# a: intervention, a1: control , a2: treatment 
# b: animal sex, b1: male, b2: female 

a1_b1 <- rep(0.5, 20) 
a1_b2 <- rep(0.7, 20) 
a2_b1 <- rep(seq(from = 0.6, to = 1.5, by = 0.1),2)
a2_b2 <- rep(seq(from = 0.8,to = 1.7, by = 0.1),2) 
n <- c(rep(10,10), rep(20,10))
sd <- rep(1,20)

dat <- tibble(a1_b1 = a1_b1, 
              a1_b2 = a1_b2, 
              a2_b1 = a2_b1, 
              a2_b2 = a2_b2, 
              n = n, 
              sd = sd) 

dat <- dat %>%
  mutate(f_a = FALSE, 
         f_b = FALSE, 
         f_ab = FALSE, 
         pwr_a = FALSE, 
         pwr_b = FALSE, 
         pwr_ab = FALSE)

for (i in 1:nrow(dat)) {
design_result_no_interaction <- ANOVA_design(
  design = "2b*2b", n = as.numeric(dat[i, 5]),
  mu = as.numeric(dat[i, 1:4]), sd = as.numeric(dat[1, 6]),
  labelnames = c("a",
                 "control", "treatment",
                 "b",
                 "male", "female"), 
  plot = FALSE)

result_exact <- ANOVA_exact(design_result_no_interaction)

dat$f_a[i] <- result_exact$main_results$cohen_f[1]
dat$f_b[i] <- result_exact$main_results$cohen_f[2]
dat$f_ab[i] <- result_exact$main_results$cohen_f[3]

dat$pwr_a[i] <- result_exact$main_results$power[1]
dat$pwr_b[i] <- result_exact$main_results$power[2]
dat$pwr_ab[i] <- result_exact$main_results$power[3]

}


d_a <- (a2_b1 + a2_b2)/2 - (a1_b1 + a1_b2)/2 
d_b <- (a2_b2 + a1_b2)/2 - (a1_b1 + a2_b1)/2
d_ab <- round(((a2_b2 - a1_b2) - (a2_b1 - a1_b1))/2)

dat <- dat %>%
        mutate(d_a = d_a, 
               d_b = d_b, 
               d_ab = d_ab)


ntotal_main_a <- vector(length = length(d_a))
ntotal_interaction_ab <- vector(length = length(d_ab))


for (i in 1:length(d_ab)) {

  ntotal_main_a[i] <- calculate_ntotal_main(d_a = d_a[i])
  ntotal_interaction_ab[i] <- calculate_ntotal_interaction(d_ab = d_ab[i])
}

dat <- dat %>%
        mutate(ntotal_main_a = ntotal_main_a, 
               ntotal_interaction_ab = ntotal_interaction_ab) 

dat <- dat %>%
        mutate(log_ntotal_main_a = log10(ntotal_main_a))


design_result_no_interaction <- ANOVA_design(
  design = "2b*2b", n = as.numeric(dat[5, 5]),
  mu = as.numeric(dat[5, 1:4]), sd = as.numeric(dat[5, 6]),
  labelnames = c("intervention",
                 "control", "treatment",
                 "sex",
                 "male", "female"), 
  plot = TRUE)

image_no_interaction <- design_result_no_interaction$meansplot

no_interaction_n <- ggplot(dat) + 
                      geom_point(aes(x = d_a, 
                                     y = log_ntotal_main_a, 
                                     color = d_ab))
no_interaction_n

no_interaction_full <- grid.arrange(
  no_interaction_n, 
  image_no_interaction, ncol = 2, 
  top=textGrob("2x2 Factorial design sample size choice under no interaction",
               gp=gpar(fontsize=20,font=1))) 







# scenario: partially attenuated interaction 

design_result_partial_interaction <- ANOVA_design(
  design = "2b*2b", n = 10, 
  mu = c(0.5, 0.7, 1.1, 0.8), sd = 1, 
  labelnames = c("a",
                 "control", "treatment",
                 "b",
                 "male", "female"), 
  plot = TRUE)


image_partial_interaction <- design_result_partial_interaction$meansplot



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

