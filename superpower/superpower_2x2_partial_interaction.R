source("./packages_interaction.R")
source("./superpower/functions_superpower.R")

# scenario: partially attenuated interaction 

index <- 1:20
a1_b1 <- rep(0.5, 20) 
a1_b2 <- rep(0.7, 20) 
a2_b1 <- rep(seq(from = 0.6, to = 1.6, length.out = 10),2)
a2_b2 <- rep(seq(from = 0.9,to = 2.5, length.out = 10),2) 
n <- c(rep(10,10), rep(20,10))
sd <- rep(1,20)

dat <- tibble(index = index,
              a1_b1 = a1_b1, 
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
    design = "2b*2b", n = as.numeric(dat[i, 6]),
    mu = as.numeric(dat[i, 2:5]), sd = as.numeric(dat[i, 7]),
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
d_ab <- ((a2_b2 - a1_b2) - (a2_b1 - a1_b1))/2

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










# visualizing an example for partial interaction 


design_result_partial_interaction <- ANOVA_design(
  design = "2b*2b", n = 10, 
  mu = as.numeric(dat[10, 2:5]), sd = 1, 
  labelnames = c("a",
                 "control", "treatment",
                 "b",
                 "male", "female"), 
  plot = TRUE)



image_partial_interaction <- design_result_partial_interaction$meansplot

image_partial_interaction

partial_interaction_n <- ggplot(dat) + 
  geom_point(aes(x = d_a, 
                 y = log_ntotal_main_a, 
                 color = d_ab))
partial_interaction_n


partial_interaction_full <- grid.arrange(
  partial_interaction_n, 
  image_partial_interaction, ncol = 2, 
  top=textGrob("2x2 Factorial design sample size choice under no interaction",
               gp=gpar(fontsize=20,font=1))) 










