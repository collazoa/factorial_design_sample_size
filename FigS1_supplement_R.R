
# Fig.1S presents the results from a simulation of interaction effects in a 2x2 design 
# with intervention = {treatment, control} and animal sex = {male, female}. 
# For a continuous outcome variable, we assumed known normally distributed of group means 
# and standard deviations for control animals of both sexes. We than drew 1000 group means
# for male and female control animals from a normal distribution with mean = 1, standard
# deviation = 0.2. We simulated 1000 mean values for treated male and female animals ranging
# by drawing from an uniform distribution with the range 0.2 – 2.
# With the R-package “Superpower” (Lakens and Caldwell 2021) we calculated effect sizes for the 
# intervention-effect (factor A), sex-effect (factor B) and intervention-sex interaction, 
# setting the sample size from which the simulated group means were derived to 10 per group. 


source("./packages_interaction.R")

cols <- RColorBrewer::brewer.pal(6, "Dark2")
theme_set(theme_classic(base_size = 12))
set.seed(100)

B = 1000 
df <- tibble()

create_data <- function(mean_a1_b1, 
                        mean_a1_b2, 
                        min,
                        max,
                        sd = 0.2, 
                        B = 1000, 
                        n = 10) {
  index <- 1:length(B)
  a1_b1 <- rnorm(B, mean = mean_a1_b1, sd = sd)
  a1_b2 <- rnorm(B, mean = mean_a1_b2, sd = sd) 
  a2_b1 <- runif(B, min = min, max = max)
  a2_b2 <- runif(B, min = min, max = max)
  n <- c(rep(n,B))
  df <- tibble(index, a1_b1, a1_b2, a2_b1, a2_b2, n, sd)
}

df <- create_data(1,1, 0.2,2)%>%
  mutate(f_a = FALSE, 
         f_b = FALSE, 
         f_ab = FALSE, 
         pwr_a = FALSE, 
         pwr_b = FALSE, 
         pwr_ab = FALSE)


for (i in 1:nrow(df)) {
  design<- ANOVA_design(
    design = "2b*2b", n = df$n[i],
    mu = as.numeric(df[i, 2:5]), sd = df$sd[i],
    labelnames = c("a",
                   "control", "treatment",
                   "b",
                   "male", "female"), 
    plot = FALSE)
  
  result_exact <- ANOVA_exact(design)
  
  df$f_a[i] <- result_exact$main_results$cohen_f[1]
  df$f_b[i] <- result_exact$main_results$cohen_f[2]
  df$f_ab[i] <- result_exact$main_results$cohen_f[3]
  
  df$pwr_a[i] <- result_exact$main_results$power[1]
  df$pwr_b[i] <- result_exact$main_results$power[2]
  df$pwr_ab[i] <- result_exact$main_results$power[3]
}


df <- df %>%
  mutate(
    effect_b1 = a2_b1 - a1_b1, 
    effect_b2 = a2_b2 - a1_b2
  )

df <- df %>%
  mutate(
    type_int = ifelse(effect_b1 == effect_b2, 
                      "no_interaction", 
                      ifelse(effect_b1 > 0 & effect_b2 > 0 |
                               effect_b1 < 0 & effect_b2 < 0, 
                             "attenuated interaction", 
                             ifelse(effect_b1 > 0 & effect_b2 < 0 |
                                      effect_b1 < 0 & effect_b2 > 0, 
                                    "cross-over interaction", FALSE)))) 



# visualization 

figa <- ggplot(df) + geom_violin(aes(x = type_int, 
                                     y = f_a, 
                                     fill = type_int))+ 
  labs(title = "C: interaction shapes and main effect of factor A", 
       y = "effect size of factor A", 
       x = "")+
  scale_fill_manual(values = c(cols[1], cols[4]), 
                    labels = c("attenuated interaction", 
                               "cross-over interaction"))+
  theme(legend.position = "none", 
        plot.title = element_text(face="bold"))

figa


figb <- ggplot(df) + geom_violin(aes(x = as.factor(type_int), 
                                     y = f_b, 
                                     fill = factor(type_int)))+ 
  labs(title = "B: interaction shapes and main effect of factor B", 
       y = "effect size of factor B", 
       x = "")+
  scale_fill_manual(values = c(cols[1], cols[4]), 
                    labels = c("attenuated interaction", 
                               "cross-over interaction"))+
  theme(legend.position = "none", 
        plot.title = element_text(face="bold"))
figb


figab <- ggplot(df) + 
  geom_violin(aes(x = as.factor(type_int), 
                  y = f_ab, 
                  fill = factor(type_int)))+ 
  labs(title = "A: interaction shapes and interaction effect size", 
       y = "interaction effect size", 
       x = "")+
  scale_fill_manual(values = c(cols[1], cols[4]), 
                    labels = c("attenuated interaction", 
                               "cross-over interaction"))+
  theme(legend.position = "none", 
        plot.title = element_text(face="bold"))
figab  


fig4 <- grid.arrange(
  figab,
  figb, 
  figa,
  nrow = 1, ncol = 3, 
  top=textGrob("Interaction shape and effect sizes", 
               gp=gpar(fontsize=16)) 
)

