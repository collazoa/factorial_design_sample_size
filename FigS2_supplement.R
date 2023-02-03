# Visualization for 4 different types of interaction with
# example data sets. 
# A: partially attenuated interaction, B: fully attenuated interaction
# C: cross-over interaction, D: no interaction 

source("./packages_interaction.R")

# A: partially attenuated interaction 

design_result_partial_interaction <- ANOVA_design(
  design = "2b*2b", n = 10, 
  mu = c(0.5, 0.7, 1.1, 0.8), sd = 1, 
  labelnames = c("a",
                 "control", "treatment",
                 "b",
                 "male", "female"), 
  plot = TRUE)


fig_partial <- design_result_partial_interaction$meansplot + 
  labs(title = "A: partially attenuated interaction")+
  theme_classic()

fig_partial

# B: fully attenuated interaction 

design_result_fully_interaction <- ANOVA_design(
  design = "2b*2b", n = 10, 
  mu = c(0.1, 0.2, 0.6, 0.2), sd = 0.5, 
  labelnames = c("a",
                 "control", "treatment",
                 "b",
                 "male", "female"), 
  plot = TRUE)

fig_full <- design_result_fully_interaction$meansplot + 
  labs(title = "B: fully attenuated interaction")+ 
  theme_classic()
fig_full

ANOVA_exact(design_result_fully_interaction)

# C: cross-over interaction 

design_result_crossover_interaction <- ANOVA_design(
  design = "2b*2b", n = 10, 
  mu = c(0.3, 0.2, 0.1, 0.7), sd = 0.5, 
  labelnames = c("a",
                 "control", "treatment",
                 "b",
                 "male", "female"), 
  plot = TRUE)
ANOVA_exact(design_result_crossover_interaction) 


fig_cross <- design_result_crossover_interaction$meansplot+ 
  labs(title = "C: cross-over interaction")+ 
  theme_classic()
fig_cross

# D: no interaction  

design_result_no_interaction <- ANOVA_design(
  design = "2b*2b", n = 10, 
  mu = c(0.3, 0.2, 0.6, 0.5), sd = 0.5, 
  labelnames = c("a",
                 "control", "treatment",
                 "b",
                 "male", "female"), 
  plot = TRUE)
ANOVA_exact(design_result_no_interaction) 
fig_no <- design_result_no_interaction$meansplot+ 
  labs(title = "D: no interaction")+ 
  theme_classic()
fig_no


fig_examples <- grid.arrange( 
  fig_partial, 
  fig_full, 
  fig_cross, 
  fig_no, 
  nrow = 2, ncol = 2)


# save 
ggsave("FigS2_examples.png", plot = fig_examples)



# other visualization tools: adding A, B, C 
grob <- grobTree(textGrob("A", x=0.97,  y=0.97, hjust=0,
                          gp=gpar(col="black", fontsize=16, fontface="bold")))

