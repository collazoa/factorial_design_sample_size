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

## checking the classification above with graphs
  
report3 = report2 %>% select(c(a1_b1, a1_b2, a2_b1, a2_b2, type_int)) %>% mutate(id=c(1:nrow(report2)))
report3 = reshape2::melt(report3, id=c("id", "type_int"))
report3 = report3 %>% mutate(group_x = 
                               if_else(grepl("a1", variable, ignore.case = T)==TRUE, "control", "treated"))
report3 = report3 %>% mutate(group_c = 
                               if_else(grepl("b1", variable, ignore.case = T)==TRUE, "male", "female"))


ggplot(report3 %>% filter(id==1), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report3$type_int)

report3 %>% summarise(max(id))

# because there are 13750 scenarios to check, I propose checking random samples to see if any error can be detected

set.seed(11)
s_list = sample(x = c(1:13750), size = 10, replace = F)

ggplot(report3 %>% filter(id==s_list[1]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report3[s_list[1],2])

ggplot(report3 %>% filter(id==s_list[2]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report3[s_list[2],2])

ggplot(report3 %>% filter(id==s_list[3]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report3[s_list[3],2])

ggplot(report3 %>% filter(id==s_list[4]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report3[s_list[4],2])

ggplot(report3 %>% filter(id==s_list[5]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report3[s_list[5],2])

ggplot(report3 %>% filter(id==s_list[6]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report3[s_list[6],2])

ggplot(report3 %>% filter(id==s_list[7]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report3[s_list[7],2])

ggplot(report3 %>% filter(id==s_list[8]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report3[s_list[8],2])

ggplot(report3 %>% filter(id==s_list[9]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report3[s_list[9],2])

ggplot(report3 %>% filter(id==s_list[10]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report3[s_list[10],2])

# in this sample there are already two cases classified as partially attenuated that should have been fully attenuated

## next attempt, to base the classification on the difference between means

report4 = report %>% mutate(delta1 = a1_b1-a2_b1)
report4 = report4 %>% mutate(delta2 = a1_b2-a2_b2)

report4 = report4 %>% filter(f_ab!=0)

report4 = report4 %>% mutate(type_int = case_when(
  (delta1==delta2) ~ "no interaction",
  ((delta1!=delta2 & delta1<0 & delta2<0)|(delta1!=delta2 & delta1>0 & delta2>0)) ~ "partially attenuated",
  ((delta1==0 & delta2!=0)|(delta2==0 & delta1!=0)) ~ "fully attenuated",
  ((delta1<0 & delta2>0)|(delta1>0 & delta2<0)) ~ "reversed",
  TRUE ~ "MISSING"))

## checking again

report4 = report4 %>% mutate(id=c(1:nrow(report4)))

report5 = report4 %>% select(c(a1_b1, a1_b2, a2_b1, a2_b2, type_int, id))
report5 = reshape2::melt(report5, id=c("id", "type_int"))
report5 = report5 %>% mutate(group_x = 
                               if_else(grepl("a1", variable, ignore.case = T)==TRUE, "control", "treated"))
report5 = report5 %>% mutate(group_c = 
                               if_else(grepl("b1", variable, ignore.case = T)==TRUE, "male", "female"))


ggplot(report5 %>% filter(id==1), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report5[1,2])

report5 %>% summarise(max(id))

# same thing, with random sample

set.seed(11)
s_list = sample(x = c(1:13750), size = 10, replace = F)

ggplot(report5 %>% filter(id==s_list[1]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report5[s_list[1],2])

ggplot(report5 %>% filter(id==s_list[2]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report5[s_list[2],2])

ggplot(report5 %>% filter(id==s_list[3]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report5[s_list[3],2])

ggplot(report5 %>% filter(id==s_list[4]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report5[s_list[4],2])

ggplot(report5 %>% filter(id==s_list[5]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report5[s_list[5],2])

ggplot(report5 %>% filter(id==s_list[6]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report5[s_list[6],2])

ggplot(report5 %>% filter(id==s_list[7]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report5[s_list[7],2])

ggplot(report5 %>% filter(id==s_list[8]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report5[s_list[8],2])

ggplot(report5 %>% filter(id==s_list[9]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report5[s_list[9],2])

ggplot(report5 %>% filter(id==s_list[10]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report5[s_list[10],2])

# as all seem to have worked, just to make sure I'll check again

set.seed(23)
s_list = sample(x = c(1:13750), size = 10, replace = F)

ggplot(report5 %>% filter(id==s_list[1]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report5[s_list[1],2])

ggplot(report5 %>% filter(id==s_list[2]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report5[s_list[2],2])

ggplot(report5 %>% filter(id==s_list[3]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report5[s_list[3],2])

ggplot(report5 %>% filter(id==s_list[4]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report5[s_list[4],2])

ggplot(report5 %>% filter(id==s_list[5]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report5[s_list[5],2])

ggplot(report5 %>% filter(id==s_list[6]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report5[s_list[6],2])

ggplot(report5 %>% filter(id==s_list[7]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report5[s_list[7],2])

ggplot(report5 %>% filter(id==s_list[8]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report5[s_list[8],2])

ggplot(report5 %>% filter(id==s_list[9]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report5[s_list[9],2])

ggplot(report5 %>% filter(id==s_list[10]), aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  ggtitle(report5[s_list[10],2])

# all seem to be working, so we can ask the question again
# does the distribution of interaction effect sizes differ among the types of interactions?

main_graph = 
  ggplot(report4, aes(y=f_ab, x=type_int)) + 
  geom_violin(draw_quantiles = T)

report4 %>% group_by(type_int) %>% summarize(median(f_ab))
report4 %>% group_by(type_int) %>% summarize(n())

## adding panels

# fully attenuated example
f_data = report4 %>% filter(type_int=="fully attenuated")
f_data = slice_sample(f_data, n=1)

f_data = f_data %>% select(c(a1_b1, a1_b2, a2_b1, a2_b2, type_int, id))
f_data = reshape2::melt(f_data, id=c("id", "type_int"))
f_data = f_data %>% mutate(group_x = 
                               if_else(grepl("a1", variable, ignore.case = T)==TRUE, "control", "treated"))
f_data = f_data %>% mutate(group_c = 
                               if_else(grepl("b1", variable, ignore.case = T)==TRUE, "male", "female"))


f_example = 
  ggplot(f_data, aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none", plot.margin = unit(c(0,0,0,0), "cm"))


# partially attenuated example
p_data = report4 %>% filter(type_int=="partially attenuated")
p_data = slice_sample(p_data, n=1)

p_data = p_data %>% select(c(a1_b1, a1_b2, a2_b1, a2_b2, type_int, id))
p_data = reshape2::melt(p_data, id=c("id", "type_int"))
p_data = p_data %>% mutate(group_x = 
                             if_else(grepl("a1", variable, ignore.case = T)==TRUE, "control", "treated"))
p_data = p_data %>% mutate(group_c = 
                             if_else(grepl("b1", variable, ignore.case = T)==TRUE, "male", "female"))


p_example = 
  ggplot(p_data, aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none", plot.margin = unit(c(0,0,0,0), "cm"))

# reversed example
r_data = report4 %>% filter(type_int=="reversed")
r_data = slice_sample(r_data, n=1)

r_data = r_data %>% select(c(a1_b1, a1_b2, a2_b1, a2_b2, type_int, id))
r_data = reshape2::melt(r_data, id=c("id", "type_int"))
r_data = r_data %>% mutate(group_x = 
                             if_else(grepl("a1", variable, ignore.case = T)==TRUE, "control", "treated"))
r_data = r_data %>% mutate(group_c = 
                             if_else(grepl("b1", variable, ignore.case = T)==TRUE, "male", "female"))


r_example = 
  ggplot(r_data, aes(x = group_x, y=value, colour=group_c, group=group_c)) +
  geom_point() +
  geom_line() + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none", plot.margin = unit(c(0,0,0,0), "cm"))

## final figure

main_graph + 
  annotation_custom(ggplotGrob(f_example), xmin = 0.5, xmax = 1.5, 
                       ymin = 1.5, ymax = 2.1) +
  annotation_custom(ggplotGrob(p_example), xmin = 1.6, xmax = 2.6, 
                    ymin = 1.5, ymax = 2.1) +
  annotation_custom(ggplotGrob(r_example), xmin = 3.1, xmax = 4.1, 
                    ymin = 1.5, ymax = 2.1)

examples = cowplot::plot_grid(f_example, p_example, r_example, nrow = 1)

cowplot::plot_grid(main_graph, examples, nrow = 2, rel_heights = c(3,1))
