# functions to calculate total N with power = 80% and alpha-level = 0.05
# for the main effect a (treatment vs. control)

calculate_ntotal_interaction <- function(d_ab, power = 0.8, alpha = 0.05) {
  numerator <- (4 * (qnorm(1 - alpha/2)+qnorm(power))^2)
  ntotal <- numerator/(d_ab^2)
}

calculate_ntotal_main <- function(d_a, power = 0.8, alpha = 0.05) {
  numerator <- (4 * (qnorm(1 - alpha/2)+qnorm(power))^2)
  ntotal <- numerator/(d_a^2)
}
