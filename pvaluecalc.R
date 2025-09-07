# test statistics and p values of distributions
# Type I Error: Rejecting a true H0
# Type II Error: Accepting a false H0

#-------------------------------------------------------------------------------

# One-Sample Parametric Tests

# MEAN

# (sample_mean - theoretical_mean)/SE_mean
# SE_mean = corr_sd/sqrt(n)
# p_value <- 1 - pt(test_stat, df = n - 1) #1- nem mindig kell

# PROPOTION

# Test Stat = (p - P_0)/SE_P
# SE_P <- SE_P = sqrt(P_0*(1-P_0)/n)
# p: observed proportion in the sample
# pnorm(test_stat_p) vagy 1-pnorm()

# in case of two-sided test
# pnorm(-abs(test_stat_p))*2

# STANDARD DEVIATION

# Sigma_0: theoretical value of the standard deviation

# Sigma_0 <- 15
# test_stat_sd <- (n-1)*s^2/Sigma_0^2
# p_value <- 1-pchisq(test_stat_sd, df=(n-1)) vagy 1- nélkül ugyanez

# in case of two-sided test
# (1-pchisq(test_stat_sd, df=(n-1)))*2 

#összefoglalva:
#         if test stat < (n-1) --» (pchisq(test_stat_sd, df=(n-1)))
#         if test stat > (n-1) --» (1-pchisq(test_stat_sd, df=(n-1)))
#         if test stat = (n-1) --» (pchisq(test_stat_sd, df=(n-1)))*2

#-------------------------------------------------------------------------------

# Two-Sample Parametric Tests

# MEAN

# test_stat = (observed_diff - delta_0)/SE_Unified
# SE_Unified = sqrt(SE_1^2 + SE_2^2)

# SE for the mean = corr st dev / sqrt(n)

#sd_NB <- sd(ESS$Education_Years[ESS$ScientistsDecievePublic=="No"],
#            na.rm = TRUE)
#sd_B <- sd(ESS$Education_Years[ESS$ScientistsDecievePublic=="Yes"],
#           na.rm = TRUE)

#n_NB <- sum(!is.na(ESS$Education_Years[ESS$ScientistsDecievePublic=="No"])) 
#n_B <- sum(!is.na(ESS$Education_Years[ESS$ScientistsDecievePublic=="Yes"]))

#SE_Unified <- sqrt(sd_NB^2/n_NB + sd_B^2/n_B)

#test_stat2_sample_mean <- (observed_diff - delta_0) / SE_Unified

#p_value <- 1-pt(test_stat2_sample_mean, df = n_NB + n_B) vagy 1-nélkül
# ÁTNÉZNI A df-et



# PROPOTION

n_B <- sum(ESS$SecretGroupInfluenceWorldPol == "Yes")
n_NB <- sum(ESS$SecretGroupInfluenceWorldPol == "No")

# k_i --> number of favorable cases in the proportion for group 'i'

#k_B <- sum(ESS$SecretGroupInfluenceWorldPol=="Yes" &
#             ESS$TrustInParlament=="Yes")
#k_NB <- sum(ESS$SecretGroupInfluenceWorldPol=="No" &
#              ESS$TrustInParlament=="Yes")

#p_B <- k_B/n_B # 11.1%
#p_NB <- k_NB/n_NB # 14.6%

#delta_0 <- 0.02
#observed_diff_p <- p_B - p_NB

# test_stat = (observed_diff_p - delta_0)/SE_Unified_p
# SE_Unified_p = sqrt(SE_1^2 + SE_2^2)
# SE_p = sqrt(p*(1-p)/n)

#test_stat_2sample_p <- (observed_diff_p - delta_0)/sqrt(
#  p_B*(1-p_B)/n_B + p_NB*(1-p_NB)/n_NB
#)

#p_value_prop <- 1-pnorm(test_stat_2sample_p) vagy 1- nélkül




