parent_wd <- getwd()
# bnch <- qs::qread(file.path(parent_wd, "g_results", "Laptop 1", paste0("combined_benchmarks", ".qs")))
bnch <- qs::qread(file.path(parent_wd, folder, "g_results", "Server 1", paste0("combined_benchmarks", ".qs")))
# bnch <- rbindlist(lapply(bnch, function(x) x[, mem_alloc := as.numeric(mem_alloc)]))

bnch$complete_label <- factor(bnch$complete_label)
bnch$algo <- factor(bnch$algo, levels = c("Naive", "Hash", "Threshold"))
bnch$exp <- as.integer(bnch$exp)
bnch$cm <- bnch$exp * (bnch$cm + 1)
bnch$mem_alloc <- bnch$mem_alloc / 1024 ^ 2
bnch$threshold_to_use <- factor(bnch$threshold_to_use)
bnch[, secondary_memory_usage := (size_intermediate + size_output) / 1024]

bnch[, match_vars_label := data.table::fcase(match_vars_label == "Mat1", "Age exact + Uniform",
                                             match_vars_label == "Mat2", "Age +=1 + Uniform",
                                             match_vars_label == "Mat3", "Age exact + Normal")]
bnch[, match_vars_label := factor(match_vars_label, levels = c("Age exact + Uniform", "Age +=1 + Uniform",
                                                               "Age exact + Normal"))]

bnch[, samp_schema_label := data.table::fcase(samp_schema_label == "S1", "Sample 1 cm to each exposed",
                                              samp_schema_label == "SN", "Take all cm to each exposed",
                                              samp_schema_label == "S1B10SExp", "Sample 1 cm to each exposed and take 10 bootstrap sampling from the exposed",
                                              samp_schema_label == "S1B10SUoO", "Sample 1 cm to each exposed and take 10 bootstrap sampling from all UoO")]
bnch[, samp_schema_label := factor(samp_schema_label,
                                   levels = c("Sample 1 cm to each exposed", "Take all cm to each exposed",
                                              "Sample 1 cm to each exposed and take 10 bootstrap sampling from the exposed",
                                              "Sample 1 cm to each exposed and take 10 bootstrap sampling from all UoO"))]

bnch[, complete_label := gsub("C1", "Cdef", complete_label)]
bnch[, complete_label := as.factor(complete_label)]

bnch[, threshold_to_use := factor(threshold_to_use, levels = c(NA, 1, "half", "max", "double"))]
bnch[is.na(threshold_to_use), threshold_to_use := "no threshold"]

bnch[, time_num := as.numeric(bnch$time)]
setorder(bnch, time_num)
bnch$experiment_id <- 1:nrow(bnch)

test <- mgcv::gamm(time_num ~ s(exp, k = 9, bs="tp") + s(cm, bs="tp") + s(threshold_used, bs="tp") +
                     s(threshold_to_use, bs = "sz") + s(match_vars_label, bs = "sz") + s(samp_schema_label, bs = "sz")
                   + s(complete_label, bs = "re")
                   , data = bnch, method = "REML")

summary(test$gam)
plot(test$gam, pages = 1)
plot(test$gam, select = 1, residuals = T, ylim = c(0, 2500))
plot(test$gam, select = 2, residuals = T, ylim = c(0, 70000))
plot(test$gam, select = 3, residuals = T, ylim = c(-20000, 60000))
plot(test$gam, select = 4, residuals = T)








MM_Intercept <- lme4::lmer(formula = log(time) ~ 1 + (1|complete_label),
                            data = bnch)

summary(MM_Intercept)
performance::icc(MM_Intercept)

MM_full <- lme4::lmer(formula = time_inv_b ~ exp + cm + threshold_to_use + match_vars_label + samp_schema_label + cores_label + (1|complete_label),
                            data = bnch)

summary(MM_full)
performance::icc(MM_full)

qqnorm(resid(MM_full))
qqline(resid(MM_full))

car::powerTransform(bnch$time + 1, family="bcPower")
plot(sort(bnch$time))
plot(-1*sort(bnch$time + 1)^(-1/2))

test <- as.integer(bnch$time)/max(as.integer(bnch$time))
EnvStats::ebeta(test)
EnvStats::ebeta(bnch$time_std)
EnvStats::ebeta(test, method="mme")
EnvStats::ebeta(bnch$time_std, method="mme")

qqnorm(resid(MM_full_2))
qqline(resid(MM_full_2))

bnch[, time_1 := -1*sort(bnch$time + 1)^(-1/2)]
bnch[, time_std := as.numeric(bnch$time)/(max(as.integer(bnch$time) + 1))]
bnch[, time_inv_b := qbeta(as.numeric(bnch$time), 0.0065, 3.21)]
bnch[, time_inv_b_2 := qbeta(time_std, 1, 6531)]
bnch[, time_inv_b_3 := qbeta(time_std, 0.095844, 500)]

stats::ks.test(bnch$time_std, pcauchy, location = 0, scale = 1)

plot(sort(bnch$time_inv_b))
plot(sort(bnch$time_inv_b_2))
plot(sort(bnch$time_inv_b_3))
qqnorm(bnch$time_inv_b)
qqline(bnch$time_inv_b)
qqnorm(bnch$time_inv_b_2)
qqline(bnch$time_inv_b_2)
qqnorm(bnch$time_inv_b_3)
qqline(bnch$time_inv_b_3)

MM_exp <- betareg::betareg(formula = time_std ~ exp + (1|complete_label),
                         data = bnch, link = "log")

qqnorm(resid(MM_exp))
qqline(resid(MM_exp))

test <- mgcv::betar()
test1 <- glmmTMB::beta_family()

library(glmmTMB)
MM_beta_exp_1 <- glmmTMB::glmmTMB(formula = time_std ~ exp + (1|complete_label),
                       data = bnch, family = glmmTMB::beta_family(link = "logit"))

qqnorm(resid(MM_beta_exp_1))
qqline(resid(MM_beta_exp_1))

MM_beta_exp_2 <- glmmTMB::glmmTMB(formula = time_std ~ exp + (1|complete_label),
                                data = bnch, family = glmmTMB::beta_family(link = "cloglog"))

qqnorm(resid(MM_beta_exp_2))
qqline(resid(MM_beta_exp_2))

MM_beta_exp_3 <- glmmTMB::glmmTMB(formula = time_std ~ exp + (1|complete_label),
                                  data = bnch, family = glmmTMB::beta_family(link = "inverse"))

qqnorm(resid(MM_beta_exp_3))
qqline(resid(MM_beta_exp_3))

MM_exp <- lme4::lmer(formula = time_inv_b_3 ~ exp + (1|complete_label),
                     data = bnch)

qqnorm(resid(MM_exp))
qqline(resid(MM_exp))

MM_cm <- lme4::lmer(formula = log(time) ~ cm + (1|complete_label),
                    data = bnch)

qqnorm(resid(MM_cm))
qqline(resid(MM_cm))

MM_cm <- lme4::lmer(formula = log(time) ~ cm + (1|complete_label),
                    data = bnch)

qqnorm(resid(MM_cm))
qqline(resid(MM_cm))










jtools::summ(MM_full, exp = T)

# m_PWV_CR_log<-lmer(log(PWV_CR)~as.factor(TX_GROUP)*visit+log(PWV_CR_BL)+AGE+sex+race+smk+family+BMI_BL+(1|ID),data=p32data_PWV_CR) 
# summary(m_PWV_CR_log)
MM_full_log_est <- emmeans::emmeans(MM_full, ~ exp | cm , type = "response", digits = 4, pbkrtest.limit = 26960)



MM_full_cauchy <- heavy::heavyLme(fixed = log(time) ~ exp + cm + threshold_to_use + match_vars_label + samp_schema_label + cores_label,
                                  random = ~ 1, groups = ~ complete_label, data = bnch, family = heavy::Cauchy())

qqnorm(MM_full_cauchy$Resid$conditional)
qqline(MM_full_cauchy$Resid$conditional)














MM_full_t <- heavy::heavyLme(fixed = log(time) ~ exp + cm + threshold_to_use + match_vars_label + samp_schema_label + cores_label,
                             random = ~ 1, groups = ~ complete_label, data = bnch, family = heavy::Student(df = 1))

qqnorm(MM_full_t$Resid$conditional)
qqline(MM_full_t$Resid$conditional)

MM_full_t1 <- heavy::heavyLme(fixed = log(time) ~ exp + cm + threshold_to_use + match_vars_label + samp_schema_label + cores_label,
                             random = ~ 1, groups = ~ complete_label, data = bnch, family = heavy::Student(df = 40))

qqnorm(MM_full_t1$Resid$conditional)
qqline(MM_full_t1$Resid$conditional)

