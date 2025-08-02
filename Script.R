####################
###### IHA 5 #######
####################

### PRELIMINARY ANALYSES ###

# load packages
library(tidyverse)
library(car)
library(psych)
library(interactions)
library(caret)
library(emmeans)
library(rties)
library(brms)
library(bayestestR)
library(BayesFactor)
library(flextable)
library(stargazer)
library(sjPlot)

# scientific notation annoyed me
options(scipen = 9) 

# loading in data
expo <- read.csv("exposure_data.csv")

# look at the data
str(expo) # looks like mask_type should be a factor
summary(expo) # no missing data

# turning mask type into a factor (binary variable)
expo$mask_typeF <- factor(expo$mask_type, levels = c("New", "Standard"), labels = c("New", "Standard"))
str(expo) # it works!
expo2 <- subset(expo, select = c(exposure_level, performance, mask_typeF))

## descriptive statistics
# mask (binary variable)
mask_d <- describeBy(expo2$performance, group = expo2$mask_typeF, quant = c(0.25,0.75), mat = T)
mask_d
mask_d <- mask_d[,c(2,4,5,6,7,16,17,10,11)]
mask_d <- flextable(mask_d)
mask_d %>%
  save_as_docx(path = "IHA5_mask_desctab.docx")

# quantitative variable (exposure level)
ex_d <- describe(expo2$exposure_level, quant = c(.25,.75))
ex_d <- ex_d[,c(2,3,4,5,14,15,8,9)]
ex_d <- flextable(ex_d)
ex_d %>%
  save_as_docx(path = "IHA5_exposurelevel_desctab1.docx")

## Checking correlations/linearity/etc
histAll(expo2)
# exposure level is uniformly distributed while performance
# is bimodal. The non-normal distribution of the outcome
# might mean that we should choose a different likelihood when we do 
# Bayesian analysis.
hist(expo2$performance, xlab = "Performance (0 - 100)", main = "Distribution of Performance on Cognitive Exam")

plot(expo2$exposure_level, expo2$performance)
# linear and negative relationship (higher exposure = lower performance)

boxplot(performance ~ mask_typeF, data = expo2)
# new masks seem to be performing better (-ish), but
# 84 subjects wore the new mask while 116 wore the old mask 
# usually with smaller samples we expect more variability, but
# new mask group is less variable
# overall, seems like new masks are performing better!

# mean centering
expo2$exposure_level <- expo2$exposure_level - mean(expo2$exposure_level)
# even though having a 0ppm exposure level makes sense, I thought it would be more
# effective to center by the mean, since this study is assessing efficacy of masking for 
# employees in industrial environments with more HS exposure than most people.
# so knowing predicted performance for someone with no exposure isn't the most representative in this sample

### NHST Analysis ###

# fit linear models
lm1 <- lm(performance ~ exposure_level + mask_typeF, data = expo2)
lm2 <- lm(performance ~ exposure_level + mask_typeF + mask_typeF*exposure_level, data = expo2)

# significant?
summary(lm1) # model IS significant - F(2,197) = 293.3, p < 2.2e-16
summary(lm2) # model IS significant - F(3,196) = 381.5, p < 2.2e-16

# checking assumptions!

# normality of residuals? centered at zero?
hist(lm1$residuals) # slight right skew, means model is OVERPREDICTING 
  # by group
  qqPlot(lm1$residuals, groups = expo2$mask_typeF)
  # standard masks are more normally distributed
  # to be expected since they have a larger sample size
  # new masks have more variability @ left tail, but not really an issue
# TLDR residuals are (roughly)normally distributed and centered at zero!
  
hist(lm2$residuals) # roughly normal and centered at zero. very slight right skew (model overpredicting LESS than m1)
  # by group
  qqPlot(lm2$residuals, groups = expo2$mask_typeF)
  # these look way better - check by group
# TLDR residuals are normally distributed and centered at zero!
  
# constant variance of residuals?
car::residualPlots(lm1,
                     pch=20, col="pink",
                     fitted = T,
                     ask = F, layout = c(1,2),
                     tests = F, quadratic = F)
  # exposure level looks super uniform - satisfied!
  # mask type has constant variance , means of both are pretty close to zero (good!)
  # yikes! overall residuals are mountain-shaped - may be missing an effect
# TLDR: homoscedicity satisfied for exposure level and mask type, but overall residuals show a mountain-shape,
# Might affect standard errors.

car::residualPlots(lm2,
                   pch=20, col="pink",
                   fitted = T,
                   ask = F, layout = c(1,2),
                   tests = F, quadratic = F)
  # exposure level is super uniform - check!
  # mask type is good, and the means are closer to zero - yay! (outliers 181, 69)
  # overall residuals are super good and don't have that weird quadratic thing anymore
# TLDR: homoscedicity is satisfied for all vars

# interpret the models!!!
# MODEL 1

summary(lm1)
# intercept: for an individual with the new mask and avg exposure level, 
# expected performance on cognitive exam is 99.18% 
# exposure level IS a signficant predictor of performance (p < 2e-16)
# for every 1ppm increase in exposure to HS, model predicts a 0.55% decrease in cognitive performance
# controlling for mask type

# omnibus test for mask type
Anova(lm1, type = 'III') # signficant! p < 0.001, F(1, ? = 290.72
# mask type is also a significant predictor (p < 0.001)
# workers with the standard mask performed 15.20% worse than those using new mask on
# congitive performance test , controlling for exposure levels

# effect size estimates
# adjusted R-squared = 0.7461 = 75% 
# for mask type, partial cohen's d is needed

PartialEffectSizes(lm1)
# PARTIAL COHEN'S D FOR MASK TYPE: 2.429580 > 0.8 = large effect size
# SEMI-PARTIAL ETA SQUARED FOR EXPOSURE LEVELS: 0.4226415 - explains % unique variance

# MODEL 2

round(cbind(summary(lm2)$coef, confint.lm(lm2)),3)

# intercept = performance level w/new mask and average exposure level
# 90.253 (SE = 1.108, CI = [88.067, 92.439]) is the expected performance for workers w/avg exposure level wearing new mask
# conditional effect (main predictor) -0.221 (SE = 0.036, CI = [-0.292, -0.150]) is est. association between exposure level and performance wearing new mask
# conditional effect (mask_typeF) not significant
# interaction effect: amount which effect of exposure level on performance changes w/1 unit increase in moderating effect
# i.e. difference between new and standard mask groups
# standard mask type has slope 0.280 - 0.494 = -0.214 (est association between exposure level and performance wearing standard mask)

# decomposing model so we can interpret:
# interactions plot
apatheme <- theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='serif')) # setting up some of display options


fig1 <- interact_plot(lm2, pred = exposure_level, modx = mask_typeF,
                      modx.labels = c("New", "Standard"), 
                      x.label = 'Exposure Level (Grand-Mean Centered)', y.label = 'Performance',
                      colors = c('black', 'gray50'), line.thickness = 0.6, 
                      legend.main = 'Mask Type', interval = TRUE, int.width = .95) +
  apatheme

fig1 # see some interaction

# Save figure with high resolution:
ggsave(filename = "IHA5_NHST_moderation.png",
       plot = fig1,
       device = "png",
       width = 6,
       height = 4.5,
       units = "in",
       dpi = 500)

# probing simple slopes
interact <- emtrends(lm2, var = "exposure_level", ~ mask_typeF)
# var = main predictor variable in model 
# ~ .. = moderating variable

test(interact) # test of whether each simple slope is significantly different from 0
# for New masks: there is a significant, negative relationship between exposure level and performance (b = -0.221, SE = 0.0361, p < .0001)
# for standard masks: there is a significant, negative relationship between exposure level and performance (b = -0.779, SE = 0.0301, p < 0.0001)

interact
# CIs: (New Masks) (-0.292, -0.150)
# standard masks (-0.838, -0.719)

# have to make a cute table!
stargazer(lm1, lm2, ci = TRUE, intercept.bottom = FALSE, out = "C:/Users/surface/Desktop/Bayesian & Frequentist Stats/IHA5_lmtab.html")

PartialEffectSizes(lm2)
# PARTIAL COHEN'S D FOR MASK TYPE: 3.149506 > 0.8 = large effect size
# SEMI-PARTIAL ETA SQUARED FOR EXPOSURE LEVEL:  0.3375999 ~ uniquely explains 34% of variance in outcome

# COMPARISON: NHST

# anova to compare nested models using F-test
anova(lm1, lm2)
# moderation model explains significantly more variance  - F(1, 196) = 140.94
# and reduced RSS (7604.8 -> 4423.8)


# adjusted R-squared
summary(lm1)$adj.r.sq # 0.746
summary(lm2)$adj.r.sq # 0.852

# comparing models using 10-fold cross validation

# Define cross-validation method and number of folds:
train_control <- trainControl(method = "cv", number = 10)
# here we are setting up to implement 10-fold cross validation 

set.seed(456)

# Model 1: Main effects only
cv_m1 <- train(performance ~ exposure_level + mask_typeF, data = expo2,
               method = "lm", trControl = train_control)

# Model 2: With interaction
cv_m2 <- train(performance ~ exposure_level + mask_typeF + exposure_level * mask_typeF, data = expo2,
               method = "lm", trControl = train_control)


# Compare CV results
cv_m1$results
cv_m2$results


# Recall that RMSE and MAE are in the units of the DV:
summary(expo2$performance) # range of about 52.65 points
96.45 - 43.80

# Interpreting MAE (for main effects model):
# The main effects model is off by an average of 5.14 points, 
5.14 / 53
# or approximately 10% of the observed range of the outcome.
# Suggests modest to moderate predictive accuracy 

# Interpreting MAE (for moderation model):
# The interaction model is off by 3.77 points on average,
3.77/ 53
# which reflects 7% of the observed range of the outcome. 
# Suggests moderate predictive accuracy overall

# Comparing fit values: 
5.14 - 3.77
1.37/53
1.37/5.14
# MAE dropped by ~1.37 -> 3% improvement in average error
# RMSE also dropped -> indicating lower typical squared error

# MAE SD went from 0.9434 to 00.6214
# RMSE SD went from 0.9307 to 0.6358
# So the moderation model is more accurate on average and more consistent across folds.

# Including the interaction between exposure level and mask type resulted in a clear but
# modest improvement in predictive performance over the main effects model. 

# Both RMSE and MAE decreased slightly, and R2 increased substantially,
# indicating that the interaction explains more variance in performance

# The drop in MAE represents a reduction in average error, which supports a moderate level of predictive accuracy for both models. 

### BAYESIAN ANALYSIS ###

# using baye's factors to see if these models are favored over null
bf1 <- lmBF(performance ~ exposure_level + mask_typeF, data = expo2)
bf2 <- lmBF(performance ~ exposure_level + mask_typeF + exposure_level*mask_typeF, data = expo2)
bf1
bf2
# we can keep going, BF is huge for both (scientific notation this later)

hist(expo2$performance) # the distribution is bimodal (i'm guessing from the binary predictor)
# so let's just use a skew normal and hope it works!

## Main Effects Model 
b1Main <- brm(performance ~ 0 + Intercept + exposure_level + mask_typeF, data = expo2,
              family = "skew_normal", chains = 4, iter = 2000, seed = 321)
saveRDS(b1Main, file = "b1Main_IHA5.rds")


## Moderation Model 
b1Int <- brm(performance ~ 0 + Intercept + exposure_level + mask_typeF + exposure_level * mask_typeF, data = expo2,
             family = "skew_normal", chains = 4, iter = 2000, seed = 321)
saveRDS(b1Int, file = "b1Int_IHA5.rds")

# checking assumptions

# (1) did the models converge? Check ESS, Rhats, trace plots
summary(b1Main) # converged from Rhats, ESS
plot(b1Main) # trace plots are caterpillars
summary(b1Int) # rhats and ess good
plot(b1Int) # converged!

# (2) predictive accuracy w/posterior predictive checks

pp_check(b1Main, ndraws = 30) # bimodality not really captured by draws
pp_check(b1Int, ndraws = 30) # wow! much better fit

pp_check(b1Main, type = "hist", ndraws = 15, set.seed(293)) # unimodal
pp_check(b1Int, type = "hist", ndraws = 15, set.seed(293)) # bimodal
# m2 has much better predictive accuracy

# (3) normality of residuals, centered @ 0 - check!

# M1
pp_check(b1Main, type = "error_hist", ndraws = 15, set.seed(293))  # looks good
pp_check(b1Main, type="error_hist_grouped", ndraws=5, group = "mask_typeF", freq = T, set.seed(293))  # looks good

# M2
pp_check(b1Int, type = "error_hist", ndraws = 15, set.seed(293))  # looks good
pp_check(b1Int, type="error_hist_grouped", ndraws=5, group = "mask_typeF", freq = T, set.seed(293))  # looks good

# (4) constant variance of residuals

# M1
# by mask group - satisfied
ggplot(expo2, aes(x = mask_typeF, y = residuals(b1Main)[,1]))+
  geom_point(size=2)+geom_jitter(width=0.1)

plot(residuals(b1Main)) # overall, looks good

# M2
# by mask group - satisfied
ggplot(expo2, aes(x = mask_typeF, y = residuals(b1Int)[,1]))+
  geom_point(size=2)+geom_jitter(width=0.1)

plot(residuals(b1Int)) # overall, looks good

# OVERALL: residuals were checked and found to be normal, centered @ 0, w/ constant variance for both models

# interpret the Bayesian models!

summary(b1Main)
tab_model(b1Main, show.se = TRUE)
# est performance score for avg exposure level and new mask type is 98.67 (CI 96.79, 100.48), se = 0.94
# performance decreases by 0.54 pts for every 1 ppm increase in HS  (for new mask type) - CI(-0.60, -0.48), se = 0.03
# holding exposure level constant, those wearing standard mask have 14.71 pts lower (se = 0.91, ci(-16.47, -12.91)

summary(b1Int)
tab_model(b1Int, show.se = TRUE)
# est performance score for avg exposure and new mask type is 90.38
# 1ppm increase in exposure level -> 0.22 pt decrease in performance, for new mask type
# exposure level constant, standard mask type associated w/ 0.6 pt decrease in performance
# interaction effect: amount which effect of exposure level on performance changes w/1 unit increase in moderating effect

## PROBE Interaction: 
simSlopes <- emtrends(b1Int, var = "exposure_level", ~ mask_typeF)
summary(as.mcmc(simSlopes)) # HDI's test whether each simple slope is credibly different from 0 
# each simple slope is credibly different from zero! all effects are credible


### plotting HDIs
bfig1 <- interact_plot(b1Int, pred = exposure_level, modx = mask_typeF,
                       modx.labels = c("New", "Standard"), 
                       x.label = 'Exposure Level (Grand-Mean Centered)', y.label = 'Cognitive Performance',
                       colors = c('black', 'gray50'), line.thickness = 0.6, 
                       legend.main = 'Mask Type', geom = 'line', interval = TRUE, int.width = .95) + 
  apatheme 

bfig1

# effect sizes

# Cohen's d for mask type (categorical)
# main
summary(b1Main)$fixed[3,1]/summary(b1Main)$spec_pars[1,1] # -2.3157
# interaction
summary(b1Int)$fixed[3,1]/summary(b1Int)$spec_pars[1,1] # -3.138

# mask type has a LARGE effect size for both models

## Goal 3 & 4: Compare Models. How well will our model do in predicting new data?
bayes_R2(b1Main) # r2 = 0.738, se = 0.015, hdi[0.707 0.763]
bayes_R2(b1Int) # r2 = 0.85, se = 0.008, hdi[0.834, 0.864]

## Can also use Bayes Factors to compare other models
bf1/bf2 # tiny!
bf2/bf1 # 3.67 * 10^21 evidence is highly in favor of interaction model!

## use PSIS Leave One Out (LOO) Cross-Validation
b1Main <- add_criterion(b1Main, criterion = "loo")
b1Int <- add_criterion(b1Int, criterion = "loo")

# Check diagnostics first (just focus on Pareto k estimates): 
loo1 <- b1Main$criteria$loo$diagnostics$pareto_k
loo2 <- b1Int$criteria$loo$diagnostics$pareto_k

table(cut(loo1, breaks = c(-Inf, 0.5, 0.7, 1, Inf), 
          labels = c("good (<=0.5)", "ok (0.5–0.7)", "bad (0.7–1)", "very bad (>1)")))

table(cut(loo2, breaks = c(-Inf, 0.5, 0.7, 1, Inf), 
          labels = c("good (<=0.5)", "ok (0.5–0.7)", "bad (0.7–1)", "very bad (>1)")))
# all are good! we can LOO now!!

loo_compare(b1Main, b1Int, criterion = "loo")
# second model is notably worse than the first in terms of predictive accuracy 
#50.8/8.7 = 5.84 > 2
# elpd_diff = -50.8, se_diff = 8.7

# ROPEs and PD

# interaction
p_d <- pd(b1Int); p_d
plot(p_d)

r <- rope(b1Int); r
plot(r)

# main
p_dm <- pd(b1Main); p_dm
plot(p_dm)

rm <- rope(b1Main); r
plot(rm)
# need to consider adjusting ROPE range - 1ppm exposure could be significant
# 100% of HDI fell within ROPE range, indicating may not be practically significant
