# FILE INFORMATION -------------------------------------------------------------

# Companion to:
#
# A Practical Guide to Estimating Risk and Prevalence Ratios in R
# Rachel Yorlets, Jason R. Gantenberg, Youjin Lee


# SETUP ------------------------------------------------------------------------

if (! "pacman" %in% installed.packages()) install.packages("pacman")

pacman::p_load(magrittr, logisticRR, boot)

# NHEFS data
nhefs <- read.csv2("nhefs.csv", sep = ",")


# INDIRECT ESTIMATION: METHOD ONE - MANUAL CALCULATION -------------------------

# Step 1. Fit logistic regression for relationship between receipt of heart
#         medication in 1971 (weakheart) and subsequent mortality (death)

fit1 <- glm(death ~ weakheart, data = nhefs, family = binomial())

# Step 2. Predict the expected probability of death for each weakheart group.

## Pr(death = 1 | weakheart = 1), risk ratio numerator
prd1_wh1 <- predict(fit1,
                    newdata = data.frame(weakheart = 1),
                    type = "response")

## Pr(death = 1 | weakheart = 0), risk ratio denominator
prd1_wh0 <- predict(fit1,
                    newdata = data.frame(weakheart = 0),
                    type = "response")

## calculate the risk ratio
rr1 <- prd1_wh1 / prd1_wh0

## print results to console
cat("Marginal Pr(death = 1) = ", mean(nhefs$death), "\n",
    "Predicted Pr(death = 1 | weakheart = 1) = ", prd1_wh1, "\n",
    "Predicted Pr(death = 1 | weakheart = 0) = ", prd1_wh0, "\n",
    "Risk ratio = ", rr1, "\n",
    "Odds ratio = ", exp(coef(fit1)[2]), "\n",
    sep = "")

# Step 3. Estimate 95% confidence limits for the risk ratio via non-parametric
#         boostrapping.

## -- 3a. Sample 2,999 bootstrap replicates (with replacement) and record
##        which observations in the original data are members of each replicate.
data_n <- nrow(nhefs)

set.seed(1971) # set random seed

B <- 2999
replist <- lapply(
  1:B,
  do.call,
  what = "sample",
  args = list(
    x = seq_len(data_n),
    size = data_n,
    replace = TRUE
  )
)

### check to make sure all vectors are of the expected length
### evaluation should return TRUE
all(sapply(replist, length) == data_n)

## -- 3b. Express steps 1 and 2 in a function that we can evaluate repeatedly,
##        within each boostrap replicate.
estimate_risk_ratio <- function(dat = nhefs, x) {
  fit <- glm(death ~ weakheart, data = dat[x, ], family = binomial())
  rrnum <- predict(fit, newdata = data.frame(weakheart = 1), type = "response")
  rrden <- predict(fit, newdata = data.frame(weakheart = 0), type = "response")
  rr_est <- rrnum / rrden
  return(rr_est)
}


## -- 3c. Run analysis in step 2 within each bootstrap replicate and extract
##        a vector of 999 risk ratio estimates (i.e., the bootstrap
##        distribution).
rrboot <- sapply(replist, function(.x) estimate_risk_ratio(x = .x)) %>% unname()

### summarize bootstrapped distribution of risk ratios
summary(rrboot)
hist(rrboot, breaks = 20)

## -- 3d. Calculate the 95% confidence limits for the risk ratio estimate.

### Percentile method (non-parametric):
###   take 2.5% and 97.5% quantiles of bootstrapped distributions
###   WARNING: may be invalid in many situations, particularly with
###            small sample sizes
bootcl_percentile <- quantile(rrboot, c(0.025, 0.975)) %>% round(digits = 3)

### Bias-corrected and accelerated (BCa) percentile method (non-parametric):
###   calculate two additional parameters (z0 and a) to adjust interval
###   estimate so that it has better coverage properties
###   NOTE: preferred over the percentile method and likely a better choice
###         for a general-purpose confidence interval estimate

### z0 and a estimated as described in:
###   Efron B. and Tibshirani R.J. An Introduction to the Bootstrap.
###   CRC Press, 1993: pgs. 185-186.

### calculate z0, bias correction
z0 <- qnorm(sum(rrboot < rr1) / B)

### calculate a, acceleration
theta_jack <- sapply(
  seq_len(nrow(nhefs)),
  function(.x) estimate_risk_ratio(dat = nhefs, x = seq_len(nrow(nhefs))[-.x])
) %>% unname()

anum <- sum((mean(theta_jack) - theta_jack)^3) # a numerator
aden <- (6 * sum((mean(theta_jack) - theta_jack)^2))^(3/2)
a <- anum / aden

### bias-corrected quantile for lower limit
a1 <- pnorm(z0 + ((z0 + qnorm(0.025)) /
                  (1 - (a * (z0 + qnorm(0.025))))))

### bias-corrected quantile for upper limit
a2 <- pnorm(z0 + ((z0 + qnorm(1 - 0.025)) /
                  (1 - (a * (z0 + qnorm(1 - 0.025))))))

bootcl_bca <- quantile(rrboot, c(a1, a2)) %>% round(digits = 3)


rr1_dig3 <- round(rr1, digits = 3)

manual_calcs <- function() {
  cat("95% confidence limits: \n",
    rep("-", 30), "\n",
    "Percentile method: ",
    rr1_dig3, " (", bootcl_percentile[1], ", ", bootcl_percentile[2], ")", "\n",
    "BCa percentile method: ",
    rr1_dig3, " (", bootcl_bca[1], ", ", bootcl_bca[2], ")", "\n",
    sep = "")
}

manual_calcs()


# INDIRECT ESTIMATION: METHOD TWO - THE BOOT PACKAGE ---------------------------

## We can also use the 'boot' package, which has two main advantages over the
## code in Method One:
##
##   1) It has built-in support for parallel processing, which we'd have to
##      code ourselves and omitted for simplicity in the prior section. Using
##      multiple CPUs to run parallel analyses within bootstrap replicates
##      can speed up estimation of the bootstrap distribution significantly,
##      particularly within large datasets (e.g., those with tens of thousands
##      to millions of rows).
##
##   2) The 'boot' package implements several methods for obtaining bootstrapped
##      confidence limits via simple arguments to its primary function.

## Step 1. Specify a function that estimates and returns the risk ratio.

### Here we can reuse the estimate_risk_ratio() function we created during
### Method One. We ask boot::boot() to split the process up into 4 "jobs"
### and run these subsets in parallel.
set.seed(31415)
rrboot_pkg <- boot::boot(
  data = nhefs[, c("seqn", "weakheart", "death")],
  statistic = estimate_risk_ratio,
  R = B,
  parallel = "multicore",
  ncpus = 4
)

## NOTE: The 'boot' package uses a slightly different calculation for
##       the acceleration parameter than the one we used when
##       calculating the BCa intervals manually. Refer to the boot()
##       package documentation of empinf() for default settings
##       regarding BCa interval estimation.
rrbootcl_pkg <- boot::boot.ci(rrboot_pkg,
  conf = 0.95,
  type = c("perc", "bca")
)

rrbootcl_pkg
manual_calcs()


# INDIRECT ESTIMATION: METHOD THREE - THE logisticRR PACKAGE -------------------

rr_lrr <- logisticRR::logisticRR(death ~ weakheart,
                                 data = nhefs,
                                 boot = TRUE,
                                 n.boot = B)
