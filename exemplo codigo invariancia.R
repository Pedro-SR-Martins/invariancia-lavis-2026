
library(lavaan)

set.seed(2026)

popModel <- '
  # ---- Loadings ----
  BES =~ 1*q1 + 0.75*q2 + c(0.80, 0.40)*q3 + 0.70*q4 + 0.75*q5
  ES  =~ 1*q6 + 0.75*q7 + c(0.80, 0.40)*q8 + 0.70*q9 + 0.75*q10
 
  # ---- Factor (co)variances: fixed at 1, moderate correlation ----
  BES ~~ 1*BES
  ES  ~~ 1*ES
  BES ~~ 0.30*ES
 
  # ---- Latent means: reference group (Urbano) = 0; Rural differs ----
  # (Only interpretable once at least partial scalar invariance holds)
  BES ~ c(0, 0.30)*1
  ES  ~ c(0, -0.20)*1
 
  # ---- Thresholds (5 categories -> 4 thresholds per item) ----
  # Invariant items: same thresholds in both groups
  q1  | -1.5*t1 + -0.5*t2 + 0.5*t3 + 1.5*t4
  q2  | -1.5*t1 + -0.5*t2 + 0.5*t3 + 1.5*t4
  q3  | -1.5*t1 + -0.5*t2 + 0.5*t3 + 1.5*t4
  q5  | -1.5*t1 + -0.5*t2 + 0.5*t3 + 1.5*t4
  q6  | -1.5*t1 + -0.5*t2 + 0.5*t3 + 1.5*t4
  q7  | -1.5*t1 + -0.5*t2 + 0.5*t3 + 1.5*t4
  q8  | -1.5*t1 + -0.5*t2 + 0.5*t3 + 1.5*t4
  q10 | -1.5*t1 + -0.5*t2 + 0.5*t3 + 1.5*t4
 
  # Threshold-noninvariant items: uniform +0.6 shift in the Rural group
  q4  | c(-1.5, -0.9)*t1 + c(-0.5, 0.1)*t2 + c(0.5, 1.1)*t3 + c(1.5, 2.1)*t4
  q9  | c(-1.5, -0.9)*t1 + c(-0.5, 0.1)*t2 + c(0.5, 1.1)*t3 + c(1.5, 2.1)*t4
'

items <- paste0("q", 1:10)


n_per_group <- c(400, 400)   # Urbano, Rural

dat <- simulateData(
  model       = popModel,
  sample.nobs = n_per_group,
  ordered     = items,
  group.label = c("Urbano", "Rural")
)
table(dat$group)
head(dat)
# haven::write_sav(dat,"dados_invariancia2026.sav")


mod1 <- '
BES =~ q1 + q2 + q3 + q4 + q5
ES  =~ q6 + q7 + q8 + q9 + q10
'
fit1 <- cfa(model = mod1,
            data = dat,
            # ordered = TRUE,
            estimator = "MLR")
summary(fit1, fit.measures = TRUE, standardized = TRUE)

configural_fit <- cfa(model = mod1,
                      data = dat,
                      # ordered = TRUE,
                      estimator = "MLR",# WLSMV
                      group = "group")
metric_fit <- cfa(model = mod1,
                  data = dat,
                  # ordered = TRUE,
                  estimator = "MLR",
                  group = "group",
                  group.equal = c("loadings"))
#aqui você vai trocar "intercepts" por "thresholds"
scalar_fit <- cfa(model = mod1,
                  data = dat,
                  # ordered = TRUE,
                  estimator = "MLR",
                  group = "group",
                  group.equal = c("loadings",
                                  "intercepts"))
library(semTools)
invariance_models <- compareFit(configural_fit,metric_fit, scalar_fit)
summary(invariance_models)
lavTestScore(metric_fit,univariate = TRUE, epc = TRUE)
summary(metric_fit, fit.measures = TRUE, standardized = TRUE)

metric_fit_partial <- cfa(model = mod1,
                  data = dat,
                  # ordered = TRUE,
                  estimator = "MLR",
                  group = "group",
                  group.equal = c("loadings"),
                  group.partial = c("BES =~ q3","ES =~ q8"))



scalar_fit2 <- cfa(model = mod1,
                          data = dat,
                          # ordered = TRUE,
                          estimator = "MLR",
                          group = "group",
                          group.equal = c("loadings",
                                          "intercepts"),
                          group.partial = c("BES =~ q3","ES =~ q8"))
lavTestScore(scalar_fit2,univariate = TRUE, epc = TRUE)


# para invariância parcial com thresholds, colocamos todos os thresholds do item
# no exemplo abaixo, uma escala de 5 pontos, gera 4 thresholds
# "q1|t1","q1|t2","q1|t3","q1|t4"
scalar_fit_partial <- cfa(model = mod1,
                          data = dat,
                          # ordered = TRUE,
                          estimator = "MLR",
                          group = "group",
                          group.equal = c("loadings",
                                          "intercepts"),
                          group.partial = c("BES =~ q3","ES =~ q8",
                                            "q1 ~ 1",
                                            "q4 ~ 1",
                                            "q6 ~ 1",
                                            "q9 ~ 1"))
partialinvariance_models <- compareFit(configural_fit,metric_fit_partial, scalar_fit_partial)
summary(partialinvariance_models)
