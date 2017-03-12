## ----setup, include = FALSE, cache = FALSE-------------------------------
library(mlrMBO)
set.seed(1)
knitr::opts_chunk$set(cache = TRUE, collapse = FALSE)
knitr::knit_hooks$set(document = function(x){
  gsub("```\n*```r*\n*", "", x)
})

## ----objective_function--------------------------------------------------
library(mlrMBO)
library(ggplot2)
fun = function(x) {
  rnorm(1, mean = x^2, sd = 0.5 + 0.5*(x+5))
}
obj.fun = makeSingleObjectiveFunction(name = "noisy_parable", fn = fun, has.simple.signature = TRUE, par.set = makeNumericParamSet("x", 1, -5, 5), noisy = TRUE)
# visualize the function
autoplot(obj.fun, length.out = 200)

## ----control-------------------------------------------------------------
ctrl = makeMBOControl(final.method = "best.predicted", final.evals = 10)

## ----infill.eqi----------------------------------------------------------
ctrl = setMBOControlInfill(ctrl, crit = crit.eqi)
ctrl = setMBOControlTermination(ctrl, iters = 7)

## ----run-----------------------------------------------------------------
# Kriging can create a lot of console output, which we want tu surpress here:
configureMlr(on.learner.warning = "quiet", show.learner.output = FALSE)
res = mbo(obj.fun, control = ctrl, show.info = FALSE)
res$x
res$y

## ----noise---------------------------------------------------------------
(final.y = getOptPathY(res$opt.path, dob = 8))
var(final.y)

## ----infill.aei----------------------------------------------------------
ctrl = setMBOControlInfill(ctrl, crit = crit.aei)
# Kriging can create a lot of console output, which we want tu surpress here:
configureMlr(on.learner.warning = "quiet", show.learner.output = FALSE)
set.seed(1)
res = exampleRun(obj.fun, control = ctrl, show.info = FALSE, points.per.dim = 200, noisy.evals = 1)

## ----aei.res-------------------------------------------------------------
res$mbo.res$x

res$mbo.res$y

## ------------------------------------------------------------------------
plotExampleRun(res, pause = FALSE, iters = c(1,2,7))

