## ----setup, include = FALSE, cache = FALSE-------------------------------
library(mlrMBO)
set.seed(1)
knitr::opts_chunk$set(cache = TRUE, collapse = FALSE)
knitr::knit_hooks$set(document = function(x){
  gsub("```\n*```r*\n*", "", x)
})

# cheat and secretly don't do parallelization for windows compatibility.
library(parallelMap)
parallelStartMulticore = parallelStop = function(...) invisible()

## ----objective_function--------------------------------------------------
library(mlrMBO)
library(ggplot2)
obj.fun = makeBraninFunction()
# visualize the function
autoplot(obj.fun, render.levels = TRUE, show.optimum = TRUE)

## ----control-------------------------------------------------------------
library(mlrMBO)
ctrl = makeMBOControl(propose.points = 2)

## ----infill--------------------------------------------------------------
ctrl = setMBOControlInfill(ctrl, crit = crit.ei)

## ----cl------------------------------------------------------------------
ctrl = setMBOControlMultiPoint(ctrl, method = "cl", cl.lie = min)

## ------------------------------------------------------------------------
ctrl = setMBOControlTermination(ctrl, iters = 6)
ctrl = setMBOControlTermination(ctrl, max.evals = 20) # for the choosen settings will result in the same number of evaluations.

## ----cl_prallelEval------------------------------------------------------
# Kriging can create a lot of console output, which we want tu surpress here:
configureMlr(on.learner.warning = "quiet", show.learner.output = FALSE)

library(parallelMap)
parallelStartMulticore(cpus = 2, show.info = TRUE)
res = mbo(obj.fun, control = ctrl, show.info = FALSE)
parallelStop()

## ----cl_res--------------------------------------------------------------
autoplot(obj.fun, render.levels = TRUE, show.optimum = TRUE) + geom_text(data = as.data.frame(res$opt.path), mapping = aes(label = dob), color = "white")

## ----obj.fun.dis---------------------------------------------------------
obj.fun = makeSwiler2014Function()
autoplot(obj.fun, render.levels = TRUE)
ctrl = makeMBOControl(propose.points = 2)
ctrl = setMBOControlInfill(ctrl, crit = crit.cb)
ctrl = setMBOControlMultiPoint(ctrl, method = "cb")
ctrl = setMBOControlTermination(ctrl, iters = 5)
design = generateDesign(n = 5*8, par.set = getParamSet(obj.fun))
library(parallelMap)
parallelStartMulticore(cpus = 2, show.info = FALSE)
res = mbo(obj.fun, control = ctrl, design = design, show.info = FALSE)
parallelStop()
autoplot(obj.fun, render.levels = TRUE) + geom_text(data = as.data.frame(res$opt.path), mapping = aes(label = dob), color = "white")

## ----custom_noisy_obj----------------------------------------------------
# noisy objective function
fn = function(x) {
  sin(x[1]) + cos(x[2]) + sum(x^2) + rnorm(1, sd = 1 + abs(prod(x)))
}

# very noisy normal smoof function
ps = makeNumericParamSet("x", len = 2, lower =  -5, upper = 5)
obj.fun.single = makeSingleObjectiveFunction(name = "noisy", fn = fn, noisy = TRUE, par.set = ps)

# unified color scale for better comparison later
brewer.div = colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"), interpolate = "spline")
general.scale = scale_fill_gradientn(limits = c(-10, 110), colors = brewer.div(200))

# noisy output
autoplot(obj.fun.single, render.levels = TRUE) + general.scale

## ----custom_noisy_parallel-----------------------------------------------
parallelRegisterLevels(levels = "objective")
obj.fun.parallel = makeSingleObjectiveFunction(
  name = "noisy_example",
  fn = function(x) {
    rep.x = replicate(4, x, simplify = FALSE)
    res = parallelMap(fn, rep.x, simplify = TRUE, level = "custom.objective")
    mean(res)
  }, 
  noisy = TRUE, 
  par.set = makeNumericParamSet(id = "x", len = 2, lower = -5, upper = 5), 
  vectorized = FALSE,
  has.simple.signature = TRUE
)
autoplot(obj.fun.parallel, render.levels = TRUE) + general.scale

## ---- eval = FALSE-------------------------------------------------------
#  library(mlr)
#  ctrl = makeMBOControl(final.method = "best.predicted")
#  ctrl = setMBOControlTermination(ctrl, iters = 5L)
#  ctrl = setMBOControlInfill(ctrl, crit = crit.aei)
#  parallelStartMulticore(cpus = 2L, show.info = TRUE, level = "custom.objective")
#  res.parallel = mbo(obj.fun.parallel, control = ctrl, show.info = FALSE)
#  parallelStop()
#  res.parallel

