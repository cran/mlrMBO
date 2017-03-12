## ----setup, include = FALSE, cache = FALSE-------------------------------
library(mlrMBO)
set.seed(123)
knitr::opts_chunk$set(cache = TRUE, collapse = FALSE)
knitr::knit_hooks$set(document = function(x){
  gsub("```\n*```r*\n*", "", x)
})

## ----objective_function--------------------------------------------------
library(mlrMBO)
library(ggplot2)

fun = function(x) {
  j = x$j
  method = x$method
  perf = ifelse(method == "a", sin(j), cos(j))
  return(perf)
}

objfun2 = makeSingleObjectiveFunction(
  name = "mixed_example",
  fn = fun,
  par.set = makeParamSet(
    makeNumericParam("j", lower = 0,upper = 2 * pi),
    makeDiscreteParam("method", values = c("a", "b"))
  ),
  has.simple.signature = FALSE,
  minimize = TRUE
)

# visualize the function
autoplot(objfun2)

## ------------------------------------------------------------------------
surr.rf = makeLearner("regr.randomForest", predict.type = "se")

## ----control_object------------------------------------------------------
control2 = makeMBOControl()
control2 = setMBOControlInfill(
  control = control2,
  crit = makeMBOInfillCritCB(cb.lambda = 5),
  opt.focussearch.points = 500
)

## ----termination---------------------------------------------------------
control2 = setMBOControlTermination(
  control = control2,
  iters = 10
)

## ----init_design---------------------------------------------------------
design2 = generateDesign(n = 8, par.set = getParamSet(objfun2))

## ----mbo_run, results='hold'---------------------------------------------
# Surpresses output of learners
mlr::configureMlr(show.info = FALSE, show.learner.output = FALSE, on.learner.warning = "quiet")
run2 = mbo(objfun2, design = design2, learner = surr.rf, control = control2, show.info = TRUE)

## ----mbo_res-------------------------------------------------------------
run2$y
run2$x

## ----example_run_2d------------------------------------------------------
ex.run2 = exampleRun(objfun2, design = design2, learner = surr.rf, control = control2, show.info = FALSE)

## ----plot_example_run_2d, warning=FALSE----------------------------------
plotExampleRun(ex.run2, iters = c(1L, 2L, 10L), pause = FALSE)

## ----wrapper-------------------------------------------------------------
wrap.fun = function(x) {
  x$j = if (x$method == "a") x$ja else x$jb
  x = dropNamed(x, c("ja", "jb"))
  fun(x)
}

## ----wrapper_par_space---------------------------------------------------
ps.wrap = makeParamSet(
  makeDiscreteParam("method", values = c("a", "b")),
  makeNumericParam("ja", lower = 0,upper = 2 * pi, requires = quote(method == "a")),
  makeNumericParam("jb", lower = 0,upper = 2 * pi, requires = quote(method == "b"))
  )

## ----wrapper_smoof-------------------------------------------------------
objfun3 = makeSingleObjectiveFunction(
  name = "mixed_example: Dependent J",
  fn = wrap.fun,
  par.set = ps.wrap,
  has.simple.signature = FALSE,
  minimize = TRUE
)

## ----impute_wrapper------------------------------------------------------
lrn = makeLearner("regr.randomForest", predict.type = "se", ntree = 200)
lrn = makeImputeWrapper(lrn, classes = list(numeric = imputeMax(2), factor = imputeConstant("__miss__")))

## ----makeMBOLearner------------------------------------------------------
lrn = makeMBOLearner(control = control2, fun = objfun3, ntree = 200)

## ----wrapper_run---------------------------------------------------------
design3 = generateDesign(n = 8, par.set = getParamSet(objfun3))
run3 = mbo(objfun3, learner = lrn, design = design3, control = control2, show.info = FALSE)
run3$x
run3$y

## ----residual_comparison-------------------------------------------------
op2 = as.data.frame(run2$opt.path)
op3 = as.data.frame(run3$opt.path)

# residual variance of the surrogate model for the first example
var(op2$mean - op2$y, na.rm = TRUE)
# residual variance of the advanced surrogate model with independetly treat ed jaand jb
var(op3$mean - op3$y, na.rm = TRUE)

