data("Mroz87")
glmX <- glm(wage ~ exper + I( exper^2 ) + educ * city,
            data = Mroz87)
test_that("Shapley decomposition GLM", {
  res <- shapleyDecompo(database = Mroz87,
                        model_eco = glmX,
                        equation = NA,
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = rep(1, nrow(Mroz87)),
                        residuals = TRUE,
                        transfo = NULL,
                        measure = Gini_w,
                        theta = NULL)
  expect_type(res, "list")
})
exTobitModel <- sampleSelection::selection(lfp ~ age + I(age^2) + faminc + kids5 + educ,
                                           wage ~ exper + I(exper^2) + educ + city,
                                           data = Mroz87)
test_that("Shapley decomposition tobit outcome with residuals, Gini_w", {
  res <- shapleyDecompo(database = Mroz87,
                        model_eco = exTobitModel,
                        equation = "outcome",
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = rep(1, nrow(Mroz87)),
                        residuals = TRUE,
                        transfo = NULL,
                        measure = Gini_w,
                        theta = NULL)
  expect_type(res, "list")
})
test_that("Shapley decomposition Gini_w theta = 0.5", {
  res <- shapleyDecompo(database = Mroz87,
                        model_eco = exTobitModel,
                        equation = "outcome",
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = rep(1, nrow(Mroz87)),
                        residuals = TRUE,
                        transfo = NULL,
                        measure = Gini_w,
                        theta = NULL)
  expect_type(res, "list")
})
test_that("Shapley decomposition tobit outcome with residuals, Gini_G", {
  res <- shapleyDecompo(database = Mroz87,
                        model_eco = exTobitModel,
                        equation = "outcome",
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = rep(1, nrow(Mroz87)),
                        residuals = TRUE,
                        transfo = NULL,
                        measure = Gini_G,
                        theta = 1)
  expect_type(res, "list")
})
exTobitModel2 <- sampleSelection::selection(lfp ~ age + I(age^2) + faminc + kids5 + educ,
                                            log(wage) ~ exper + I(exper^2) + educ + city,
                                            data = Mroz87)
test_that("Shapley decomposition tobit outcome with residuals, Atkinson", {
  res <- shapleyDecompo(database = Mroz87,
                        model_eco = exTobitModel2,
                        equation = "outcome",
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = rep(1, nrow(Mroz87)),
                        residuals = TRUE,
                        transfo = exp,
                        measure = Atkinson,
                        theta = 1)
  expect_type(res, "list")
})
test_that("Shapley decomposition tobit outcome with residuals, Var", {
  res <- shapleyDecompo(database = Mroz87,
                        model_eco = exTobitModel2,
                        equation = "outcome",
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = rep(1, nrow(Mroz87)),
                        residuals = TRUE,
                        transfo = exp,
                        measure = Var)
  expect_type(res, "list")
})
test_that("Shapley decomposition tobit outcome with residuals, Entropy", {
  res <- shapleyDecompo(database = Mroz87,
                        model_eco = exTobitModel2,
                        equation = "outcome",
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = rep(1, nrow(Mroz87)),
                        residuals = TRUE,
                        transfo = exp,
                        measure = Entropy,
                        theta = 1)
  expect_type(res, "list")
})
test_that("Entropy theta = 0", {
  res <- shapleyDecompo(database = Mroz87,
                        model_eco = exTobitModel2,
                        equation = "outcome",
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = rep(1, nrow(Mroz87)),
                        residuals = TRUE,
                        transfo = exp,
                        measure = Entropy,
                        theta = 0)
  expect_type(res, "list")
})
test_that("Entropy theta = 0.5", {
  res <- shapleyDecompo(database = Mroz87,
                        model_eco = exTobitModel2,
                        equation = "outcome",
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = rep(1, nrow(Mroz87)),
                        residuals = TRUE,
                        transfo = exp,
                        measure = Entropy,
                        theta = 0.5)
  expect_type(res, "list")
})

test_that("Shapley decomposition tobit outcome with residuals, Kolm", {
  res <- shapleyDecompo(database = Mroz87,
                        model_eco = exTobitModel,
                        equation = "outcome",
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = rep(1, nrow(Mroz87)),
                        residuals = TRUE,
                        transfo = NULL,
                        measure = Kolm,
                        theta = 1)
  expect_type(res, "list")
})
test_that("Shapley decomposition tobit outcome, Atkinson theta = 0.5", {
  res <- shapleyDecompo(database = Mroz87,
                        model_eco = exTobitModel2,
                        equation = "outcome",
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = rep(1, nrow(Mroz87)),
                        residuals = FALSE,
                        transfo = exp,
                        measure = Atkinson,
                        theta = 0.5)
  expect_type(res, "list")
})
test_that("Shapley decomposition tobit selection with residuals", {
  res <- shapleyDecompo(database = Mroz87,
                        model_eco = exTobitModel,
                        equation = "selection",
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = rep(1, nrow(Mroz87)),
                        residuals = TRUE,
                        transfo = NULL,
                        measure = Kolm,
                        theta = 1)
  expect_type(res, "list")
})
test_that("no equalized game", {
  res <- shapleyDecompo(database = Mroz87,
                        model_eco = exTobitModel,
                        equation = "outcome",
                        equaGame = FALSE,
                        correction = NA,
                        data_weights = rep(1, nrow(Mroz87)),
                        residuals = TRUE,
                        transfo = NULL,
                        measure = Kolm,
                        theta = 1)
  expect_type(res, "list")
})
tobitCrossTerms <- sampleSelection::selection(
  lfp ~ age + I(age^2) + faminc + kids5 * educ,
  log(wage) ~ exper + I(exper^2) + educ * city,
  data = Mroz87)
test_that("tobit model with crossed terms, correction abs", {
  res <- shapleyDecompo(database = Mroz87,
                        model_eco = tobitCrossTerms,
                        equation = "outcome",
                        equaGame = TRUE,
                        correction = "abs",
                        data_weights = rep(1, nrow(Mroz87)),
                        residuals = TRUE,
                        transfo = exp,
                        measure = Atkinson,
                        theta = 1)
  expect_type(res, "list")
})
test_that("tobit model with crossed terms, correction rel", {
  res <- shapleyDecompo(database = Mroz87,
                        model_eco = tobitCrossTerms,
                        equation = "outcome",
                        equaGame = TRUE,
                        correction = "rel",
                        data_weights = rep(1, nrow(Mroz87)),
                        residuals = TRUE,
                        transfo = exp,
                        measure = Atkinson,
                        theta = 1)
  expect_type(res, "list")
})
test_that("tobit selection model with crossed terms", {
  res <- shapleyDecompo(database = Mroz87,
                        model_eco = tobitCrossTerms,
                        equation = "selection",
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = rep(1, nrow(Mroz87)),
                        residuals = TRUE,
                        transfo = exp,
                        measure = Atkinson,
                        theta = 1)
  expect_type(res, "list")
})
