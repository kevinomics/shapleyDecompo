glmX <- glm(log(salred) ~ SEXE * POT_EXP + I(POT_EXP^2),
            data = exData,
            family = gaussian(link = "log"),
            weights = exData$extridf)

test_that("Shapley decomposition GLM", {
  res <- shapleyDecompo(database = exData,
                        model_eco = glmX,
                        equation = NA,
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = exData$extridf,
                        residuals = TRUE,
                        transfo = exp,
                        measure = Atkinson,
                        theta = 1)
  expect_type(res, "list")
})
test_that("Shapley decomposition tobit outcome with residuals, Atkinson", {
  res <- shapleyDecompo(database = exData,
                        model_eco = exTobitModel,
                        equation = "outcome",
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = exData$extridf,
                        residuals = TRUE,
                        transfo = exp,
                        measure = Atkinson,
                        theta = 1)
  expect_type(res, "list")
})
test_that("Shapley decomposition Atkinson theta = 0.5", {
  res <- shapleyDecompo(database = exData,
                        model_eco = exTobitModel,
                        equation = "outcome",
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = exData$extridf,
                        residuals = TRUE,
                        transfo = exp,
                        measure = Atkinson,
                        theta = 0.5)
  expect_type(res, "list")
})
test_that("Shapley decomposition tobit outcome with residuals, Gini_G", {
  res <- shapleyDecompo(database = exData,
                        model_eco = exTobitModel,
                        equation = "outcome",
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = exData$extridf,
                        residuals = TRUE,
                        transfo = exp,
                        measure = Gini_G,
                        theta = 1)
  expect_type(res, "list")
})
test_that("Shapley decomposition tobit outcome with residuals, Gini_w", {
  res <- shapleyDecompo(database = exData,
                        model_eco = exTobitModel,
                        equation = "outcome",
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = exData$extridf,
                        residuals = TRUE,
                        transfo = exp,
                        measure = Gini_w)
  expect_type(res, "list")
})
test_that("Shapley decomposition tobit outcome with residuals, Var", {
  res <- shapleyDecompo(database = exData,
                        model_eco = exTobitModel,
                        equation = "outcome",
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = exData$extridf,
                        residuals = TRUE,
                        transfo = exp,
                        measure = Var)
  expect_type(res, "list")
})
test_that("Shapley decomposition tobit outcome with residuals, Entropy", {
  res <- shapleyDecompo(database = exData,
                        model_eco = exTobitModel,
                        equation = "outcome",
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = exData$extridf,
                        residuals = TRUE,
                        transfo = exp,
                        measure = Entropy,
                        theta = 1)
  expect_type(res, "list")
})
test_that("Entropy theta = 0", {
  res <- shapleyDecompo(database = exData,
                        model_eco = exTobitModel,
                        equation = "outcome",
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = exData$extridf,
                        residuals = TRUE,
                        transfo = exp,
                        measure = Entropy,
                        theta = 0)
  expect_type(res, "list")
})
test_that("Entropy theta = 0.5", {
  res <- shapleyDecompo(database = exData,
                        model_eco = exTobitModel,
                        equation = "outcome",
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = exData$extridf,
                        residuals = TRUE,
                        transfo = exp,
                        measure = Entropy,
                        theta = 0.5)
  expect_type(res, "list")
})

test_that("Shapley decomposition tobit outcome with residuals, Kolm", {
  res <- shapleyDecompo(database = exData,
                        model_eco = exTobitModel,
                        equation = "outcome",
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = exData$extridf,
                        residuals = TRUE,
                        transfo = exp,
                        measure = Kolm,
                        theta = 1)
  expect_type(res, "list")
})
test_that("Shapley decomposition tobit outcome no residuals", {
  res <- shapleyDecompo(database = exData,
                        model_eco = exTobitModel,
                        equation = "outcome",
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = exData$extridf,
                        residuals = FALSE,
                        transfo = exp,
                        measure = Atkinson,
                        theta = 1)
  expect_type(res, "list")
})
test_that("Shapley decomposition tobit selection with residuals", {
  res <- shapleyDecompo(database = exData,
                        model_eco = exTobitModel,
                        equation = "selection",
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = exData$extridf,
                        residuals = TRUE,
                        transfo = exp,
                        measure = Atkinson,
                        theta = 1)
  expect_type(res, "list")
})
test_that("no equalized game", {
  res <- shapleyDecompo(database = exData,
                        model_eco = exTobitModel,
                        equation = "selection",
                        equaGame = FALSE,
                        correction = NA,
                        data_weights = exData$extridf,
                        residuals = TRUE,
                        transfo = exp,
                        measure = Atkinson,
                        theta = 1)
  expect_type(res, "list")
})
tobitCrossTerms <- selection(FULLTIME ~ SEXE * AGEQ + diplom,
                             log(salred) ~ SEXE * POT_EXP + I(POT_EXP^2),
                             method = "ml",
                             weights = exData$extridf,
                             data = exData)
test_that("tobit model with crossed terms, correction abs", {
  res <- shapleyDecompo(database = exData,
                        model_eco = tobitCrossTerms,
                        equation = "outcome",
                        equaGame = TRUE,
                        correction = "abs",
                        data_weights = exData$extridf,
                        residuals = TRUE,
                        transfo = exp,
                        measure = Atkinson,
                        theta = 1)
  expect_type(res, "list")
})
test_that("tobit model with crossed terms, correction rel", {
  res <- shapleyDecompo(database = exData,
                        model_eco = tobitCrossTerms,
                        equation = "outcome",
                        equaGame = TRUE,
                        correction = "rel",
                        data_weights = exData$extridf,
                        residuals = TRUE,
                        transfo = exp,
                        measure = Atkinson,
                        theta = 1)
  expect_type(res, "list")
})
test_that("tobit selection model with crossed terms", {
  res <- shapleyDecompo(database = exData,
                        model_eco = tobitCrossTerms,
                        equation = "selection",
                        equaGame = TRUE,
                        correction = NA,
                        data_weights = exData$extridf,
                        residuals = TRUE,
                        transfo = exp,
                        measure = Atkinson,
                        theta = 1)
  expect_type(res, "list")
})

