test_that("createModelList works", {
    library(dplyr)
    dataOld <- gapminder::gapminder %>% filter(year <= 2000)
    mod1 <- lm(lifeExp ~ year + pop + gdpPercap, dataOld)
    mod2 <- rpart::rpart(lifeExp ~ year + pop + gdpPercap, dataOld)
    mod3 <- glmnetUtils::cv.glmnet(lifeExp ~ year + pop + gdpPercap, dataOld)
    ModList <- list("mod1" = mod1,
                    "mod2" = mod2,
                    "mod3" = mod3)
    class(ModList) <- "ModelList"
    expect_equal(createModelList(mod1, mod2, mod3), ModList)
})

test_that("summary.ModelList works", {
    library(dplyr)
    dataOld <- gapminder::gapminder %>% filter(year <= 2000)
    mod1 <- lm(lifeExp ~ year + pop + gdpPercap, dataOld)
    mod2 <- rpart::rpart(lifeExp ~ year + pop + gdpPercap, dataOld)
    mod3 <- glmnetUtils::cv.glmnet(lifeExp ~ year + pop + gdpPercap, dataOld)
    ModList <- createModelList(mod1, mod2, mod3)
    expect_no_error(summary(ModList))
    expect_length(summary(ModList), 3L)
})

test_that("predictModelList works", {
    library(dplyr)
    dataOld <- gapminder::gapminder %>% filter(year <= 2000)
    mod1 <- lm(lifeExp ~ year + pop + gdpPercap, dataOld)
    mod2 <- rpart::rpart(lifeExp ~ year + pop + gdpPercap, dataOld)
    mod3 <- glmnetUtils::cv.glmnet(lifeExp ~ year + pop + gdpPercap, dataOld)
    ModList <- createModelList(mod1, mod2, mod3)
    dataNew <- gapminder::gapminder %>% filter(year > 2000)
    expect_no_error(predict(ModList, newdata = dataNew))
})

