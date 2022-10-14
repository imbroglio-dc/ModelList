#' @title ModelList
#' @name ModelList
#' @description
#' createModelList() combines fitted models into an object of class "ModelList".
#'
#' summary.ModelList() is the summary method for class "ModelList".
#'
#' predict.ModelList() is the predict method for class "ModelList".
#'
#' @param ... : fitted models
#' @param object : object of class "ModelList"
#'
#' @return an object of class "ModelList"
#'
#' @importFrom stats predict
#'
#' @export createModelList
#' @exportClass ModelList
#'
#' @examples
#' library(dplyr)
#' dataOld <- gapminder::gapminder %>% filter(year <= 2000)
#' mod1 <- lm(lifeExp ~ year + pop + gdpPercap, dataOld)
#' mod2 <- rpart::rpart(lifeExp ~ year + pop + gdpPercap, dataOld)
#' mod3 <- glmnetUtils::cv.glmnet(lifeExp ~ year + pop + gdpPercap, dataOld)
#' ModList <- createModelList(mod1, mod2, mod3)
#' summary(ModList)

createModelList <- function(...) {
    Models <- structure(list(...),
                        class = "ModelList",
                        names = paste0("mod", 1:...length()))
    return(Models)
}

#' @rdname ModelList
#' @method summary ModelList
#' @exportS3Method summary ModelList
summary.ModelList <- function(object, ...) {
    ModelListSummary <- lapply(seq_along(object), function(i) {
        cat("###", names(object)[i], "###\n")
        obj <- object[[i]]
        if (inherits(obj, "rpart")) {
            try(summary(obj, cp = obj$cptable[, "CP"][1],
                    digits = 3))
        } else {
            try(print(obj))
        }
        })
    invisible(ModelListSummary)
}

#' @rdname ModelList
#' @method predict ModelList
#' @exportS3Method predict ModelList
predict.ModelList <- function(object, ...) {
    ModelListPred <- try(sapply(object, function(obj) {
        do.call(predict, c(list("object" = obj), list(...)))
    }))
}

