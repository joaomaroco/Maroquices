
# This file is automatically generated, you probably don't want to edit this

POISSTESTOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "POISSTESTOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            switch = "fromraw",
            dep = NULL,
            intervalValue = NULL,
            testValue = NULL,
            alt = "two.sided",
            ci = "exact",
            conf = 0.95,
            lambda = NULL, ...) {

            super$initialize(
                package="FEExtra",
                name="POISSTEST",
                requiresData=TRUE,
                ...)

            private$..switch <- jmvcore::OptionList$new(
                "switch",
                switch,
                options=list(
                    "fromraw",
                    "fromsummary"),
                default="fromraw")
            private$..dep <- jmvcore::OptionVariable$new(
                "dep",
                dep)
            private$..intervalValue <- jmvcore::OptionInteger$new(
                "intervalValue",
                intervalValue)
            private$..testValue <- jmvcore::OptionNumber$new(
                "testValue",
                testValue)
            private$..alt <- jmvcore::OptionList$new(
                "alt",
                alt,
                options=list(
                    "two.sided",
                    "greater",
                    "less"),
                default="two.sided")
            private$..ci <- jmvcore::OptionList$new(
                "ci",
                ci,
                options=list(
                    "exact",
                    "score",
                    "wald"),
                default="exact")
            private$..conf <- jmvcore::OptionNumber$new(
                "conf",
                conf,
                min=0,
                max=1,
                default=0.95)
            private$..lambda <- jmvcore::OptionNumber$new(
                "lambda",
                lambda)

            self$.addOption(private$..switch)
            self$.addOption(private$..dep)
            self$.addOption(private$..intervalValue)
            self$.addOption(private$..testValue)
            self$.addOption(private$..alt)
            self$.addOption(private$..ci)
            self$.addOption(private$..conf)
            self$.addOption(private$..lambda)
        }),
    active = list(
        switch = function() private$..switch$value,
        dep = function() private$..dep$value,
        intervalValue = function() private$..intervalValue$value,
        testValue = function() private$..testValue$value,
        alt = function() private$..alt$value,
        ci = function() private$..ci$value,
        conf = function() private$..conf$value,
        lambda = function() private$..lambda$value),
    private = list(
        ..switch = NA,
        ..dep = NA,
        ..intervalValue = NA,
        ..testValue = NA,
        ..alt = NA,
        ..ci = NA,
        ..conf = NA,
        ..lambda = NA)
)

POISSTESTResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "POISSTESTResults",
    inherit = jmvcore::Group,
    active = list(
        text = function() private$.items[["text"]],
        poissontest = function() private$.items[["poissontest"]],
        poissonci = function() private$.items[["poissonci"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="")
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="text",
                title=""))
            self$add(jmvcore::Table$new(
                options=options,
                name="poissontest",
                title="Teste de Poisson",
                rows=1,
                columns=list(
                    list(
                        `name`="var", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="NumberofEvents", 
                        `title`="N\u00FAmero de eventos", 
                        `type`="integer"),
                    list(
                        `name`="Intervalbase", 
                        `title`="Intervalo", 
                        `type`="integer"),
                    list(
                        `name`="Eventrate", 
                        `title`="Taxa de Eventos", 
                        `type`="number"),
                    list(
                        `name`="p", 
                        `type`="number", 
                        `format`="zto,pvalue"),
                    list(
                        `name`="Lower", 
                        `title`="Lim. Inf.", 
                        `type`="number"),
                    list(
                        `name`="Upper", 
                        `title`="Lim. Inf.", 
                        `type`="number"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="poissonci",
                title="Intervalos de Confian\u00E7a para a Poisson",
                rows=3,
                columns=list(
                    list(
                        `name`="var", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="method", 
                        `title`="M\u00E9todo", 
                        `type`="text"),
                    list(
                        `name`="lambda", 
                        `type`="number"),
                    list(
                        `name`="Lower", 
                        `title`="Lim. Inf.", 
                        `type`="number"),
                    list(
                        `name`="Upper", 
                        `title`="Lim. Sup.", 
                        `type`="number"))))}))

POISSTESTBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "POISSTESTBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "FEExtra",
                name = "POISSTEST",
                version = c(1,0,0),
                options = options,
                results = POISSTESTResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Teste e Intervalo de Confiança para a Poisson
#'
#' 
#' @param switch .
#' @param data .
#' @param dep .
#' @param intervalValue .
#' @param testValue .
#' @param alt .
#' @param ci .
#' @param conf .
#' @param lambda .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$text} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$poissontest} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$poissonci} \tab \tab \tab \tab \tab a table \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$poissontest$asDF}
#'
#' \code{as.data.frame(results$poissontest)}
#'
#' @export
POISSTEST <- function(
    switch = "fromraw",
    data,
    dep,
    intervalValue,
    testValue,
    alt = "two.sided",
    ci = "exact",
    conf = 0.95,
    lambda) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("POISSTEST requires jmvcore to be installed (restart may be required)")

    if ( ! missing(dep)) dep <- jmvcore::resolveQuo(jmvcore::enquo(dep))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(dep), dep, NULL))


    options <- POISSTESTOptions$new(
        switch = switch,
        dep = dep,
        intervalValue = intervalValue,
        testValue = testValue,
        alt = alt,
        ci = ci,
        conf = conf,
        lambda = lambda)

    analysis <- POISSTESTClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

