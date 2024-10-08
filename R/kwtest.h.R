
# This file is automatically generated, you probably don't want to edit this

KWTestOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "KWTestOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            dep = NULL,
            group = NULL,
            posts = "Nenhum",
            method = "bonferroni", ...) {

            super$initialize(
                package="FEExtra",
                name="KWTest",
                requiresData=TRUE,
                ...)

            private$..dep <- jmvcore::OptionVariable$new(
                "dep",
                dep,
                required=TRUE,
                suggested=list(
                    "continuous",
                    "ordinal"),
                permitted=list(
                    "numeric"))
            private$..group <- jmvcore::OptionVariable$new(
                "group",
                group,
                required=TRUE,
                suggested=list(
                    "nominal",
                    "ordinal"))
            private$..posts <- jmvcore::OptionList$new(
                "posts",
                posts,
                options=list(
                    "Nenhum",
                    "Dunn",
                    "Iman-Conover",
                    "DSCF"),
                default="Nenhum")
            private$..method <- jmvcore::OptionList$new(
                "method",
                method,
                options=list(
                    "bonferroni",
                    "sidak",
                    "holm"),
                default="bonferroni")

            self$.addOption(private$..dep)
            self$.addOption(private$..group)
            self$.addOption(private$..posts)
            self$.addOption(private$..method)
        }),
    active = list(
        dep = function() private$..dep$value,
        group = function() private$..group$value,
        posts = function() private$..posts$value,
        method = function() private$..method$value),
    private = list(
        ..dep = NA,
        ..group = NA,
        ..posts = NA,
        ..method = NA)
)

KWTestResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "KWTestResults",
    inherit = jmvcore::Group,
    active = list(
        kw = function() private$.items[["kw"]],
        posts = function() private$.items[["posts"]],
        dunn = function() private$.items[["dunn"]],
        DSCF = function() private$.items[["DSCF"]],
        iman = function() private$.items[["iman"]],
        plot = function() private$.items[["plot"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="ANOVA em Ordens de Kruskal-Wallis")
            self$add(jmvcore::Table$new(
                options=options,
                name="kw",
                title="ANOVA em ordens de Kruskal-Wallis",
                rows=1,
                visible=TRUE,
                clearWith=list(
                    "dep",
                    "group"),
                columns=list(
                    list(
                        `name`="var", 
                        `title`="Var. Dependente", 
                        `type`="text"),
                    list(
                        `name`="chiSq", 
                        `title`="H", 
                        `type`="number"),
                    list(
                        `name`="df", 
                        `title`="df", 
                        `type`="integer"),
                    list(
                        `name`="p", 
                        `title`="Pasymp", 
                        `type`="number", 
                        `format`="zto,pvalue"),
                    list(
                        `name`="px", 
                        `title`="Pexact", 
                        `type`="number"),
                    list(
                        `name`="es", 
                        `title`="\u03B7\u00B2", 
                        `type`="number"))))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="posts",
                title="Testes Post-hoc"))
            self$add(jmvcore::Table$new(
                options=options,
                name="dunn",
                title="Teste Post-hoc de Dunn para amostras independentes",
                rows="nComps",
                visible=FALSE,
                columns=list(
                    list(
                        `name`="var", 
                        `title`="Compara\u00E7\u00E3o", 
                        `type`="text"),
                    list(
                        `name`="Z", 
                        `type`="number"),
                    list(
                        `name`="p", 
                        `type`="number", 
                        `format`="zto,pvalue"),
                    list(
                        `name`="Padj", 
                        `title`="p<sub>aj<\\aj>", 
                        `type`="number", 
                        `format`="zto,pvalue"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="DSCF",
                title="Teste Post-hoc de DSCF",
                rows="nComps",
                visible=FALSE,
                columns=list(
                    list(
                        `name`="var", 
                        `title`="Compara\u00E7\u00E3o", 
                        `type`="text"),
                    list(
                        `name`="W", 
                        `type`="number"),
                    list(
                        `name`="P", 
                        `type`="number", 
                        `format`="zto,pvalue"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="iman",
                title="Testes Post-hoc de Iman-Conover",
                rows="nComps",
                visible=FALSE,
                columns=list(
                    list(
                        `name`="var", 
                        `title`="Compara\u00E7\u00E3o", 
                        `type`="text"),
                    list(
                        `name`="t", 
                        `type`="number"),
                    list(
                        `name`="P", 
                        `type`="number", 
                        `format`="zto,pvalue"),
                    list(
                        `name`="Padj", 
                        `title`="Paj", 
                        `type`="number", 
                        `format`="zto,pvalue"))))
            self$add(jmvcore::Image$new(
                options=options,
                name="plot",
                title="Box Plots",
                visible=FALSE,
                width=450,
                height=300,
                renderFun=".plot"))}))

KWTestBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "KWTestBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "FEExtra",
                name = "KWTest",
                version = c(1,0,0),
                options = options,
                results = KWTestResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' ANOVA em Ordens de Kruskal-Wallis com testes Post-hoc
#'
#' 
#' @param data .
#' @param dep A variável dependente. Tem que ser pelo menos ordinal.
#' @param group A variável independente. Nominal ou ordinal..
#' @param posts Teste de Dunn sem correção e com correção a escolher
#'   (Bonferroni-Dunn é a mais usual)
#' @param method .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$kw} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$posts} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$dunn} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$DSCF} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$iman} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$plot} \tab \tab \tab \tab \tab an image \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$kw$asDF}
#'
#' \code{as.data.frame(results$kw)}
#'
#' @export
KWTest <- function(
    data,
    dep,
    group,
    posts = "Nenhum",
    method = "bonferroni") {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("KWTest requires jmvcore to be installed (restart may be required)")

    if ( ! missing(dep)) dep <- jmvcore::resolveQuo(jmvcore::enquo(dep))
    if ( ! missing(group)) group <- jmvcore::resolveQuo(jmvcore::enquo(group))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(dep), dep, NULL),
            `if`( ! missing(group), group, NULL))


    options <- KWTestOptions$new(
        dep = dep,
        group = group,
        posts = posts,
        method = method)

    analysis <- KWTestClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

