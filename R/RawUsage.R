library(iSEE)
.featAssayAssay <- "Assay"
.featAssayXAxis <- "XAxis"
.featAssayXAxisColData <- "XAxisColumnData"

.featAssayXAxisFeatName <- "XAxisFeatureName"
.featAssayXAxisRowTable <- "XAxisFeatureSource"
.featAssayXAxisFeatDynamic <- "XAxisFeatureDynamicSource"

.featAssayYAxisFeatName <- "YAxisFeatureName"
.featAssayYAxisRowTable <- "YAxisFeatureSource"
.featAssayYAxisFeatDynamic <- "YAxisFeatureDynamicSource"

collated <- character(0)
collated[.featAssayAssay] <- "character"
collated[.featAssayXAxis] <- "character"
collated[.featAssayXAxisColData] <- "character"

collated[.featAssayXAxisFeatName] <- "character"
collated[.featAssayXAxisRowTable] <- "character"
collated[.featAssayXAxisFeatDynamic] <- "logical"

collated[.featAssayYAxisFeatName] <- "character"
collated[.featAssayYAxisRowTable] <- "character"
collated[.featAssayYAxisFeatDynamic] <- "logical"

setClass("RawUsage", contains="ColumnDotPlot", slots=collated)

####################################################

RawUsage <- function(...) {
  new("RawUsage", ...)
}

setMethod("initialize", "RawUsage", function(.Object, ...) {
  args <- list(...)
  args <- .emptyDefault(args, .featAssayAssay, NA_character_)
  args <- .emptyDefault(args, .featAssayXAxis, .featAssayXAxisNothingTitle)
  args <- .emptyDefault(args, .featAssayXAxisColData, NA_character_)

  args <- .emptyDefault(args, .featAssayXAxisRowTable, iSEE:::.noSelection)
  args <- .emptyDefault(args, .featAssayXAxisFeatName, NA_character_)
  args <- .emptyDefault(args, .featAssayXAxisFeatDynamic, iSEEOptions$get("selection.dynamic.single"))

  args <- .emptyDefault(args, .featAssayYAxisRowTable, iSEE:::.noSelection)
  args <- .emptyDefault(args, .featAssayYAxisFeatName, NA_character_)
  args <- .emptyDefault(args, .featAssayYAxisFeatDynamic, iSEEOptions$get("selection.dynamic.single"))

  do.call(callNextMethod, c(list(.Object), args))
})

setMethod(".refineParameters", "RawUsage", function(x, se) {
  x <- callNextMethod()
  if (is.null(x)) {
    return(NULL)
  }

  if (nrow(se)==0L) {
    warning(sprintf("no rows available for plotting '%s'", class(x)[1]))
    return(NULL)
  }

  all_assays <- .getCachedCommonInfo(se, "DotPlot")$valid.assay.names
  if (length(all_assays)==0L) {
    warning(sprintf("no valid 'assays' for plotting '%s'", class(x)[1]))
    return(NULL)
  }

  all_assays <- c(intersect(iSEEOptions$get("assay"), all_assays), all_assays)
  x <- .replaceMissingWithFirst(x, .featAssayAssay, all_assays)

  for (field in c(.featAssayXAxisFeatName, .featAssayYAxisFeatName)) {
    x <- .replaceMissingWithFirst(x, field, rownames(se))
  }

  column_covariates <- .getCachedCommonInfo(se, "ColumnDotPlot")$valid.colData.names
  if (length(column_covariates)==0L) {
    if (x[[.featAssayXAxis]] == .featAssayXAxisColDataTitle) {
      x[[.featAssayXAxis]] <- .featAssayXAxisNothingTitle
    }
  } else {
    x <- .replaceMissingWithFirst(x, .featAssayXAxisColData, column_covariates)
  }

  x
})

.featAssayXAxisNothingTitle <- "None"
.featAssayXAxisColDataTitle <- "Column data"
.featAssayXAxisFeatNameTitle <- "Feature name"

S4Vectors::setValidity2("RawUsage", function(object) {
  msg <- character(0)

  msg <- .allowableChoiceError(msg, object, .featAssayXAxis,
                               c(.featAssayXAxisNothingTitle, .featAssayXAxisColDataTitle, .featAssayXAxisFeatNameTitle))

  msg <- .singleStringError(msg, object,
                            c(.featAssayAssay, .featAssayXAxisColData, .featAssayXAxisRowTable,
                              .featAssayXAxisFeatName, .featAssayYAxisRowTable, .featAssayYAxisFeatName))

  if (length(msg)) {
    return(msg)
  }
  TRUE
})

setMethod(".defineDataInterface", "RawUsage", function(x, se, select_info) {
  panel_name <- .getEncodedName(x)
  .input_FUN <- function(field) { paste0(panel_name, "_", field) }

  all_assays <- .getCachedCommonInfo(se, "DotPlot")$valid.assay.names
  column_covariates <- .getCachedCommonInfo(se, "ColumnDotPlot")$valid.colData.names
  tab_by_row <- select_info$single$feature

  xaxis_choices <- c(.featAssayXAxisNothingTitle)
  if (length(column_covariates)) { # As it is possible for this plot to be _feasible_ but for no column data to exist.
    xaxis_choices <- c(xaxis_choices, .featAssayXAxisColDataTitle)
  }
  xaxis_choices <- c(xaxis_choices, .featAssayXAxisFeatNameTitle)

  list(
    selectizeInput(.input_FUN(.featAssayYAxisFeatName),
                   label="Y-axis feature:", choices=NULL, selected=NULL, multiple=FALSE),
    selectInput(.input_FUN(.featAssayYAxisRowTable), label=NULL, choices=tab_by_row,
                selected=iSEE:::.choose_link(x[[.featAssayYAxisRowTable]], tab_by_row)),
    checkboxInput(.input_FUN(.featAssayYAxisFeatDynamic),
                  label="Use dynamic feature selection for the y-axis",
                  value=x[[.featAssayYAxisFeatDynamic]]),

    selectInput(paste0(.getEncodedName(x), "_", .featAssayAssay), label=NULL,
                choices="proportions", selected=x[[.featAssayAssay]]), # all_assays --> proportions
    radioButtons(.input_FUN(.featAssayXAxis), label="X-axis:", inline=TRUE,
                 choices=xaxis_choices, selected=x[[.featAssayXAxis]]),

    .conditionalOnRadio(.input_FUN(.featAssayXAxis),
                        .featAssayXAxisColDataTitle,
                        selectInput(.input_FUN(.featAssayXAxisColData),
                                    label="X-axis column data:",
                                    choices=column_covariates, selected=x[[.featAssayXAxisColData]])),

    .conditionalOnRadio(.input_FUN(.featAssayXAxis),
                        .featAssayXAxisFeatNameTitle,
                        selectizeInput(.input_FUN(.featAssayXAxisFeatName),
                                       label="X-axis feature:", choices=NULL, selected=NULL, multiple=FALSE),
                        selectInput(.input_FUN(.featAssayXAxisRowTable), label=NULL,
                                    choices=tab_by_row, selected=x[[.featAssayXAxisRowTable]]),
                        checkboxInput(.input_FUN(.featAssayXAxisFeatDynamic),
                                      label="Use dynamic feature selection for the x-axis",
                                      value=x[[.featAssayXAxisFeatDynamic]])
    )
  )
})

setMethod(".createObservers", "RawUsage", function(x, se, input, session, pObjects, rObjects) {
  callNextMethod()

  plot_name <- .getEncodedName(x)

  .createProtectedParameterObservers(plot_name,
                                     fields=c(.featAssayAssay, .featAssayXAxisColData),
                                     input=input, pObjects=pObjects, rObjects=rObjects)
})

setMethod(".singleSelectionSlots", "RawUsage", function(x) {
  c(callNextMethod(),
    list(
      list(
        parameter=.featAssayXAxisFeatName,
        source=.featAssayXAxisRowTable,
        dimension="feature",
        dynamic=.featAssayXAxisFeatDynamic,
        use_mode=.featAssayXAxis,
        use_value=.featAssayXAxisFeatNameTitle,
        protected=TRUE
      ),
      list(
        parameter=.featAssayYAxisFeatName,
        source=.featAssayYAxisRowTable,
        dimension="feature",
        dynamic=.featAssayYAxisFeatDynamic,
        use_mode=NA,
        use_value=NA,
        protected=TRUE
      )
    )
  )
})

setMethod(".fullName", "RawUsage", function(x) "Raw usage plot") # Feature assay --> Raw usage

setMethod(".panelColor", "RawUsage", function(x) "#808080") # #7BB854 --> #808080

setMethod(".generateDotPlotData", "RawUsage", function(x, envir) {
  data_cmds <- list()

  ## Setting up the y-axis:
  gene_selected_y <- x[[.featAssayYAxisFeatName]]
  assay_choice <- x[[.featAssayAssay]]
  plot_title <- gene_selected_y
  y_lab <- sprintf("%s (%s)", gene_selected_y, assay_choice)
  data_cmds[["y"]] <- sprintf(
    "plot.data <- data.frame(Y=assay(se, %s)[%s, ], row.names=colnames(se))",
    deparse(assay_choice), deparse(gene_selected_y)
  )

  ## Checking X axis choice:
  x_choice <- x[[.featAssayXAxis]]

  if (x_choice == .featAssayXAxisColDataTitle) { # colData column selected
    x_lab <- x[[.featAssayXAxisColData]]
    plot_title <- paste(plot_title, "vs", x_lab)
    data_cmds[["x"]] <- sprintf("plot.data$X <- colData(se)[, %s];", deparse(x_lab))

  } else if (x_choice == .featAssayXAxisFeatNameTitle) { # gene selected
    gene_selected_x <- x[[.featAssayXAxisFeatName]]
    plot_title <- paste(plot_title, "vs", gene_selected_x)
    x_lab <- sprintf("%s (%s)", gene_selected_x, assay_choice)
    data_cmds[["x"]] <- sprintf(
      "plot.data$X <- assay(se, %s)[%s, ];",
      deparse(assay_choice), deparse(gene_selected_x)
    )

  } else { # no x axis variable specified: show single violin
    x_lab <- ''
    data_cmds[["x"]] <- "plot.data$X <- factor(character(ncol(se)))"
  }

  data_cmds <- unlist(data_cmds)
  .textEval(data_cmds, envir)

  list(commands=data_cmds, labels=list(title=plot_title, X=x_lab, Y=y_lab))
})

setMethod(".definePanelTour", "RawUsage", function(x) {
  collated <- character(0)

  collated <- rbind(
    c(paste0("#", .getEncodedName(x)), sprintf("The <font color=\"%s\">Feature assay plot</font> panel shows assay values for a particular feature (i.e., row) of a <code>SummarizedExperiment</code> object or one of its subclasses. Here, each point corresponds to a column (usually a sample) of the <code>SummarizedExperiment</code> object, and the y-axis represents the assay values.", .getPanelColor(x))),
    .addTourStep(x, .dataParamBoxOpen, "The <i>Data parameters</i> box shows the available parameters that can be tweaked in this plot.<br/><br/><strong>Action:</strong> click on this box to open up available options."),
    .addTourStep(x, .featAssayYAxisFeatName, "We can manually choose the feature of interest based on the row names of our <code>SummarizedExperiment</code> object.", is_selectize=TRUE),
    .addTourStep(x, .featAssayYAxisRowTable, sprintf("Alternatively, we can link the choice of feature to a single selection from another panel such as a <font color=\"%s\">Row data table</font>.", .getPanelColor(RowDataTable())), is_selectize=TRUE),
    .addTourStep(x, .featAssayYAxisFeatDynamic, "The upstream panel can even be chosen dynamically, where a single selection of a feature from any panel in the current instance can be used to specify the feature to be shown on the y-axis in this pane."),
    .addTourStep(x, .featAssayXAxis, "A variety of choices are available for the variable to be plotted on the x-axis.<br/><br/><strong>Action:</strong> click on <i>Column data</i> to stratify values by a column metadata field."),
    .addTourStep(x, .featAssayXAxisColData, "This exposes a new interface element that can be used that can be used to choose a covariate to show on the x-axis. Similar logic applies for plotting against the assay values of another feature with the <i>Feature name</i> choice.", is_selectize=TRUE)
  )

  rbind(
    data.frame(element=collated[,1], intro=collated[,2], stringsAsFactors=FALSE),
    callNextMethod()
  )
})

