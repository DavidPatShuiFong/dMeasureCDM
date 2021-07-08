#' dMeasureCDM - list billings module for dMeasure
#'
#' @md
#'
#' @name cdm
#' @title dMeasureCDM
#'
#' @include r6_helpers.R
#' functions to help create R6 classes
NULL

#' dMeasureIntegration
#'
#' @name dMeasureIntegration
#'
#' @description integration with dMeasure
#'   (especially DailyMeasure)
#'
#' @param information the information required
#'   `Provides` - modules provided (in this case, `dMeasureCDM`)
#'   `Requires` - the modules required (including `dMeasure`)
#'   `moduleID` - IDs of modules to create
#'
#' @return vector of required information
#'
#' @export
dMeasureIntegration <- function(information) {
  if (information == "Provides") {return(c("dMeasureCDM"))}
  if (information == "Requires") {return(c("dMeasure", "dMeasureBillings"))}
  if (information == "moduleID") {return(c("CDM_dt"))}
}

#' dMeasureCDM class
#' @title dMeasureCDM class
#' @description list chronic disease management opportunities
#' @export
dMeasureCDM <- R6::R6Class("dMeasureCDM",
  public = list(
    # dM is a dMeasure object
    dM = NULL,
    dMBillings = NULL,
    initialize = function(dMeasure_obj, dMeasureBillings_obj) {
      # dMeasure_obj is a R6 dMeasure object
      # dMeasureBilings_obj is a R6 object (which also depends on dMeasure!)
      self$dM <- dMeasure_obj
      self$dMBillings <- dMeasureBillings_obj
      if (length(public_init_fields$name) > 0) { # only if any defined
        for (i in 1:length(public_init_fields$name)) {
          if (public_init_fields$obj[[i]] == "dMeasureCDM") {
            self[[public_init_fields$name[[i]]]] <-
              eval(public_init_fields$value[[i]]) # could 'quote' the value
          }
        }
      }
      if (length(private_init_fields$name) > 0) { # only if any defined
        for (i in 1:length(private_init_fields$name)) {
          if (private_init_fields$obj[[i]] == "dMeasureCDM") {
            private[[private_init_fields$name[[i]]]] <-
              eval(private_init_fields$value[[i]]) # could 'quote' the value
          }
        }
      }

      if (requireNamespace("shiny", quietly = TRUE)) {
        # set reactive version only if shiny is available
        # note that this is for reading (from programs calling this object) only!
        if (length(reactive_fields$name) > 0) { # only if any .reactive() defined
          for (i in 1:length(reactive_fields$name)) {
            if (reactive_fields$obj[[i]] == "dMeasureCDM") {
              self[[reactive_fields$name[[i]]]] <- shiny::reactiveVal(
                eval(reactive_fields$value[[i]]) # could 'quote' the value
              )
            }
          }
        }
        if (length(reactive_event$name) > 0) { # only if any .reactive() defined
          for (i in 1:length(reactive_event$name)) {
            if (reactive_event$obj[[i]] == "dMeasureCDM") {
              self[[reactive_event$name[[i]]]] <-
                eval(reactive_event$value[[i]]) # could 'quote' the value
            }
          }
        }
      }
    }
  )
  # this is a 'skeleton' class
  # it is filled in the with the '.public' function
)

##### special reactive functions ##########################


.private(dMeasureCDM, "set_reactive", function(myreactive, value) {
  # reactive (if shiny/reactive environment is available) is set to 'value'
  # myreactive is passed by reference
  if (requireNamespace("shiny", quietly = TRUE)) {
    myreactive(value)
  }
})
.private(dMeasureCDM, "trigger", function(myreactive) {
  # toggles a reactive between (usually) 0 and 1
  if (requireNamespace("shiny", quietly = TRUE)) {
    myreactive(1 - shiny::isolate(myreactive()))
  }
})

##### constants #########################################################################

# MBS (medicare benefits schedule) item numbers for CDM
cdm_item <- data.frame(
  code = c(
    721, 92024, 92068, 723, 92025, 92069, 732, 92028, 92072,
    703, 705, 707,
    2517, 2521, 2525,
    2546, 2552, 2558,
    2700, 2701, 92112, 92124, 92113, 92125,
    2715, 2717, 92116, 92128, 92117, 92129,
    715, 228, 93470, 93479
  ),
  name = c(
    "GPMP", "GPMP", "GPMP", "TCA", "TCA", "TCA", "GPMP R/V", "GPMP R/v", "GPMP R/V",
    "HA", "HA", "HA",
    # note that the same item numbers 703/705/707 can be used for multiple health assessments
    # 75+, elsewhere coded as "HA75"
    # 45-49 (or 40-49) health assessment, which will elsewhere be code "HA45"
    # and *also* for intellectual disability health assessment, elsewhere coded "HAIQ"
    # and *also* for refugee health asseessment, elsewhere coded  'RHA'
    "DiabetesSIP", "DiabetesSIP", "DiabetesSIP",
    "AsthmaSIP", "AsthmaSIP", "AsthmaSIP",
    "MHCP", "MHCP", "MHCP", "MHCP", "MHCP", "MHCP",
    "MHCP", "MHCP", "MHCP", "MHCP", "MHCP", "MHCP",
    "HAATSI", "HAATSI", "HAATSI", "HAATSI"
  ),
  stringsAsFactors = FALSE
)

cdm_item_names <- c(as.character(unique(cdm_item$name)), "HA75", "HA45", "HAIQ", "RHA")
cdm_item_names <- cdm_item_names[!cdm_item_names == "HA"]
# de-factored and unique cdm_item$name
# need to manually add HA45 and HAIQ

item_status <- list(never = "Never", late = "Late", uptodate = "Up-to-date")

