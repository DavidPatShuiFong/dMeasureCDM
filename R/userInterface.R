##### Chronic Disease Management item numbers ###########################################
#' Chronic Disease Management (CDM) item numbers - UI for GPstat
#'
#' @name cdm_GPstatUI
#' @include CDM.R
#' R6 definitions
NULL

#' item description for left sidebar menu
#'
#' @name shinydashboardmenuItem
#'
#' @return shinydashboard menuItem object
#'
#' @export
shinydashboardmenuItem <- function() {
  x <- list(
    shinydashboard::menuItem(
      "CDM items",
      tabName = "cdm",
      icon = shiny::icon("file-medical-alt")
    )
  )
  return(x)
}

#' center panel description
#'
#' @name dMeasureShinytabItems
#'
#' @return shinytabItems
#'
#' @export
dMeasureShinytabItems <- function() {
  x <- list(
    shinydashboard::tabItem(
      tabName = "cdm",
      shiny::fluidRow(column(
        width = 12, align = "center",
        h2("Chronic Disease Management items")
      )),
      shiny::fluidRow(column(
        width = 12,
        shiny::div(
          id = "cdm_datatable_wrapper",
          dMeasureCDM::datatableUI("CDM_dt")
        ))
      )
    )
  )
  return(x)
}

#' Chronic Disease Management (CDM) module - UI function
#'
#' Display CDM status and opportunities within selected range of
#' dates and providers
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return Shiny user interface element
#'
#' @export
datatableUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shinyWidgets::switchInput(
          inputId = ns("printcopy_view"),
          label = paste(
            "<i class=\"fas fa-print\"></i>",
            "<i class=\"far fa-copy\"></i>",
            " Print and Copy View"
          ),
          labelWidth = "12em",
          width = "20em"
        )
      ),
      shiny::column(
        width = 2,
        offset = 2,
        shinyWidgets::pickerInput(
          inputId = ns("appointment_contact_view"),
          choices = c("Appointment view", "Contact view"),
          choicesOpt = list(icon = c("fa fa-calendar-alt", "fa fa-handshake"))
        )
      ),
      shiny::column(
        width = 2,
        offset = 2, # note that total 'column' width = 12
        shiny::uiOutput(ns("cdm_item_choice"))
      )
    ),
    DT::DTOutput(ns("cdm_table"))
  )
}

#' Chronic disease management list module - server
#'
#' chronic disease management items claimed, pending or unclaimed
#' for appointment list
#'
#' @param id id
#' @param dMCDM dMeasureCDM R6 object
#'  access to appointments lists, condition lists, history and EMR database
#'
#' @return none
#'
#' @export
datatableServer <- function(id, dMCDM) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    cdm_chosen <- shiny::reactiveVal(
      setdiff(cdm_item_names, c("DiabetesSIP", "AsthmaSIP"))
    ) # cdm_item_names are defined in CDM.R as global
    itemstatus_chosen <- shiny::reactiveVal(
      unlist(item_status, use.names = FALSE)
      # item_status defined in CDM.R as global
    )
    output$cdm_item_choice <- renderUI({
      shinyWidgets::dropMenu(
        shiny::actionButton(
          inputId = ns("choice_dropdown"),
          icon = icon("gear"),
          label = "CDM settings"
        ),
        shiny::tags$div(
          shinyWidgets::checkboxGroupButtons(
            inputId = ns("cdm_chosen"), label = "CDM items shown",
            choices = cdm_item_names,
            selected = cdm_chosen(),
            # DiabetesSIP and AsthmaSIP no longer valid items :(
            status = "primary",
            checkIcon = list(yes = icon("ok", lib = "glyphicon"))
          ),
          shiny::hr(),
          shinyWidgets::checkboxGroupButtons(
            inputId = ns("itemstatus_chosen"), label = "Item status shown",
            choices = unlist(item_status, use.names = FALSE),
            # item_status defined in CDM.R
            selected = itemstatus_chosen(),
            status = "primary",
            checkIcon = list(yes = icon("ok", lib = "glyphicon"))
          ),
          shiny::br(),
          shiny::em("Close to confirm")
        ),
        placement = "bottom-end"
      )
    })
    shiny::observeEvent(
      input$choice_dropdown_dropmenu,
      ignoreInit = TRUE, {
        # this is triggered when shinyWidgets::dropMenu is opened/closed
        # tag is derived from the first tag in dropMenu, adding '_dropmenu'
        if (!input$choice_dropdown_dropmenu) {
          # only if closing the 'dropmenu' modal
          # unfortunately, is also triggered during Init (despite the ignoreInit)
          cdm_chosen(input$cdm_chosen)
          itemstatus_chosen(input$itemstatus_chosen)
        }
      }
    )

    # filter to CDM item billed prior to (or on) the
    #  day of displayed appointments
    # only show most recent billed item in each category

    billings_cdm_list <-
      shiny::eventReactive(
        c(
          dMCDM$dM$appointments_filteredR(),
          dMCDM$dM$contact_count_listR(),
          dMCDM$dM$dateformat(),
          cdm_chosen(),
          itemstatus_chosen(),
          input$appointment_contact_view,
          input$printcopy_view
        ), ignoreInit = FALSE, {
          shiny::req(dMCDM$dM$clinicians) # need some clinicians defined!
          shiny::validate(
            shiny::need(
              dMCDM$dMBillings$appointments_billingsR(),
              "No appointments defined"
            )
          )
          # respond to appointments_filteredR, since that is what is changed
          # when clinician or dates is changed
          if (input$appointment_contact_view == "Appointment view") {
            intID <- NULL
          } else {
            intID <- dMCDM$dM$contact_count_listR() %>>%
              dplyr::pull(InternalID)
          }

          billings_cdm_list <- dMCDM$billings_cdm(
            # called with 'appointment' method if intID = NULL,
            # otherwise called with 'intID' defined
            intID = intID, intID_Date = Sys.Date(),
            cdm_chosen = cdm_chosen(),
            itemstatus_chosen = itemstatus_chosen(),
            lazy = TRUE, # no need to re-calculate $appointments_billings
            screentag = !input$printcopy_view,
            screentag_print = input$printcopy_view
          )

          return(billings_cdm_list)
        }
      )

    ### create tag-styled datatable (or 'printable' datatable)

    cdm_styled_datatable <- shiny::reactive({
      shiny::validate(
        shiny::need(
          dMCDM$dM$appointments_filtered_timeR(),
          "No appointments in selected range"
        )
      )

      if (requireNamespace("lubridate", quietly = TRUE)) {
        dateformat <- lubridate::stamp_date(dMCDM$dM$dateformat())
        # formats date into desired format
        # this is 'reactive'
      } else {
        # if no lubridate library is available then, just return the date
        dateformat <- function(x) {as.character(x)}
      }

      if (!is.null(billings_cdm_list()) &
          !is.null(dMCDM$dM$appointments_filtered_timeR())) {
        if (input$appointment_contact_view == "Appointment view") {
          cdm_list <- dMCDM$dM$appointments_filtered_timeR() %>>%
            dplyr::inner_join(
              billings_cdm_list(),
              by = c(
                "InternalID", "AppointmentDate",
                "AppointmentTime", "Provider"
              )
            )
          if (input$printcopy_view == TRUE) {
            # printable/copyable view
            DailyMeasure::datatable_styled(
              cdm_list %>>%
                dplyr::select(
                  Patient, AppointmentDate, AppointmentTime,
                  Provider, cdm_print
                ) %>>%
                dplyr::mutate(
                  AppointmentDate = dateformat(AppointmentDate)
                ),
              colnames = c(
                "Patient", "Appointment Date", "Appointment Time",
                "Provider", "CDM items"
              )
            )
          } else {
            # fomantic/semantic tag view
            DailyMeasure::datatable_styled(
              cdm_list %>>%
                dplyr::select(
                  Patient, AppointmentDate, AppointmentTime,
                  Provider, cdm
                ) %>>%
                dplyr::mutate(
                  AppointmentDate = dateformat(AppointmentDate)
                ),
              colnames = c(
                "Patient", "Appointment Date", "Appointment Time",
                "Provider", "CDM items"
              ),
              printButton = NULL, copyHtml5 = NULL,
              downloadButton = NULL, # no copy/print buttons
              escape = c(5)
            ) # only interpret HTML for last column
          }
        } else {
          # 'Contact view'
          intID <- billings_cdm_list() %>>%
            dplyr::pull(InternalID)
          cdm_list <- billings_cdm_list() %>>%
            dplyr::left_join(
              dMCDM$dM$db$patients %>>% # attach names etc.
                dplyr::filter(InternalID %in% c(intID, -1)) %>>%
                dplyr::select(
                  InternalID, ExternalID, DOB,
                  Firstname, Surname,
                  HomePhone, MobilePhone
                ) %>>%
                dplyr::collect() %>>%
                dplyr::mutate(DOB = as.Date(DOB), Date = Sys.Date()) %>>%
                # initially Date is a dttm (POSIXt) object,
                # which makes the subsequent calc_age very slow,
                # and throws up warnings
                dplyr::mutate(
                  Age = dMeasure::calc_age(DOB, Date),
                  Patient = paste(Firstname, Surname)
                ),
              by = "InternalID"
            )
          if (input$printcopy_view == TRUE) {
            # printable/copyable view
            DailyMeasure::datatable_styled(
              cdm_list %>>%
                dplyr::select(
                  Patient, ExternalID, DOB, Age,
                  HomePhone, MobilePhone, cdm_print
                ) %>>%
                dplyr::mutate(
                  DOB = dateformat(DOB)
                ),
              colnames = c(
                "Patient", "ExternalID", "DOB", "Age",
                "Home Phone", "Mobile Phone", "CDM items"
              )
            )
          } else {
            # fomantic/semantic tag view
            DailyMeasure::datatable_styled(
              cdm_list %>>%
                dplyr::select(
                  Patient, ExternalID, DOB, Age,
                  HomePhone, MobilePhone, cdm
                ) %>>%
                dplyr::mutate(
                  DOB = dateformat(DOB)
                ),
              colnames = c(
                "Patient", "ExternalID", "DOB", "Age",
                "Home Phone", "Mobile Phone", "CDM items"
              ),
              printButton = NULL, copyHtml5 = NULL,
              downloadButton = NULL, # no copy/print buttons
              escape = c(7)
            ) # only interpret HTML for last column
          }
        }
      }
    })

    output$cdm_table <- DT::renderDT({
      cdm_styled_datatable()
    },
    server = TRUE
    )
  })
}
