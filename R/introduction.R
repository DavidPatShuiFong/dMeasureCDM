# rintrojs introduction for CDM

#' rintrojs steps for CDM module
#'
#' @title steps_introduction_df
#' @param element_name the HTML datatable element, including datatable and helpers such as print/copy view
#' @description returns a dataframe of rintrojs steps
#'
#' requires pro-forma steps from DailyMeasure
#'
#' @export
steps_introduction_df <- function(element_name) {
  steps_df <-
    data.frame(
      element = as.character(NA),
      intro = c(paste(
        shiny::tags$h4("GPstat! Chronic Disease Management (CDM)"),
        shiny::br(),
        "View CDM opportunities for chosen clinicians and dates.",
        shiny::br(), shiny::br(),
        "CDM opportunities can be",
        "viewed for (usually future)",
        "Appointments", shiny::icon("calendar-alt"), ",",
        "or historical Contacts", shiny::icon("handshake"), ".",
        shiny::br(), shiny::br(),
        "If a selected clinician is not registered/subscribed",
        "then CDM opportunities will only be shown if at least ninety days old."
      )),
      position = "auto",
      stringsAsFactors = FALSE
    ) %>>%
    rbind(DailyMeasure::steps_choose_clinician_date_df()) %>>%
    rbind(data.frame(
      element = element_name,
      intro = c(paste(
        shiny::tags$h4("Chronic Disease Management (CDM) view"),
        shiny::br(),
        "List of CDM opportunities",
        "according to currently selected clinicians, dates",
        "and Appointment/Contact view.",
        shiny::br(), shiny::br(),
        "By default shows : ", shiny::strong("Patient"), "(name),",
        shiny::strong("DOB/Age"), " and ",
        shiny::strong("CDM items"), " list.",
        shiny::br(), shiny::br(),
        "In Appointments", shiny::icon("calendar-alt"), "view also",
        "shows Appointment details e.g.", shiny::strong("AppointmentTime"),
        "and", shiny::strong("Provider"), " (clinician).",
        shiny::br(), shiny::br(),
        "In Contacts", shiny::icon("handshake"), "view shows",
        shiny::strong("ExternalID"), "(Patient ID), and ",
        shiny::strong("telephone contact details")
      )),
      position = "auto"
    )) %>>%
    rbind(DailyMeasure::steps_datatable_helpers(element_name)) %>>%
    rbind(data.frame(
      element = element_name,
      intro = c(paste(
        shiny::tags$h4("CDM items shown"),
        shiny::br(),
        shiny::icon("gear"), shiny::br(),
        "Top-right of the table view.",
        shiny::br(), shiny::br(),
        "Choose CDM items to display.",
        shiny::br(), shiny::br(),
        "You can try it now",
        emo::ji("smile"), "!"
      )),
      position = "auto"
    )) %>>%
    rbind(data.frame(
      element = element_name,
      intro = c(paste(
        shiny::tags$h4("Appointment/Contact view"),
        shiny::br(),
        shiny::icon("calendar-alt"), shiny::icon("handshake"), shiny::br(),
        "Top/middle of the table view.",
        shiny::br(), shiny::br(),
        "Chronic Disease Management (CDM) opportunities can be",
        "viewed for (usually future)",
        "Appointments", shiny::icon("calendar-alt"), ",",
        "or historical Contacts", shiny::icon("handshake"), ".",
        shiny::br(), shiny::br(),
        "If using Contacts", shiny::icon("handshake"), "view",
        "then the", shiny::tags$em("types"), "of contact e.g.",
        "Appointment-book, Visits (progress notes) and/or Services (billings)",
        "and", shiny::tags$em("minimum number"), "of contact (within the",
        "specified time period) is chosen with the", shiny::tags$em("Contact details"),
        shiny::icon("handshake"), "tab in the right side-bar.",
        shiny::br(), shiny::br(),
        "You can try choosing between",
        shiny::icon("calendar-alt"), "Appointment view and",
        shiny::icon("handshake"), "Contact view now",
        emo::ji("smile"), "!"
      )),
      position = "auto"
    )) %>>%
    rbind(steps_choose_contact_details_df())

  return(steps_df)
}
