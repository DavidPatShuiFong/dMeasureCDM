##### Chronic Disease Management item numbers ###########################################
#' Chronic Disease Management (CDM) item numbers
#'
#' @name cdm_methods
#' @include CDM.R
#' 'tags only' fomantic/semantic definitions
NULL

##### constants #########################################################################
# MBS (medicare benefits schedule) item numbers for CDM
cdm_item <- data.frame(
  code = c(
    721, 92024, 92068, 723, 92025, 92069, 732, 92028, 92072,
    703, 705, 707,
    2517, 2521, 2525,
    2546, 2552, 2558,
    2700, 2701, 92112, 92124, 92113, 92125,
    2715, 2717, 92116, 92128, 92117, 92129
  ),
  name = c(
    "GPMP", "GPMP", "GPMP", "TCA", "TCA", "TCA", "GPMP R/V", "GPMP R/v", "GPMP R/V",
    "HA", "HA", "HA",
    "DiabetesSIP", "DiabetesSIP", "DiabetesSIP",
    "AsthmaSIP", "AsthmaSIP", "AsthmaSIP",
    "MHCP", "MHCP", "MHCP", "MHCP", "MHCP", "MHCP",
    "MHCP", "MHCP", "MHCP", "MHCP", "MHCP", "MHCP"
  )
)

cdm_item_names <- as.character(unique(cdm_item$name)) # de-factored and unique cdm_item$name

##### fields ############################################################################

### AHA 75 (annual health assessment for those aged 75 years and above)
.public(dMeasureCDM, "aha75_list_cdm", function(intID_list) {
  intID_list %>>%
    dplyr::filter(Age >= 75) %>>%
    dplyr::select(c("InternalID", "AppointmentDate", "AppointmentTime", "Provider")) %>>%
    dplyr::mutate(
      MBSName = c("HA"), Description = c("Age 75 years or older"),
      ServiceDate = as.Date(-Inf, origin = "1970-01-01"), MBSItem = NA
    ) %>>%
    unique()
})

### Diabetes list for CDM
.public(dMeasureCDM, "diabetes_list_cdm", function(intID_list) {
  a <- intID_list %>>% {
    dplyr::filter(., InternalID %in%
      self$dM$diabetes_list(. %>>%
        dplyr::select(InternalID, AppointmentDate) %>>%
        dplyr::rename(Date = AppointmentDate)))
  } %>>%
    dplyr::select(c("InternalID", "AppointmentDate", "AppointmentTime", "Provider")) %>>%
    dplyr::mutate(
      MBSName = c("DiabetesSIP"), Description = c("History : Diabetes"),
      ServiceDate = as.Date(-Inf, origin = "1970-01-01"), MBSItem = NA
    ) %>>%
    unique()
  # invalid date set as -Inf, which looks like NA, but is not (is equal to -Inf)
  # setting invalid date to NA is not good for later comparisons,
  # where max(... , na.rm=TRUE) needs to be used

  b <- a %>>% dplyr::mutate(MBSName = "GPMP")
  # people with diabetes also qualify for GPMP. duplicate list with 'GPMP' MBSName
  rbind(a, b)
})

### asthma list for CDM
.public(dMeasureCDM, "asthma_list_cdm", function(intID_list) {
  a <- intID_list %>>% {
    dplyr::filter(., InternalID %in%
      self$dM$asthma_list(. %>>%
        dplyr::select(InternalID, AppointmentDate) %>>%
        dplyr::rename(Date = AppointmentDate)))
  } %>>%
    dplyr::select(c("InternalID", "AppointmentDate", "AppointmentTime", "Provider")) %>>%
    dplyr::mutate(
      MBSName = c("AsthmaSIP"), Description = c("History : Asthma"),
      ServiceDate = as.Date(-Inf, origin = "1970-01-01"), MBSItem = NA
    ) %>>%
    unique()
  # invalid date set as -Inf, which looks like NA, but is not (is equal to -Inf)
  # setting invalid date to NA is not good for later comparisons,
  # where max(... , na.rm=TRUE) needs to be used

  b <- a %>>% dplyr::mutate(MBSName = c("GPMP"))
  # people with asthma also qualify for GPMP. duplicate list with 'GPMP' MBSName
  rbind(a, b)
})

### malignancy list for CDM
.public(dMeasureCDM, "malignancy_list_cdm", function(intID_list) {
  a <- intID_list %>>% {
    dplyr::filter(., InternalID %in%
      self$dM$malignancy_list(. %>>%
        dplyr::select(InternalID, AppointmentDate) %>>%
        dplyr::rename(Date = AppointmentDate)))
  } %>>%
    dplyr::select(c("InternalID", "AppointmentDate", "AppointmentTime", "Provider")) %>>%
    dplyr::mutate(
      MBSName = c("GPMP"), Description = c("History : Malignancy"),
      ServiceDate = as.Date(-Inf, origin = "1970-01-01"), MBSItem = NA
    ) %>>%
    unique()
  # invalid date set as -Inf, which looks like NA, but is not (is equal to -Inf)
  # setting invalid date to NA is not good for later comparisons,
  # where max(... , na.rm=TRUE) needs to be used

  return(a)
})

### hiv list for CDM
.public(dMeasureCDM, "hiv_list_cdm", function(intID_list) {
  a <- intID_list %>>% {
    dplyr::filter(., InternalID %in%
      self$dM$hiv_list(. %>>%
        dplyr::select(InternalID, AppointmentDate) %>>%
        dplyr::rename(Date = AppointmentDate)))
  } %>>%
    dplyr::select(c("InternalID", "AppointmentDate", "AppointmentTime", "Provider")) %>>%
    dplyr::mutate(
      MBSName = c("GPMP"), Description = c("History : HIV"),
      ServiceDate = as.Date(-Inf, origin = "1970-01-01"), MBSItem = NA
    ) %>>%
    unique()
  # invalid date set as -Inf, which looks like NA, but is not (is equal to -Inf)
  # setting invalid date to NA is not good for later comparisons,
  # where max(... , na.rm=TRUE) needs to be used

  return(a)
})

### haemoglobinopathy list for CDM
.public(dMeasureCDM, "haemoglobinopathy_list_cdm", function(intID_list) {
  a <- intID_list %>>% {
    dplyr::filter(., InternalID %in%
      self$dM$haemoglobinopathy_list(. %>>%
        dplyr::select(InternalID, AppointmentDate) %>>%
        dplyr::rename(Date = AppointmentDate)))
  } %>>%
    dplyr::select(c("InternalID", "AppointmentDate", "AppointmentTime", "Provider")) %>>%
    dplyr::mutate(
      MBSName = c("GPMP"), Description = c("History : Haemoglobinopathy"),
      ServiceDate = as.Date(-Inf, origin = "1970-01-01"), MBSItem = NA
    ) %>>%
    unique()
  # invalid date set as -Inf, which looks like NA, but is not (is equal to -Inf)
  # setting invalid date to NA is not good for later comparisons,
  # where max(... , na.rm=TRUE) needs to be used

  return(a)
})

### asplenic list for CDM
.public(dMeasureCDM, "asplenic_list_cdm", function(intID_list) {
  a <- intID_list %>>% {
    dplyr::filter(., InternalID %in%
      self$dM$asplenic_list(. %>>%
        dplyr::select(InternalID, AppointmentDate) %>>%
        dplyr::rename(Date = AppointmentDate)))
  } %>>%
    dplyr::select(c("InternalID", "AppointmentDate", "AppointmentTime", "Provider")) %>>%
    dplyr::mutate(
      MBSName = c("GPMP"), Description = c("History : Asplenia"),
      ServiceDate = as.Date(-Inf, origin = "1970-01-01"), MBSItem = NA
    ) %>>%
    unique()
  # invalid date set as -Inf, which looks like NA, but is not (is equal to -Inf)
  # setting invalid date to NA is not good for later comparisons,
  # where max(... , na.rm=TRUE) needs to be used

  return(a)
})

### transplant list for CDM
.public(dMeasureCDM, "transplant_list_cdm", function(intID_list) {
  a <- intID_list %>>% {
    dplyr::filter(., InternalID %in%
      self$dM$transplant_list(. %>>%
        dplyr::select(InternalID, AppointmentDate) %>>%
        dplyr::rename(Date = AppointmentDate)))
  } %>>%
    dplyr::select(c("InternalID", "AppointmentDate", "AppointmentTime", "Provider")) %>>%
    dplyr::mutate(
      MBSName = c("GPMP"), Description = c("History : transplant"),
      ServiceDate = as.Date(-Inf, origin = "1970-01-01"), MBSItem = NA
    ) %>>%
    unique()
  # invalid date set as -Inf, which looks like NA, but is not (is equal to -Inf)
  # setting invalid date to NA is not good for later comparisons,
  # where max(... , na.rm=TRUE) needs to be used

  return(a)
})

### trisomy21 list for CDM
.public(dMeasureCDM, "trisomy21_list_cdm", function(intID_list) {
  a <- intID_list %>>% {
    dplyr::filter(., InternalID %in%
      self$dM$trisomy21_list(. %>>%
        dplyr::select(InternalID, AppointmentDate) %>>%
        dplyr::rename(Date = AppointmentDate)))
  } %>>%
    dplyr::select(c("InternalID", "AppointmentDate", "AppointmentTime", "Provider")) %>>%
    dplyr::mutate(
      MBSName = c("GPMP"), Description = c("History : Trisomy 21"),
      ServiceDate = as.Date(-Inf, origin = "1970-01-01"), MBSItem = NA
    ) %>>%
    unique()
  # invalid date set as -Inf, which looks like NA, but is not (is equal to -Inf)
  # setting invalid date to NA is not good for later comparisons,
  # where max(... , na.rm=TRUE) needs to be used

  return(a)
})

### chroniclungdisease list for CDM
.public(dMeasureCDM, "chroniclungdisease_list_cdm", function(intID_list) {
  a <- intID_list %>>% {
    dplyr::filter(., InternalID %in%
      self$dM$chroniclungdisease_list(. %>>%
        dplyr::select(InternalID, AppointmentDate) %>>%
        dplyr::rename(Date = AppointmentDate)))
  } %>>%
    dplyr::select(c("InternalID", "AppointmentDate", "AppointmentTime", "Provider")) %>>%
    dplyr::mutate(
      MBSName = c("GPMP"), Description = c("History : Chronic Lung Disease"),
      ServiceDate = as.Date(-Inf, origin = "1970-01-01"), MBSItem = NA
    ) %>>%
    unique()
  # invalid date set as -Inf, which looks like NA, but is not (is equal to -Inf)
  # setting invalid date to NA is not good for later comparisons,
  # where max(... , na.rm=TRUE) needs to be used

  return(a)
})

### neurologic list for CDM
.public(dMeasureCDM, "neurologic_list_cdm", function(intID_list) {
  a <- intID_list %>>% {
    dplyr::filter(., InternalID %in%
      self$dM$neurologic_list(. %>>%
        dplyr::select(InternalID, AppointmentDate) %>>%
        dplyr::rename(Date = AppointmentDate)))
  } %>>%
    dplyr::select(c("InternalID", "AppointmentDate", "AppointmentTime", "Provider")) %>>%
    dplyr::mutate(
      MBSName = c("GPMP"), Description = c("History : Neurologic condition"),
      ServiceDate = as.Date(-Inf, origin = "1970-01-01"), MBSItem = NA
    ) %>>%
    unique()
  # invalid date set as -Inf, which looks like NA, but is not (is equal to -Inf)
  # setting invalid date to NA is not good for later comparisons,
  # where max(... , na.rm=TRUE) needs to be used

  return(a)
})

### chronic liver disease list for CDM
.public(dMeasureCDM, "chronicliverdisease_list_cdm", function(intID_list) {
  a <- intID_list %>>% {
    dplyr::filter(., InternalID %in%
      self$dM$chronicliverdisease_list(. %>>%
        dplyr::select(InternalID, AppointmentDate) %>>%
        dplyr::rename(Date = AppointmentDate)))
  } %>>%
    dplyr::select(c("InternalID", "AppointmentDate", "AppointmentTime", "Provider")) %>>%
    dplyr::mutate(
      MBSName = c("GPMP"), Description = c("History : Liver Disease"),
      ServiceDate = as.Date(-Inf, origin = "1970-01-01"), MBSItem = NA
    ) %>>%
    unique()
  # invalid date set as -Inf, which looks like NA, but is not (is equal to -Inf)
  # setting invalid date to NA is not good for later comparisons,
  # where max(... , na.rm=TRUE) needs to be used

  return(a)
})

### chronicrenaldisease list for CDM
.public(dMeasureCDM, "chronicrenaldisease_list_cdm", function(intID_list) {
  a <- intID_list %>>% {
    dplyr::filter(., InternalID %in%
      self$dM$chronicrenaldisease_list(. %>>%
        dplyr::select(InternalID, AppointmentDate) %>>%
        dplyr::rename(Date = AppointmentDate)))
  } %>>%
    dplyr::select(c("InternalID", "AppointmentDate", "AppointmentTime", "Provider")) %>>%
    dplyr::mutate(
      MBSName = c("GPMP"), Description = c("History : Chronic Renal Disease"),
      ServiceDate = as.Date(-Inf, origin = "1970-01-01"), MBSItem = NA
    ) %>>%
    unique()
  # invalid date set as -Inf, which looks like NA, but is not (is equal to -Inf)
  # setting invalid date to NA is not good for later comparisons,
  # where max(... , na.rm=TRUE) needs to be used

  return(a)
})

### cardiacdisease list for CDM
.public(dMeasureCDM, "cardiacdisease_list_cdm", function(intID_list) {
  a <- intID_list %>>% {
    dplyr::filter(., InternalID %in%
      self$dM$cardiacdisease_list(. %>>%
        dplyr::select(InternalID, AppointmentDate) %>>%
        dplyr::rename(Date = AppointmentDate)))
  } %>>%
    dplyr::select(c("InternalID", "AppointmentDate", "AppointmentTime", "Provider")) %>>%
    dplyr::mutate(
      MBSName = c("GPMP"), Description = c("History : Cardiac Disease"),
      ServiceDate = as.Date(-Inf, origin = "1970-01-01"), MBSItem = NA
    ) %>>%
    unique()
  # invalid date set as -Inf, which looks like NA, but is not (is equal to -Inf)
  # setting invalid date to NA is not good for later comparisons,
  # where max(... , na.rm=TRUE) needs to be used

  return(a)
})


##### methods ###########################################################################

#' CDM item status of patients in appointment view
#'
#' filter to CDM item billed prior to (or on) the day of displayed appointments
#' only show most recent billed item in each category
#'
#' @param dMeasureCDM_obj dMeasureCDM R6 object
#' @param date_from $date_a (from dMeasure) start date
#' @param date_to $date_b end date (inclusive)
#' @param clinicians $clinicians (from dMeasure) list of clinicians to view
#' @param intID vector of internal ID (default is NULL, in which case appointments_list is used)
#' @param intID_Date if intID is not NULL, then date to check (default is Sys.Date())
#'   intID_Date can be a single value (applies to all intID), or a vector (different
#'   date can be defined for each intID) the same length as intID.
#' @param cdm_chosen (defaut cdm_item_names) item types to show, defaults to all available
#' @param lazy if TRUE, then do not recalculate appointment list. otherwise, re-calculate
#' @param screentag (default FALSE) optionally add a fomantic/semantic HTML description of 'action'
#' @param screentag_print (default TRUE) optionally add a 'printable' description of 'action'
#'
#' @return list of appointments (with patient details)
#'   returns empty dataframe if no valid subscription for chosen clinicians
#'   and selected date-range
billings_cdm <- function(dMeasureCDM_obj, date_from = NA, date_to = NA, clinicians = NA,
                         intID = NULL, intID_Date = Sys.Date(),
                         cdm_chosen = cdm_item_names,
                         lazy = FALSE,
                         screentag = FALSE, screentag_print = TRUE) {
  dMeasureCDM_obj$billings_cdm(
    date_from, date_to, clinicians, intID, intID_Date,
    cdm_chosen, lazy, screentag, screentag_print
  )
}
.public(
  dMeasureCDM, "billings_cdm",
  function(date_from = NA, date_to = NA, clinicians = NA,
             intID = NULL, intID_Date = Sys.Date(),
             cdm_chosen = cdm_item_names,
             lazy = FALSE,
             screentag = FALSE, screentag_print = TRUE) {

    if (!is.null(intID)) {
      if (length(intID) == 0) {
        # intID is defined, but is length 0! e.g. numeric(0)
        intID <- -1 # assign a dummy value
      }
      if (length(intID_Date) > 1 &&
          length(intID_Date) != length(intID))
        stop("Length of intID_Date must either be '1' (one) or ",
             "the same length as intID")
    }

    if (is.na(date_from)) {
      date_from <- self$dM$date_a
    }
    if (is.na(date_to)) {
      date_to <- self$dM$date_b
    }
    if (all(is.na(clinicians))) {
      clinicians <- self$dM$clinicians
    }
    # no additional clinician filtering based on privileges or user restrictions

    if (all(is.na(clinicians)) || length(clinicians) == 0) {
      clinicians <- c("") # dplyr::filter cannot handle empty list()
    }

    return_empty_dataframe <- function(intID, screentag, screentag_print) {
      if (is.null(intID)) {
        # appointment list
        billings_list <- data.frame(
          Patient = character(),
          InternalID = integer(),
          DOB = as.Date(integer(0), origin = "1970-01-01"),
          AppointmentDate = as.Date(integer(0), origin = "1970-01-01"),
          AppointmentTime = character(),
          Status = character(),
          Provider = character(),
          ServiceDate = as.Date(integer(0), origin = "1970-01-01"),
          MBSItem = numeric(0),
          Description = character()
        )
      } else {
        billings_list <- data.frame(
          InternalID = integer(),
          AppointmentDate = as.Date(integer(0), origin = "1970-01-01"),
          AppointmentTime = character(),
          Provider = character(),
          ServiceDate = as.Date(integer(0), origin = "1970-01-01"),
          MBSItem = numeric(0),
          Description = character()
        )
      }
      if (screentag) {
        billings_list <- cbind(billings_list, data.frame(cdm = character()))
      }
      if (screentag_print) {
        billings_list <- cbind(billings_list, data.frame(cdm_print = character()))
      }
      return(billings_list)
    }
    if (!self$dM$emr_db$is_open()) {
      # EMR database is not open
      # create empty data-frame to return
      billings_list <- return_empty_dataframe(intID, screentag, screentag_print)
    } else {
      # only if EMR database is open
      if (is.null(intID)) {
        adjust_days <- 7
      } else {
        adjust_days <- 120
      }
      # if appointment view and no valid subscription,
      # minimum seven days old
      # if contact view and no valid subscription,
      # minimum one hundred and twenty days old
      x <- self$dM$check_subscription(
        clinicians,
        date_from, date_to,
        adjust_days = adjust_days
      )
      # check subscription, which depends on selected
      # clinicians and selected date range
      if (x$changedate) {
        # if a user without valid subscription for chosen date range
        # then return an empty dataframe
        billings_list <- return_empty_dataframe(intID, screentag, screentag_print)
      } else {
        if (is.null(intID)) {
          # appointment list
          if (!lazy) {
            self$dMBillings$billed_appointments(
              date_from, date_to,
              clinicians,
              lazy = FALSE
            )
          }
          # if not 'lazy' evaluation, then re-calculate self$appointments_billings
          # (that is automatically done by calling the $billed_appointments method)
          billings_list <- self$dMBillings$appointments_billings %>>%
            dplyr::filter(
              MBSItem %in% cdm_item$code,
              # only chronic disease management items
              # only items billed before the appointment day
              ServiceDate <= AppointmentDate
            )
        } else {
          max_intID_Date <- max(intID_Date)
          # SQL has problem with max dates
          cdm_codes <- cdm_item$code
          billings_list <- self$dM$db$services %>>%
            dplyr::filter(
              InternalID %in% c(intID, -1),
              MBSItem %in% cdm_codes,
              ServiceDate <= max_intID_Date
              # intID_Date could be a single date, but can also filter
              # if intID_Date is a vector of dates
            ) %>>%
            dplyr::collect() %>>%
            dplyr::mutate(
              ServiceDate = as.Date(ServiceDate),
              AppointmentTime = as.character(NA),
              Provider = as.character(NA)
              # dummy columns, remove later
            )
          # add AppointmentDate, which will be used to compare ServiceDate
          if (length(intID_Date) > 1 && length(intID_Date) == length(intID)) {
            # intID_Date is a vector, which needs to be as long as intID
            billings_list <- billings_list %>>%
              dplyr::left_join(
                data.frame(InternalID = intID, AppointmentDate = intID_Date),
                by = "InternalID") %>>%
              dplyr::filter(AppointmentDate >= ServiceDate)
                # this might filter out some more entries if
                # intID_Date is a vector, rather than a single value
          } else {
            billings_list <- billings_list %>>%
              dplyr::mutate(AppointmentDate = intID_Date)
            # intID_date is only a single value
          }
        }
      }

      billings_list <- billings_list %>>%
        dplyr::select(intersect(
          names(billings_list),
          c(
            "InternalID", "AppointmentDate", "AppointmentTime", "Provider",
            "ServiceDate", "MBSItem", "Description"
          )
        )) %>>%
        dplyr::mutate(MBSName = cdm_item$name[match(MBSItem, cdm_item$code)])

      if ("GPMP R/V" %in% cdm_chosen) {
        gpmprv <- billings_list %>>%
          # GPMP R/V tags.
          # unlike other items, this is on a 3 month schedule, and can follow
          # an item 'other' than itself (e.g. it can follow a GPMP or TCA)
          #
          # only show if a GPMP R/V is due (greater than three months since gpmp or tca or gpmp r/v)
          # or if GPMP R/V is the most recent of gpmp/tca/gpmp r/v
          #
          # green if 'up-to-date' (GPMP R/V is the most recent, and less than 3/months)
          # yellow if 'done, but old' (GPMP R/V is the most recent, and more than 3/months)
          # red if 'not done' (GPMP/TCA most recent, and more than three)
          dplyr::filter(MBSName %in% c("GPMP", "TCA", "GPMP R/V")) %>>%
          # r/v only applies if gpmp/tca or r/v already claimed
          dplyr::group_by(InternalID, AppointmentDate, AppointmentTime, Provider) %>>%
          # group by appointment
          dplyr::slice(which.max(ServiceDate)) %>>%
          dplyr::ungroup() %>>%
          # (one) item with latest servicedate
          dplyr::filter((MBSName == "GPMP R/V") |
            dMeasure::interval(ServiceDate, AppointmentDate, unit = "month")$month >= 3)
        # minimum 3-month gap since claiming previous GPMP/TCA,
        # or most recent claim is a GPMP R/V

        # add screentags as necessary
        if (screentag) {
          gpmprv <- gpmprv %>>%
            dplyr::mutate(
              mbstag =
                dMeasure::semantic_tag(
                  "GPMP R/V", # semantic/fomantic buttons
                  colour =
                    dplyr::if_else(
                      MBSName %in% c("GPMP", "TCA"),
                      "red",
                      # no GPMP R/V since the last GPMP/TCA
                      dplyr::if_else(
                        dMeasure::interval(ServiceDate, AppointmentDate, unit = "month")$month >= 3,
                        # GPMP R/V. Less than or more than 3 months?
                        "yellow",
                        "green"
                      )
                    ),
                  popuphtml =
                    paste0(
                      "<h4>Date : ", ServiceDate,
                      "</h4><h6>Item : ", MBSItem,
                      "</h6><p><font size=\'+0\'>", Description, "</p>"
                    )
                )
            )
        }
        if (screentag_print) {
          gpmprv <- gpmprv %>>%
            dplyr::mutate(
              mbstag_print =
                paste0(
                  "GPMP R/V", " ", # printable version of information
                  dplyr::if_else(
                    MBSName %in% c("GPMP", "TCA"),
                    paste0("(", MBSName, ": ", ServiceDate, ") Overdue"),
                    dplyr::if_else(
                      dMeasure::interval(ServiceDate, AppointmentDate, unit = "month")$month >= 3,
                      paste0("(", ServiceDate, ") Overdue"),
                      paste0("(", ServiceDate, ")")
                    )
                  )
                )
            )
        }
      } else {
        gpmprv <- NULL
      }

      if (x$changedate) {
        intID_list <- c(-1) # no valid subscription, don't do any searches!
      } else {
        if (is.null(intID)) {
          intID_list <- self$dM$appointments_list %>>%
            dplyr::select(InternalID, AppointmentDate, AppointmentTime, Provider, Age)
        } else {
          intID_list <- self$dM$db$patients %>>% # check the age of the intID list
            dplyr::filter(InternalID %in% c(intID, -1)) %>>%
            dplyr::select(InternalID, DOB) %>>%
            dplyr::collect() %>>%
            dplyr::mutate(DOB = as.Date(DOB)) %>>%
            dplyr::left_join(
              data.frame(InternalID = intID, Date = intID_Date),
              by = "InternalID") %>>%
            # intID_Date could either be a single date or vector of dates
            # initially Date is a dttm (POSIXt) object,
            # which makes the subsequent calc_age very slow,
            # and throws up warnings
            dplyr::mutate(
              Age = dMeasure::calc_age(DOB, Date),
              AppointmentDate = Date,
              AppointmentTime = as.character(NA),
              Provider = as.character(NA)
            ) %>>%
            dplyr::select(-DOB)
        }
      }

      if (!x$changedate) { # only search if valid subscription!
        billings_list <- billings_list %>>%
          dplyr::filter(!(MBSName == "GPMP R/V")) %>>% # GPMP R/V will be added back in as a 'tagged' version
          rbind(self$diabetes_list_cdm(intID_list)) %>>%
          rbind(self$asthma_list_cdm(intID_list)) %>>%
          rbind(self$malignancy_list_cdm(intID_list)) %>>%
          rbind(self$hiv_list_cdm(intID_list)) %>>%
          rbind(self$haemoglobinopathy_list_cdm(intID_list)) %>>%
          rbind(self$asplenic_list_cdm(intID_list)) %>>%
          rbind(self$transplant_list_cdm(intID_list)) %>>%
          rbind(self$chronicliverdisease_list_cdm(intID_list)) %>>%
          rbind(self$chronicrenaldisease_list_cdm(intID_list)) %>>%
          rbind(self$chroniclungdisease_list_cdm(intID_list)) %>>%
          rbind(self$neurologic_list_cdm(intID_list)) %>>%
          rbind(self$trisomy21_list_cdm(intID_list)) %>>%
          rbind(self$cardiacdisease_list_cdm(intID_list)) %>>%
          rbind(self$aha75_list_cdm(intID_list)) %>>%
          dplyr::filter(MBSName %in% cdm_chosen) %>>%
          dplyr::group_by(InternalID, AppointmentDate, AppointmentTime, Provider, MBSName) %>>%
          # group by patient, appointment and CDM type (name)
          dplyr::mutate(Description = dMeasure::paste2(
            Description, collapse = ", ", na.rm = TRUE
          )) %>>%
          # group the reasons/description together
          dplyr::filter(ServiceDate == max(ServiceDate, na.rm = TRUE)) %>>%
          # only keep most recent service
          dplyr::ungroup()
      }

      if (screentag) {
        billings_list <- billings_list %>>%
          dplyr::mutate(
            mbstag =
              dMeasure::semantic_tag(MBSName, # semantic/fomantic buttons
                colour =
                  dplyr::if_else(
                    ServiceDate == -Inf,
                    "red",
                    # invalid date is '-Inf', means item not claimed yet
                    dplyr::if_else(
                      dMeasure::interval(ServiceDate, AppointmentDate)$year < 1,
                      "green",
                      "yellow"
                    )
                  ),
                popuphtml =
                  paste0(
                    "<h4>Date : ", ServiceDate,
                    "</h4><h6>Item : ", MBSItem,
                    "</h6><p><font size=\'+0\'>", Description, "</p>"
                  )
              )
          )
      }

      if (screentag_print) {
        billings_list <- billings_list %>>%
          dplyr::mutate(mbstag_print = paste0(
            MBSName, # printable version of information
            dplyr::if_else(
              ServiceDate == -Inf,
              paste0(" (", Description, ")"),
              paste0(
                " (", ServiceDate, " : ", Description, ")",
                dplyr::if_else(
                  dMeasure::interval(ServiceDate, AppointmentDate)$year < 1,
                  "",
                  " Overdue"
                )
              )
            )
          ))
      }

      billings_list <- billings_list %>>%
        rbind(gpmprv) %>>% # add in GPMP reviews
        dplyr::group_by(InternalID, AppointmentDate, AppointmentTime, Provider) %>>%
        # gathers item numbers on the same day into a single row
        {
          if (screentag) {
            dplyr::summarise(., cdm = paste(mbstag, collapse = ""))
          }
          else {
            .
          }
        } %>>% {
          if (screentag_print) {
            dplyr::summarise(.,
              cdm_print =
                paste(mbstag_print, collapse = ", ")
            )
          }
          else {
            .
          }
        } %>>%
        dplyr::ungroup()
    }

    if (!is.null(intID)) {
      billings_list <- billings_list %>>%
        dplyr::select(intersect(
          names(billings_list),
          c("InternalID", "cdm", "cdm_print")
        ))
    }

    return(billings_list)
  }
)
