# News

# 0.5.0
25th July 2020

## New

* `dMeasureIntegration` to allow auto-loading

## Changes

* update `Suggests` in `DESCRIPTION`
* remove use of `shinycssloaders`

# 0.4.3
16th July 2020

## Changes

* `billings_cdm_list` requires (`shiny::req`) non-empty `dM$clinicians`
* change dropdown to dropMenu

# 0.4.2
8th July 2020

## Changes

* change dropdown label to 'Settings'

# 0.4.1
3rd July 2020

## Changes

* CDM items shown chosen through modal
  + as a result, the displayed lists of CDM items does not change each
    time a CDM item is selected/de-selected, but only after the modal
    is closed with 'OK'

# 0.4.0
27th June 2020

## Changes

* Responsiblity for GPstat/DailyMeasure UI moved from DailyMeasure
  to dMeasureCDM
  + in file `CDM_GPstatUI.R`
  + `shinydashboardmenuItem`, `dMeasureShinytabItems`, `datatableUI`, `datatableServer`
  
## Bugfix

* billings_cdm had an error if passed a non-null `intID` of length zero 
  e.g. numeric(0)
  
# 0.3.3
20th June 2020

* `billings_cdm` now gathers together Descriptions. If a CDM
  has been claimed before, the discovered reasons for doing a
  CDM are now included in the Description.

# 0.3.2
20th May 2020

* changes in management of lack of valid subscription for
  chosen clinicians and selected date range.
  no longer tries to change the selected date range,
  instead returns empty dataframe.

# 0.3.1
10th April 2020

* add rintrojs introduction (steps_introduction_df)

# 0.3.0
30th March 2020

* contact view added to 'appointment' view
* add telehealth and telephone item numbers

# 0.2.0
2nd February 2020

* access `dMeasure$check_subscription` (subscription restrictions)