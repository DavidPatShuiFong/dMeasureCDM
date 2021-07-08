# News

# 0.6.0
8th July 2021

## New

* Aboriginal or Torres Strait Islander Health Assessment and age 45-49 year health assessments
  + `self$haatsi_list_cdm` - those who could qualify for health assessment
  + `self$ha4549_list_cdm` - those who could qualify for health assessment
  + suggestion of RN Cathy Xie, thanks
* Health Assessment for those with intellectual disability
  + `self$haiq_list_cdm` - those who could qualify for health assessment
* Refugee/Asylum seeker health assessment
  + `self$refugeeAsylum_list_cdm` - those who could qualify for health assessment
  
## Changes

* Aged 75+ Health Assessment renamed to `self$ha75_list_cdm`
  + display 'code' changed from 'HA' to 'HA75'
  
# 0.5.2
3rd July 2021

## New

* Filter `billings_cdm` by item status ('Never', 'Late', 'Up-to-date')
  + `item_status` list of status types defined as global in `CDM.R`
  + extra parameter `itemstatus_chosen` added to `billings_cdm`
  + suggestion of RN Dat Le, thanks
* `cdm_tem` and `cdm_item_names` defined as global in `CDM.R`

## Changes

* `dM$dateformat_choice` changes display format of dates

# 0.5.1
2nd December 2020

## Changes

* `steps_introduction_df` parameter `element_name` default to `as.character(NA)`
  + explicit `DailyMeasure::` for call to `steps_choose_contact_details`

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

* changes in management of lack of valid subscription/registration for
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
