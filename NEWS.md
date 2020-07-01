# News

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
