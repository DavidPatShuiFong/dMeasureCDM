% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CDM_GPstatUI.R
\name{datatableServer}
\alias{datatableServer}
\title{Chronic disease management list module - server}
\usage{
datatableServer(input, output, session, dMCDM)
}
\arguments{
\item{input}{as required by Shiny modules}

\item{output}{as required by Shiny modules}

\item{session}{as required by Shiny modules}

\item{dMCDM}{dMeasureCDM R6 object
access to appointments lists, condition lists, history and EMR database}
}
\value{
none
}
\description{
chronic disease management items claimed, pending or unclaimed
for appointment list
}