# Overview

This repository contains the data engineering code for the Nonprofit Trends and 
Impacts survey dashboard. 

## Requirements

- `Y://` drive access
- Google Sheets access to the [`metricsMaster`](https://docs.google.com/spreadsheets/d/1HMoQymn4F6q0qOqTX9njTLujYwHFXXtSGnhgR0_Ohjk/edit?gid=0#gid=0) and [`dataTemplate`](https://docs.google.com/spreadsheets/d/1ZD2niTLxDAuIhaw_RR-Rg9iVlu1tEuX5OXFcYODv-8k/edit?gid=2005883929#gid=2005883929) sheet.
- Alternative data template [URL](https://observablehq.com/@jeffmacinnes/urban-nonprofit-donor-trends). Has download links for `dataTemplate` and `metricsmaster`.

## Directory

- `R` contains the data engineering scrips in the order they should be run. 
Non-numbered scripts containing either helper functions or configuration parameters.
- `data` contains raw and processed data, and datasets used for validation.
  - `raw`: stores raw data downloaded from Y drive or Google Sheets, for validation datasets
  - `intermediate`: Intermediate datasets
  - `processed`: Final datasets
    ` _filtered`: dataset for dashboard
    `_formatted`: string formatting without removing metrics where n < 25
    `_full_postprocessed`: numeric variables for validation/inspection
  - `validate`
- `data_requests` contains additional code used to run ad-hoc data pulls.