# Overview

This repository contains the data engineering code for the Nonprofit Trends and 
Impacts survey dashboard. 

## Requirements

- `Y://` drive access
- Google Sheets access to the [`metricsMaster`](https://docs.google.com/spreadsheets/d/1HMoQymn4F6q0qOqTX9njTLujYwHFXXtSGnhgR0_Ohjk/edit?gid=0#gid=0) and [`dataTemplate`](https://docs.google.com/spreadsheets/d/1ZD2niTLxDAuIhaw_RR-Rg9iVlu1tEuX5OXFcYODv-8k/edit?gid=2005883929#gid=2005883929) sheet.

## Directory

- `R` contains the data engineering scrips in the order they should be run. 
Non-numbered scripts containing either helper functions or configuration parameters.
- `data` contains raw and processed data, and datasets used for validation.
    - To access the data stored on Google sheets, you will have to authenticate 
    your Google account for Google Sheets API access by running the following 
    lines:
    ```
    googlesheets4::gs4_auth(email = "your_google_email@gmail.com",
                            scopes = "https://www.googleapis.com/auth/spreadsheets")
    ```
- `data_requests` contains additional code used to run ad-hoc data pulls.