# This is the back-end of the app

# This allows the large federal file to be uploaded
options(shiny.maxRequestSize=30*1024^2)

# These are calls to libraries or packages of code that help create the app
library(tidyverse) # Data cleaning library
library(vroom) # Reading in data
library(janitor) # Data cleaning
library(lubridate) # The best library for working with dates
library(gt) # Table library I like

# Building the backend
server <- function(input, output) {

  # This is a function that reads in and figures out what's in the federal file
  fed_data <- reactive({
    req(input$federal_list)

    ext <- tools::file_ext(input$federal_list$name)

    fed_df <- switch(
      ext,
      csv = vroom::vroom(input$federal_list$datapath, delim = ","),
      tsv = vroom::vroom(input$federal_list$datapath, delim = "\t"),
      validate("Invalid file; Please upload a .csv or .tsv file")
    )

    janitor::clean_names(fed_df) %>%
      mutate(row_num_fed=row_number()+1) %>%
      drop_na(lastname, firstname) %>%
      select(
        row_num_fed,
        lastname,
        firstname,
        dob,
        address,
        city,
        fed_state = state,
        zip,
        excldate,
        reindate,
        waiverdate,
        wvrstate
        ) %>%
      mutate(
        dob = format(lubridate::ymd(dob), '%Y-%m-%d'),
        excldate = format(lubridate::ymd(excldate), '%Y-%m-%d'),
        reindate = format(lubridate::ymd(reindate), '%Y-%m-%d'),
        waiverdate  = format(lubridate::ymd(waiverdate ), '%Y-%m-%d')
      )%>%
      mutate(across(where(is.character), tolower)) %>%
      mutate(on_fed_list=TRUE)


  })
  # This is a function that reads in and figures out what's in the state file
  state_data <- reactive({
    req(input$state_list)

    ext <- tools::file_ext(input$state_list$name)

    state_df <- switch(
      ext,
      csv = vroom::vroom(input$state_list$datapath, delim = ","),
      tsv = vroom::vroom(input$state_list$datapath, delim = "\t"),
      validate("Invalid file; Please upload a .csv or .tsv file")
    )

    janitor::clean_names(state_df) %>%
      mutate(row_num_state = row_number()+1) %>%
      drop_na(last_name, first_name) %>%
      separate(first_name, sep = " ", into = c("first_name", NA),extra = "drop") %>%
      select(
        row_num_state,
        last_name,
        first_name,
        effective_date,
        "state_end_date" = end_date,
        state_license_number
        ) %>%
      mutate(effective_date = format(lubridate::mdy(effective_date), '%Y-%m-%d')) %>%
      mutate(across(where(is.character), tolower)) %>%
      mutate(on_state_list=TRUE)



  })
  # This is a function that reads in and figures out what's in the staff file
  district_data <- reactive({
    req(input$district_list)

    ext <- tools::file_ext(input$district_list$name)

    district_df <- switch(
      ext,
      csv = vroom::vroom(input$district_list$datapath, delim = ","),
      tsv = vroom::vroom(input$district_list$datapath, delim = "\t"),
      validate("Invalid file; Please upload a .csv or .tsv file")
    )

    district_df %>%
      drop_na(last_name, first_name) %>%
      mutate(birthdate=format(lubridate::mdy(birthdate), '%Y-%m-%d')) %>%
      mutate(across(where(is.character), tolower))


    })

  matched <- reactive({
    # Require the files to loaded before doing anywork
    req(input$federal_list)
    req(input$state_list)
    req(input$district_list)

    # Fetch the data for this function
    fed_df <- fed_data()
    state_df <- state_data()
    district_df <- district_data()

    # Find the close matches Kind of similar to SQL syntax
    district_df %>%
      #Join state Data
    left_join(
      state_df,
      by = c("last_name" = "last_name", "first_name"="first_name"),

    ) %>%
      mutate(on_state_list = replace_na(on_state_list, FALSE)) %>%
      #Join fed data
      left_join(
        fed_df,
        by = c(
          "last_name" = "lastname",
          "first_name"="firstname",
          "birthdate" = "dob"
        ),

      ) %>%
      mutate(on_fed_list = replace_na(on_fed_list, FALSE)) %>%
      #Filter if on either list
      filter(on_state_list | on_fed_list)
  })



# Create a table of potential matches to display
  output$matched_display <- gt::render_gt({
    matched() %>%
      select(
        last_name,
        first_name,
        middle_name,
        birthdate,
        row_num_state,
        state_end_date,
        on_state_list,
        row_num_fed,
        fed_state,
        excldate,
        reindate,
        on_fed_list
      ) %>%
      # This is package specific syntax for GT
      gt() %>%
      # A spanner goes above the columns listed
      gt::tab_spanner(
        "Staff Info",
        columns = c(last_name, first_name, middle_name, birthdate)
      )  %>%
      gt::tab_spanner(
        "State List",
        columns = c(row_num_state, on_state_list, state_end_date)
      ) %>%
      gt::tab_spanner(
        "Federal List",
        columns = c(row_num_fed, on_fed_list, fed_state, excldate, reindate)
      ) %>%
      # Renaming the columns for display
      gt::cols_label(
        last_name = "Last Name",
        first_name = "First Name",
        middle_name = "Middle Name",
        birthdate = "DOB",
        row_num_state = "Row Number"

      )
  })

}
