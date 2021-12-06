#' This is the User Interface file for the shinyapp

# These are calls to libraries or packages of code that help create the app
library(shiny) # This loads the shiny library which does the backend
library(shinydashboard) # My preferred front end library
library(gt) # This is a table library that I like

#####   Create elements for page   #####
####   Create Header   ####
page_header <- dashboardHeader(title = "Medicaid Exclusions")
####   Create Sidebar   ####
page_sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      text = "Upload Data",
      icon = icon("file-upload"),
      tabName = "upload_data"

    ),
    menuItem(
      text = "Results",
      icon = icon("th-list"),
      tabName = "see_results"
    )
  )
)
####   Create body   ####
page_body <- dashboardBody(
  # I made this with multiple tabs. I probably didn't have to
  tabItems(
    tabItem(
      # Page to upload data to app
      tabName = "upload_data",
      column(
        width = 12,

        box(width = 12,
            title = "Upload Files",
            a("Check out the template for file specificaion and instructions.",
              href="https://docs.google.com/spreadsheets/d/16rwHoDvhvCjViAPgrLS9xLlysYHj2nvNVfEFrfrsR-4/edit?usp=sharing"
            ),
            fileInput(
              inputId = "federal_list",
              label = "Federal List",
              accept = c(".csv", ".tsv")
              ),
            fileInput(
              inputId = "state_list",
              label = "State List",
              accept = c(".csv", ".tsv")
              ),
            fileInput(
              inputId = "district_list",
              label = "District List",
              accept = c(".csv", ".tsv")
              )


            )
      )
    ),
    # page to see the results
    tabItem(
      tabName = "see_results",
      column(
        width = 12,
        box(width = 12,
            title = "Potential Matches",
            gt::gt_output("matched_display")


        )
      )
    )
  )
)

#### Actually assemble the UI ####
ui <- dashboardPage(
  title = "Medicaid Exclusion Checker",
  header = page_header,
  sidebar = page_sidebar,
  body = page_body

)
