#=========================
# Libraries
#=========================
library(shiny)
library(shinydashboard)
library(dplyr)
library(haven)
library(r2rtf)
library(DT)
library(stringr)

path <- "C:\\GOT\\Rprogramming\\"
output_path <- "C:\\GOT\\Rprogramming\\OUTPUT\\"

#=========================
# Listing Functions
#=========================

listing_1623 <- function(adsl, output_path) {
  
  analysis_pop_ <- adsl %>%
    mutate(
      RANDFL1  = if_else(RANDFL == "Y", "Yes", "No"),
      SAFFL1   = if_else(SAFFL == "Y", "Yes", "No"),
      ITTFL1   = if_else(ITTFL == "Y", "Yes", "No"),
      PPROTFL1 = if_else(PPROTFL == "Y", "Yes", "No")
    ) %>%
    select(USUBJID, SAFFL1, RANDFL1, ITTFL1, PPROTFL1)
  
  file_path <- paste0(output_path, "Listing_16_2_3.rtf")
  
  analysis_pop_ %>%
    rtf_title("Listing 16.2.3 Assignment to Analysis Populations") %>%
    rtf_body() %>%
    rtf_encode() %>%
    write_rtf(file_path)
  
  list(data = analysis_pop_, file = file_path)
}


listing_16241 <- function(adsl, output_path) {
  
  demo_pop <- adsl %>%
    select(USUBJID, AGE, SEX, ETHNIC, RACE, BHGHTSI, BWGHTSI, BBMISI)
  
  file_path <- paste0(output_path, "Listing_16_2_4_1.rtf")
  
  demo_pop %>%
    rtf_page(orientation = "landscape") %>%
    rtf_title("Listing 16.2.4.1 Subject Demographics") %>%
    rtf_body() %>%
    rtf_encode() %>%
    write_rtf(file_path)
  
  list(data = demo_pop, file = file_path)
}


listing_16211 <- function(adsl, output_path) {
  
  withdrawal_list <- adsl %>%
    filter(EOSSTT == "Discontinued") %>%
    select(USUBJID, RFPENDTC, DCSREAS, TRTEDT)
  
  file_path <- paste0(output_path, "Listing_16_2_1_1.rtf")
  
  withdrawal_list %>%
    rtf_page(orientation = "landscape") %>%
    rtf_title("Listing 16.2.1.1 Withdrawals from the Study") %>%
    rtf_body() %>%
    rtf_encode() %>%
    write_rtf(file_path)
  
  list(data = withdrawal_list, file = file_path)
}


#=========================
# Dispatcher
#=========================
run_listing <- function(listing_id, adsl, output_path) {
  
  switch(
    listing_id,
    "L16231" = listing_1623(adsl, output_path),
    "L16241" = listing_16241(adsl, output_path),
    "L16211" = listing_16211(adsl, output_path),
    stop("Listing not implemented")
  )
}

#=========================
# UI
#=========================
ui <- dashboardPage(
  
  dashboardHeader(title = "Clinical Listings Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Listings", tabName = "listings", icon = icon("list")),
      selectInput(
        "listing",
        "Select Listing",
        choices = c(
          "L_16.2.1.1 – Withdrawals" = "L16211",
          "L_16.2.3.1 – Analysis Pop Flag" = "L16231",
          "L_16.2.4.1 – Demographics" = "L16241"
        )
      ),
      actionButton("run", "Generate Listing"),
      br(),
      downloadButton("download_rtf", "Download RTF")
    )
  ),
  
  dashboardBody(
    DTOutput("listing_table")
  )
)

#=========================
# SERVER
#=========================
server <- function(input, output, session) {
  
  listing_result <- eventReactive(input$run, {
    
    adsl <- read_sas(paste0(path, "adsl.sas7bdat"))
    
    run_listing(
      listing_id  = input$listing,
      adsl        = adsl,
      output_path = output_path
    )
  })
  
  # Show table
  output$listing_table <- renderDT({
    req(listing_result())
    listing_result()$data
  })
  
  # Download RTF
  output$download_rtf <- downloadHandler(
    filename = function() {
      basename(listing_result()$file)
    },
    content = function(file) {
      file.copy(listing_result()$file, file)
    }
  )
}

#=========================
# Run App
#=========================
shinyApp(ui, server)
