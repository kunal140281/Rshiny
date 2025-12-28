library(shiny)
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(sqldf)
library(r2rtf)
library(DT)

#========================================================
# READ DATA (EDIT THIS PATH ONLY)
#========================================================
path <- "C:\\GOT\\Rprogramming\\"   # <-- change if needed
adsl <- read_sas(paste0(path,"adsl.sas7bdat"))
adae <- read_sas(paste0(path,"adae.sas7bdat"))


#========================================================
# PRE-PROCESSING FUNCTION (SORT + INDENT PT)
#========================================================
create_final_table <- function(adsl, adae) {
  
  # keep safety population
  adsl <- adsl %>% filter(SAFFL == "Y")
  adsl_overall <- adsl %>% mutate(TRT01A = "Overall")
  master_adsl <- rbind(adsl, adsl_overall)
  
  adae1 <- adae %>%
    filter(SAFFL == "Y") %>%
    select(USUBJID, TRT01A, TRTEMFL, AETERM, AEDECOD, AEBODSYS)
  
  adae_overall <- adae1 %>% mutate(TRT01A = "Overall")
  master_adae <- rbind(adae1, adae_overall) %>% filter(AEBODSYS != "")
  
  # denominators per treatment
  denom_cnt <- master_adsl %>% group_by(TRT01A) %>% summarise(N = n(), .groups = "drop")
  
  # SOC-level counts
  soc_count <- sqldf("
        select TRT01A, AEBODSYS,
        count(distinct USUBJID) as sub_count,
        1 as soc_ord,
        '' as AEDECOD,
        count(USUBJID) as evt_cnt
        from master_adae
        where TRTEMFL = 'Y'
        group by TRT01A, AEBODSYS
        ")
  
  # PT-level counts
  pt_count <- sqldf("
        select TRT01A, AEBODSYS, AEDECOD,
        count(distinct USUBJID) as sub_count,
        2 as soc_ord,
        count(USUBJID) as evt_cnt
        from master_adae
        where TRTEMFL = 'Y'
        group by TRT01A, AEBODSYS, AEDECOD
        ")
  
  # combine and compute %
  final <- rbind(soc_count, pt_count) %>%
    left_join(denom_cnt, by = "TRT01A") %>%
    mutate(
      pct  = (sub_count / N) * 100,
      pct1 = round(pct, 2),
      total = paste0(sub_count, "(", pct1, ")", "[", evt_cnt, "]"),
      Stat = "n (%) E"
    ) %>%
    select(AEBODSYS, AEDECOD, Stat, total, soc_ord, TRT01A)
  
  final <- final %>% arrange(AEBODSYS, soc_ord, AEDECOD)
  
  # reshape
  last <- final %>%
    pivot_wider(
      id_cols = c("AEBODSYS", "soc_ord", "AEDECOD"),
      names_from = "TRT01A",
      values_from = "total"
    )
  
  #============ HERE – FIXED 8 SPACE INDENT ==============
  last1 <- last %>%
    mutate(
      SOCPT =
        ifelse(
          soc_ord == 1,
          AEBODSYS,                         # SOC no indent
          paste0(strrep("&nbsp;", 8), AEDECOD)   # PT with 8 spaces
        ),
      Stat = "n (%) E"
    )
  #========================================================
  
  df <- last1 %>%
    replace_na(list(
      Placebo = "0(0.00)[0]",
      Tquine  = "0(0.00)[0]",
      Overall = "0(0.00)[0]"
    )) %>%
    select(`System Organ Class / Preferred Term` = SOCPT,
           Stat, Tquine, Placebo, Overall)
  
  list(df = df, denom = denom_cnt)
}

#========================================================
# UI
#========================================================
ui <- fluidPage(
  titlePanel("TEAE Table 14.3.1.2 Interactive Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Treatment Counts (N)"),
      tableOutput("headerN"),
      
      hr(),
      selectInput("trtSel", "Select Treatment to Display:",
                  choices = c("Tquine", "Placebo", "Overall"),
                  selected = "Tquine"),
      
      hr(),
      downloadButton("downloadRTF", "Download RTF")
    ),
    
    mainPanel(
      h3("TEAE Summary"),
      DTOutput("tableOutput")
    )
  )
)

#========================================================
# SERVER
#========================================================
server <- function(input, output) {
  
  processed <- reactive({
    create_final_table(adsl, adae)
  })
  
  # show denominators
  output$headerN <- renderTable({
    processed()$denom
  }, rownames = FALSE)
  
  # DT table output (with indentation visible)
  output$tableOutput <- renderDT({
    df <- processed()$df
    col_name <- "System Organ Class / Preferred Term"
    
    df %>%
      select(!!col_name, Stat, input$trtSel) %>%
      datatable(
        rownames = FALSE,
        escape = FALSE,                # <-- IMPORTANT: keeps indentation
        options = list(pageLength = 20, autoWidth = TRUE),
        colnames = c(col_name, "Statistic", input$trtSel)
      )
  })
  
  # RTF download
  output$downloadRTF <- downloadHandler(
    filename = function() { "TEAE_Table_14.3.1.2.rtf" },
    content = function(file) {
      
      df <- processed()$df
      denom <- processed()$denom
      
      getN <- function(trt) {
        if (trt %in% denom$TRT01A) denom$N[denom$TRT01A == trt] else 0
      }
      drugN    <- getN("Tquine")
      placeboN <- getN("Placebo")
      totalN   <- getN("Overall")
      
      foot_notes <- paste0(
        "E: Number of events; N: Number of subjects; ",
        "n: Number of subjects with adverse event; %: n/N*100. ",
        "MedDRA Version 25.2"
      )
      
      df %>%
        rtf_page(orientation = "landscape") %>%
        rtf_title(
          title = "Section 14.3.1: Adverse Events",
          subtitle = "Table 14.3.1.2 TEAE by Treatment, SOC and PT (Safety Population)",
          text_justification = c("l", "l"),
          text_font = c(3, 3),
          text_format = c("b", "")
        ) %>%
        rtf_colheader(
          paste0(
            "System Organ Class / Preferred Term|Statistic|Tquine (N=", drugN,
            ")|Placebo (N=", placeboN, ")|Any Treatment (N=", totalN, ")"
          ),
          col_rel_width = c(30, 10, 10, 10, 10),
          text_justification = c("l", "c", "c", "c", "c")
        ) %>%
        rtf_body(
          col_rel_width = c(30, 10, 10, 10, 10),
          text_justification = c("l","c","c","c","c")
        ) %>%
        rtf_footnote(foot_notes, text_justification = "l") %>%
        rtf_encode() %>%
        write_rtf(file)
    }
  )
}

#========================================================
# RUN APP
#========================================================
shinyApp(ui, server)
