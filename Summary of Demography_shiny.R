library(shiny)
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(r2rtf)
library(DT)

#========================================================
# READ DATA (EDIT THIS PATH ONLY)
#========================================================

#adsl <- read_sas("C:\\Users\\Owner\\Downloads\\adsl.sas7bdat")

path <- "C:\\GOT\\Rprogramming\\"
adsl <- read_sas(paste0(path,"adsl.sas7bdat"))
OUTPUT <- "C:\\GOT\\Rprogramming\\OUTPUT"
#========================================================
# PRE-PROCESSING FUNCTION
#========================================================
create_demographics_table <- function(adsl) {
  
  # Safety population
  safety_pop <- adsl %>% 
    filter(SAFFL == 'Y') %>% 
    select(USUBJID, TRT01A, AGE, SEX, RACE, ETHNIC, BHGHTSI, BBMISI, BWGHTSI)
  
  overall <- safety_pop %>% mutate(TRT01A = 'Overall')
  master <- rbind(safety_pop, overall)
  
  # Denominator count
  denom_cnt <- master %>% 
    group_by(TRT01A) %>% 
    summarize(N = n(), .groups = "drop")
  
  # Age statistics
  age_des <- master %>% 
    group_by(TRT01A) %>% 
    summarise(
      n = as.character(n()),
      mean = as.character(round(mean(AGE), 1)),
      sd = as.character(round(sd(AGE), 2)),
      median = as.character(round(median(AGE), 1)),
      min = as.character(round(min(AGE), 0)),
      max = as.character(round(max(AGE), 0)),
      .groups = "drop"
    ) %>% 
    mutate(ord = 1, CATEG = "", param = "Age (years)")
  
  # Gender statistics
  gendr <- master %>% 
    group_by(TRT01A, SEX) %>% 
    summarise(a_n = n(), .groups = "drop") %>% 
    mutate(
      param = "Gender",
      CATEG = case_when(
        SEX == "M" ~ "Male",
        SEX == "F" ~ "Female",
        TRUE ~ SEX
      ),
      ord = case_when(
        SEX == "M" ~ 2.1,
        SEX == "F" ~ 2.2
      )
    )
  
  Gen <- merge(gendr, denom_cnt, by = 'TRT01A') %>% 
    mutate(
      pct = (a_n/N) * 100,
      pct1 = round(pct, 2),
      total = paste0(a_n, "(", pct1, ")"),
      Stat = "n(%)"
    ) %>% 
    arrange(ord) %>% 
    select(TRT01A, param, CATEG, ord, total, Stat)
  
  # Race statistics
  Race <- master %>% 
    group_by(TRT01A, RACE) %>% 
    summarise(a_n = n(), .groups = "drop") %>% 
    mutate(
      param = "Race",
      CATEG = case_when(
        RACE == "WHITE" ~ "White",
        RACE == "BLACK OR AFRICAN AMERICAN" ~ "Black or African American",
        TRUE ~ RACE
      ),
      ord = case_when(
        RACE == "WHITE" ~ 3.1,
        RACE == "BLACK OR AFRICAN AMERICAN" ~ 3.3
      )
    )
  
  race1 <- merge(Race, denom_cnt, by = 'TRT01A') %>% 
    mutate(
      pct = (a_n/N) * 100,
      pct1 = round(pct, 2),
      total = paste0(a_n, "(", pct1, ")"),
      Stat = "n(%)"
    ) %>% 
    arrange(ord) %>% 
    select(TRT01A, param, CATEG, ord, total, Stat)
  
  # Dummy race categories
  dummy <- tribble(
    ~TRT01A, ~RACE, ~param, ~CATEG, ~ord, ~total, ~Stat,
    "Overall", "Asian", "Race", "Asian", 3.2, '0(0.0)', "n(%)",
    "Placebo", "Asian", "Race", "Asian", 3.2, '0(0.0)', "n(%)",
    "Tquine", "Asian", "Race", "Asian", 3.2, '0(0.0)', "n(%)",
    "Overall", "American Indian or Alaska Native", "Race", "American Indian or Alaska Native", 3.4, '0(0.0)', "n(%)",
    "Placebo", "American Indian or Alaska Native", "Race", "American Indian or Alaska Native", 3.4, '0(0.0)', "n(%)",
    "Tquine", "American Indian or Alaska Native", "Race", "American Indian or Alaska Native", 3.4, '0(0.0)', "n(%)",
    "Overall", "Native Hawaiian or Other Pacific ", "Race", "Native Hawaiian or Other Pacific ", 3.5, '0(0.0)', "n(%)",
    "Placebo", "Native Hawaiian or Other Pacific ", "Race", "Native Hawaiian or Other Pacific ", 3.5, '0(0.0)', "n(%)",
    "Tquine", "Native Hawaiian or Other Pacific ", "Race", "Native Hawaiian or Other Pacific ", 3.5, '0(0.0)', "n(%)",
    "Overall", "Islander ", "Race", "Islander ", 3.6, '0(0.0)', "n(%)",
    "Placebo", "Islander ", "Race", "Islander ", 3.6, '0(0.0)', "n(%)",
    "Tquine", "Islander ", "Race", "Islander ", 3.6, '0(0.0)', "n(%)"
  ) %>% 
    select(-RACE)
  
  finrace <- rbind(race1, dummy) %>% arrange(ord)
  Genrace <- rbind(Gen, finrace)
  
  overall <- Genrace %>% 
    filter(TRT01A == "Overall") %>% 
    select(-TRT01A) %>% 
    rename(Overall = total) %>% 
    select(-Stat)
  
  Placebo <- Genrace %>% 
    filter(TRT01A == "Placebo") %>% 
    select(-TRT01A) %>% 
    rename(Placebo = total) %>% 
    select(-Stat)
  
  drug <- Genrace %>% 
    filter(TRT01A == "Tquine") %>% 
    select(-TRT01A) %>% 
    rename(Drug = total) %>% 
    select(-Stat)
  
  char <- overall %>% 
    left_join(Placebo, by = c("ord", "param", "CATEG")) %>% 
    left_join(drug, by = c("ord", "param", "CATEG")) %>% 
    mutate(Stat = "n(%)")
  
  # Height statistics
  hgt_des <- master %>% 
    group_by(TRT01A) %>% 
    summarise(
      n = as.character(n()),
      mean = as.character(round(mean(BHGHTSI), 1)),
      sd = as.character(round(sd(BHGHTSI), 2)),
      median = as.character(round(median(BHGHTSI), 1)),
      min = as.character(round(min(BHGHTSI), 0)),
      max = as.character(round(max(BHGHTSI), 0)),
      .groups = "drop"
    ) %>% 
    mutate(ord = 4, CATEG = "", param = "Height (cm)")
  
  # Weight statistics
  Wgt_des <- master %>% 
    group_by(TRT01A) %>% 
    summarise(
      n = as.character(n()),
      mean = as.character(round(mean(BWGHTSI), 1)),
      sd = as.character(round(sd(BWGHTSI), 2)),
      median = as.character(round(median(BWGHTSI), 1)),
      min = as.character(round(min(BWGHTSI), 0)),
      max = as.character(round(max(BWGHTSI), 0)),
      .groups = "drop"
    ) %>% 
    mutate(ord = 5, CATEG = "", param = "Weight (kg)")
  
  # BMI statistics
  BMI_des <- master %>% 
    group_by(TRT01A) %>% 
    summarise(
      n = as.character(n()),
      mean = as.character(round(mean(BBMISI), 1)),
      sd = as.character(round(sd(BBMISI), 2)),
      median = as.character(round(median(BBMISI), 1)),
      min = as.character(round(min(BBMISI), 0)),
      max = as.character(round(max(BBMISI), 0)),
      .groups = "drop"
    ) %>% 
    mutate(ord = 6, CATEG = "", param = "BMI (kg/m2)")
  
  # Combine numeric summaries
  num_des <- bind_rows(age_des, hgt_des, Wgt_des, BMI_des) %>% 
    pivot_longer(
      cols = c("n", "mean", "sd", "median", "min", "max"),
      names_to = "Stat",
      values_to = "Overall"
    ) %>% 
    filter(TRT01A == "Overall") %>% 
    select(-TRT01A, -CATEG)
  
  num_des1 <- bind_rows(age_des, hgt_des, Wgt_des, BMI_des) %>% 
    pivot_longer(
      cols = c("n", "mean", "sd", "median", "min", "max"),
      names_to = "Stat",
      values_to = "Placebo"
    ) %>% 
    filter(TRT01A == "Placebo") %>% 
    select(-TRT01A, -CATEG)
  
  num_des2 <- bind_rows(age_des, hgt_des, Wgt_des, BMI_des) %>% 
    pivot_longer(
      cols = c("n", "mean", "sd", "median", "min", "max"),
      names_to = "Stat",
      values_to = "Drug"
    ) %>% 
    filter(TRT01A == "Tquine") %>% 
    select(-TRT01A, -CATEG)
  
  num <- num_des %>% 
    left_join(num_des1, by = c("ord", "param", "Stat")) %>% 
    left_join(num_des2, by = c("ord", "param", "Stat")) %>% 
    mutate(CATEG = "")
  
  # Final combined table
  last <- rbind(num, char) %>% 
    arrange(ord) %>% 
    select("param", "CATEG", "Stat", "Drug", "Placebo", "Overall")
  
  list(df = last, denom = denom_cnt)
}

#========================================================
# UI
#========================================================
ui <- fluidPage(
  titlePanel("Demographics Table 14.1.3 Interactive Dashboard"), ##Main Title
  
  sidebarLayout(
    sidebarPanel(
      h4("Treatment Counts (N)"),
      tableOutput("headerN"), ## the counts table
      
      hr(),
      selectInput("trtSel", "Select Treatment to Display:",
                  choices = c("Drug (Tquine)", "Placebo", "Overall"),
                  selected = "Overall"), ## Default Value
      
      hr(),
      checkboxGroupInput("paramSel", "Select Parameters to Display:",
                         choices = c("Age (years)", "Gender", "Race", 
                                     "Height (cm)", "Weight (kg)", "BMI (kg/m2)"),
                         selected = c("Age (years)", "Gender", "Race", 
                                      "Height (cm)", "Weight (kg)", "BMI (kg/m2)")),
      
      hr(),
      downloadButton("downloadRTF", "Download RTF")
    ),
    
    mainPanel(
      h3("Demographics Summary"),## Main Table
      DTOutput("tableOutput") ##actual adsl table
    )
  )
)

#========================================================
# SERVER
#========================================================
server <- function(input, output) {
  
  processed <- reactive({
    create_demographics_table(adsl) ## call on the function to create the table
  })
  
  # Show denominators
  output$headerN <- renderTable({
    processed()$denom
  }, rownames = FALSE)
  
  # DT table output
  output$tableOutput <- renderDT({
    df <- processed()$df
    
    # Filter by selected parameters
    df_filtered <- df %>% filter(param %in% input$paramSel)
    
    # Select treatment column
    trt_col <- case_when(
      input$trtSel == "Drug (Tquine)" ~ "Drug",
      input$trtSel == "Placebo" ~ "Placebo",
      input$trtSel == "Overall" ~ "Overall"
    )
    
    ##### filtering mechanism
    df_filtered %>%
      select(Characteristic = param, Category = CATEG, Statistics = Stat, !!input$trtSel := !!trt_col) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 25, autoWidth = TRUE, dom = 't'),
        colnames = c("Characteristic", "Category", "Statistics", input$trtSel)
      )
  })
  
  # RTF download
  output$downloadRTF <- downloadHandler(
    filename = function() { "Table_14.1.3_Demographics.rtf" },
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
        "BMI: body mass index; SD: standard deviation.\n",
        "N: The number of subjects in the safety population; ",
        "n: The number of subjects in the specific category; ",
        "%: calculated using the number of subjects in the safety population ",
        "as the denominator (n/N*100)."
      )
      
      df %>%
        rtf_page(orientation = "landscape") %>%
        rtf_title(
          title = "Section 14.1: Disposition and Demographic Data",
          subtitle = "Table 14.1.3 Subject Demographics (Safety Population)",
          text_justification = c("l", "l"),
          text_font = c(3, 3),
          text_format = c("b", "")
        ) %>%
        rtf_colheader(
          paste0(
            "Characteristic|Category|Statistics|Test (N=", drugN,
            ")|Reference (N=", placeboN, ")|Any Treatment (N=", totalN, ")"
          ),
          col_rel_width = c(10, 20, 10, 10, 10, 10),
          text_justification = c("l", "l", "c", "c", "c", "c")
        ) %>%
        rtf_body(
          col_rel_width = c(10, 20, 10, 10, 10, 10),
          text_justification = c("l", "l", "c", "c", "c", "c"),
          group_by = "param"
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

