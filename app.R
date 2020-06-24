library(tidyverse)
library(lubridate)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(xtable)
library(shinyjs)
library(knitr)
library(kableExtra)

intubation <- readRDS("intubation.rda")

emergency <- readRDS("emergency.rda")

infusion <- readRDS("infusion.rda")

broncho <- readRDS("broncho.rda")

# ui ----------------------------------------------------------------------

ui <- fluidPage(
  
  theme = shinytheme("superhero"), 
  
  useShinyjs(),
  
  titlePanel("ScotSTAR Paediatric Retrieval Drug Dose Calculator"),
  
  br(),
  
  div(id = "title",
  
    strong("Disclaimer:"),
    
    p("This drug calculator is intended as an aid to management and must 
      AT ALL TIMES be used in conjuction with local drug preparation, 
      checking and administration policies and in conjuction with advice provided from the regional PICU centre."),
    
    p("Please only use this calculator if you agree to these terms."),
    
    p("This app is based on the ScotSTAR Excel calculator.  Please check the doses before administration and use clinical judgement, the author does not take responsibility for any unintended consequences."),
    
    HTML(paste(("For the original calculator, please refer to the"),
    
                a(href = "https://www.snprs.scot.nhs.uk/?page_id=152", "ScotSTAR website."))),
    
    br(),
    
    br(),
    
    HTML(paste("Copyright (C) Lawrence Li 2020.", 
                a(href = "https://github.com/lawrencelmli/ScotSTAR", "The code is available here.")
                )
         ),
    
    br(),
    
    br(),
    
    p("For best results, please use widescreen monitor."),
    
    hr()
  ),
  
  sidebarLayout(
    div(id="sidebar",
        
        sidebarPanel(
        
        strong("Today's Date:"),
      
        textOutput("today_date"),
        
        hr(),
        
        h5("Please input the following:"),
        
        dateInput("dob", label = "Date of Birth:",
                  max = today(), 
                  value = today(),
                  format = "dd/mm/yyyy"
                  ),
        
        strong("Age"),
        
        div(style="display: inline-block;vertical-align:top; ", textOutput("year.age")),
        
        div(style="display: inline-block;vertical-align:top; ", textOutput("month.age")),
        
        div(style="display: inline-block;vertical-align:top; ", textOutput("day.age")),
        
        br(),
        
        # helpText("The age will be displayed as 0 Years 0 Months if the child is less than a month old."),
        
        strong("Estimated Weight:"),
        
        div(style="display: inline-block;vertical-align:top; ", textOutput("est.weight")),
        
        div(style="display: inline-block;vertical-align:top; ", strong(" kg")),
        
        helpText("The Estimated Weight is calculated using the APLS formulae. It is not suitable for preterm babies. 
                  Please use clinical judgement as over- or under-estimation may occur."),
        
        uiOutput("actual.wt"), # now it changes depending on the estimated weight,
        
        # numericInput("actual.wt", "Enter Weight (kg) to Use for Calculation", value = 3.5),
        
        helpText("Note: weight less than 2 kg or greater than 50 kg may generate doses that are clinically inappropriate. 
                 Use clinical judgement"),
        
        textInput("allergies", "Please enter any allergies: ", value = "", placeholder = "Default is NKDA"),
        
        width = 3
        )
      ),
    
    mainPanel(
      
      tags$head(tags$script(src="pop.js")),
      
      fluidRow(
        
        column(
          
          wellPanel(
            
            strong("Allergies: "),
          
            div(style="display: inline-block;vertical-align:top; ", textOutput("txt.allergies")),
            
            br(),
            
            div(style="display: inline-block;vertical-align:top; ", strong("Age: ")),
            
            div(style="display: inline-block;vertical-align:top; ", textOutput("print.age")),
            
            br(),
            
            div(style="display: inline-block;vertical-align:top; ", strong("Weight: ")),
            
            div(style="display: inline-block;vertical-align:top; ", textOutput("print.wt")),
            
            div(style="display: inline-block;vertical-align:top; ", strong(" kg")),
            
            br()
            
            ),
          
          width = 3
          
          ),
        
        column(
          
          actionButton("showTitle", "Show/Hide Top Panel", width = "180px"),
          
          # actionButton("hideTitle", "Hide Top Panel", width = "130px"),
          
          div(style="display: inline-block;vertical-align:middle; ", helpText("Show/Hide Top Panel")),
          
          br(),
          
          br(),
          
          actionButton("showSidebar", "Show/Hide Sidebar", width = "180px"),
          
          # actionButton("hideSidebar", "Hide Sidebar", width = "130px"),
          
          div(style="display: inline-block;vertical-align:middle; ", helpText("Show/Hide Side Panel")),
          
          width = 4),
        
        column(width = 2),
        
        column(
          downloadButton(outputId = "printout", label = "Download as HTML"),
          
          helpText("To print this page as a prescription chart, download as HTML and print from browser"),
          
          width = 3)
        
      ),
      
      div(
        
        h4("Drugs with Standard Dilutions"),
        
        h5("Intubation Drugs"),
        
        htmlOutput("intubation.Kable"),
        
        h5("Emergency Drugs"),
        
        htmlOutput("emergency.Kable"),
        
        h5("Drugs for Infusion"),
        
        htmlOutput("infusion.Kable"),
        
        h5("IV Aminophylline & Salbutamol - Loading and Maintenance"),
        
        htmlOutput("broncho.Kable")
        
        
        
      )
      
      
      
    ) #/mainPanel
  
  ), #/sidebarLayout
  
) #/ui



# server ------------------------------------------------------------------

server <- function(input, output){
  
  observeEvent(input$showSidebar, {
    shinyjs::toggle(id = "sidebar")
  })
  
  # observeEvent(input$hideSidebar, {
  #   shinyjs::hide(id = "sidebar")
  # })
  
  observeEvent(input$showTitle, {
    shinyjs::toggle(id = "title")
  })
  
  # observeEvent(input$hideTitle, {
  #   shinyjs::hide(id = "title")
  # })
  
  
  output$today_date <- renderText({
    
    today.date <- today()
    
    today.date <- format(today.date, "%d/%m/%Y")
    
    return(today.date)
    
  })
  
  age.year <- reactive({
    
    ageinterval = as.period(input$dob %--% today())
    
      # from_lt = as.POSIXlt(input$dob)
      # to_lt = as.POSIXlt(today())
      # 
      # age = to_lt$year - from_lt$year
      # 
      # age <- ifelse(to_lt$mon < from_lt$mon |
      #          (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
      #        age - 1, age)
      # 
      # return(age)
     
    yr <- ageinterval$year
    
    return(yr)
      
      })
  
  age.month <- reactive({
    
    ageinterval = as.period(input$dob %--% today())

    # from_lt = as.POSIXlt(input$dob)
    # to_lt = as.POSIXlt(today())
    # 
    # 
    # if(to_lt$mon >= from_lt$mon && to_lt$mday >= from_lt$mday){
    #   
    #   mth <- to_lt$mon - from_lt$mon
    #   
    # }else if(to_lt$mon > from_lt$mon && to_lt$mday < from_lt$mday){
    #   
    #   mth <- to_lt$mon - from_lt$mon
    #   
    #   mth <- mth-1
    #   
    # }else if(to_lt$mon < from_lt$mon && to_lt$mday >= from_lt$mday){
    #   
    #   mth <- to_lt$mon - from_lt$mon
    #   
    #   mth <- mth+12
    #   
    # }else if(to_lt$mon <= from_lt$mon && to_lt$mday < from_lt$mday){
    #   
    #   mth <- to_lt$mon - from_lt$mon
    #   
    #   mth <- mth+11
    #   
    # }
    mth <- ageinterval$month
    
    return(mth)
    
  })
  
  age.day <- reactive({
    
    ageinterval = as.period(input$dob %--% today())
    
    age.Day <- ageinterval$day
    
    return(age.Day)
    
  })
  
  output$year.age <- renderText({
    
    paste0(age.year(), " Years")
    
  })
  
  output$month.age <- renderText({
    
    paste0(age.month(), " Months")
    
  })
  
  output$day.age <- renderText({
    
    paste0(age.day(), " Days")
    
  })
  
  output$age.printout <- renderUI({
    
    paste(age.year(), age.month(), age.day())
    
  })
  
 est.wt <- reactive({
    
   yr <- age.year()
   
   mth <- age.month()
   
   age <- round(yr+(mth/12), 1)
   
   if(age == 0){
     wt <- 3.5
   }else if(age > 0 && age < 1){
     wt <- (mth/2)+4
   }else if(age >= 1 && age <= 6){
     wt <- (age*2)+8
   }else if(age > 6){
     wt <- (age*3)+7
   }
  
  })
 
 output$est.weight <- renderText({
   
   est.wt()
   
 })
 
 output$actual.wt <- renderUI({
   
   numericInput("actual.wt", "Enter Weight (kg) to Use for Calculation (Default weight is the estimated weight but can be overwritten):", value = est.wt(), min = 0)
   
 })
 
 output$txt.allergies <- reactive({
   
   txt <- ifelse(input$allergies == "", "NKDA", input$allergies)
   
   return(txt)
   
 })
 
 print.allergies <- reactive({
   
   allergies <- ifelse(input$allergies == "", "NKDA", input$allergies)
   
   return(allergies)
   
 })
 
 
output$print.age <- renderText({
   
   txt.age <- paste(age.year(), "Years", age.month(), "Months", age.day(), "Days")
   
   return(txt.age)
   
 })
 
 output$print.wt <- renderText({
   
   txt.wt <- input$actual.wt
   
 })
 

# Colour Coding for Drugs -------------------------------------------------

 
 
 opioid <- "#5db1e4"
 
 relaxant <- "#f02e17"
 
 gaDrugs <- "#ffff00"
 
 uppers <- "#e5c1e5"
 
 antichol <- "#51d851"
 
 benzo <- "#FFA500"
 
 mod <- "#c0c0c0"
 

# Printing Tables ---------------------------------------------------------

 
 
 intubation.calc <- reactive({
   
   intubation$volume <- c(paste0(input$actual.wt/5, " ml"),
                          paste0(input$actual.wt/10, " ml"),
                          paste0(input$actual.wt/5, " ml"),
                          paste0(input$actual.wt/5, " ml"),
                          paste0(input$actual.wt*0.12, " ml")
                          )
   
   intubation$dose <- c(paste0(input$actual.wt*2, " mcg = 2 microgram/kg"),
                        paste0(input$actual.wt, " mg"),
                        paste0(input$actual.wt*2, " mg"),
                        paste0(input$actual.wt*2, " mg"),
                        paste0(input$actual.wt*3, " mg = 3 mg/kg")
                        )
   
   intubation$prescription <- c("", "", "", "", "")
   
   
   intubation$check <- c("", "", "", "", "")
   
   return(intubation)
   
 })
 
 output$intubation.Kable <- renderText({
   
   table.intubation <- intubation.calc()
   
   table.intubation <- table.intubation %>% 
     select("Drug" = drug,
            "Formulation" = formulation,
            "Recommended Dose" = rec_dose,
            "Dilution" = dilution,
            "Volume" = volume,
            "Dose" = dose
            # "Prescribed By" = prescription,
            # "Checked By" = check
     )
   
   kable(table.intubation, format = "html", escape = F) %>% 
     kable_styling(full_width = T, font_size = 14, bootstrap_options = c("condensed", "responsive")) %>%
     row_spec(1:nrow(table.intubation), color = "black") %>% 
     row_spec(1, background = opioid) %>%
     row_spec(2:3, background = relaxant) %>%
     row_spec(4:5, background = gaDrugs) %>%
     column_spec(2:4, background = "white") %>%
     column_spec(5, background = mod, width = "8em") %>%
     column_spec(6, background = mod, width = "15em") %>%
     column_spec(1, bold = T, width = "10em")
   
   })
 
 emergency.calc <- reactive({
   
   emergency$volume <- c(paste0(input$actual.wt/10, " ml"),
                         paste0(input$actual.wt/5, " ml"),
                         paste0(input$actual.wt/10, " ml"),
                         paste0(input$actual.wt/20, " ml")
                         )
   
   
   emergency$dose <- c(paste0(input$actual.wt*10, " micrograms"),
                       paste0(input$actual.wt*20, " micrograms"),
                       paste0(input$actual.wt*0.1, " mg"),
                       paste0(input$actual.wt*50, " micrograms")
                       )
   
   emergency$prescription <- c("", "", "", "")
   
   
   emergency$check <- c("", "", "", "")
   
   return(emergency)
   
 })
 
 output$emergency.Kable <- renderText({
   
   table.emergency <- emergency.calc()
   
   table.emergency <- table.emergency %>%
     select("Drug" = drug,
            "Formulation" = formulation,
            "Recommended Dose" = rec_dose,
            "Dilution" = dilution,
            "Volume" = volume,
            "Dose" = dose
            # "Prescribed By" = prescription,
            # "Checked By" = check
            )
   
   kable(table.emergency, format = "html", escape = F) %>% 
     kable_styling(full_width = T, font_size = 14, bootstrap_options = c("condensed", "responsive")) %>%
     row_spec(1:nrow(table.emergency), color = "black") %>%
     row_spec(1, background = uppers) %>%
     row_spec(2, background = antichol) %>%
     row_spec(3, background = benzo) %>%
     row_spec(4, background = "white") %>% 
     column_spec(2:4, background = "white") %>%
     column_spec(5, background = mod, width = "8em") %>%
     column_spec(6, background = mod, width = "15em") %>%
     column_spec(1, bold = T, width = "10em")
   
 })
 
 infusion.calc <- reactive({
   
   infusion$rate <- as.numeric(infusion$rate)
   
   if(input$actual.wt > 10){
     infusion <- infusion[-8, ]
     
     infusion$prescription <- c("", "", "", "", "", "", "")
     
     infusion$check <- c("", "", "", "", "", "", "")
     
   }else{
     infusion <- infusion
     
     infusion[8, 6] <- ifelse(input$actual.wt <= 10, input$actual.wt*75, 750)
     
     infusion$prescription <- c("", "", "", "", "", "", "", "")
     
     infusion$check <- c("", "", "", "", "", "", "", "")
     
   }
   
   infusion[5, 4] <- ifelse(input$actual.wt <= 50, "1", "Max 50 mg")
   infusion[5, 5] <- ifelse(input$actual.wt <= 50, "mg/kg in 50 ml 5% Glucose", "total in 50 ml 5% Glucose")
   
   infusion[1, 6] <- input$actual.wt*0.3
   
   infusion[2, 6] <- input$actual.wt*0.3
   
   infusion[3, 6] <- input$actual.wt*3
   
   infusion[4, 6] <- input$actual.wt*3
   
   infusion[5, 6] <- ifelse(input$actual.wt <= 50, input$actual.wt, 50)
   
   
   
   infusion[5, 8] <- ifelse(input$actual.wt <= 50, 1, input$actual.wt*0.02)
   
   infusion[6, 8] <- input$actual.wt*0.1
   
   infusion[7, 8] <- input$actual.wt*0.1
   
   infusion <- infusion %>%
     mutate(dilution = paste(as.character(dilution), dilutant),
            to_syringe = paste(as.character(to_syringe), dose_unit),
            rate = paste(as.character(rate), dose_equiv))
   
   
   
   
   
   
   
   return(infusion)
   
   })
 
 output$infusion.Kable <- renderText({
   
   table.infusion <- infusion.calc()
   
   table.infusion <- table.infusion %>% 
     select("Drug" = drug,
            "Formulation" = formulation,
            "Recommended Dose" = rec_dose,
            "Standard Dilution" = dilution,
            "Add to Syringe" = to_syringe,
            "Infusion Rate" = rate
            # "Prescribed By" = prescription,
            # "Checked By" = check 
            )
   
   kable(table.infusion, "html", escape = F) %>% 
     kable_styling(full_width = T, font_size = 14, bootstrap_options = c("condensed", "responsive")) %>% 
     row_spec(1:nrow(table.infusion), color = "black", background = "white") %>%
     row_spec(1:4, background = uppers) %>%
     row_spec(5, background = opioid) %>%
     row_spec(6, background = benzo) %>%
     row_spec(7, background = relaxant) %>%
     column_spec(2:4, background = "white") %>%
     column_spec(5, background = mod, width = "8em") %>%
     column_spec(6, background = mod, width = "15em") %>%
     column_spec(1, bold = T, width = "10em")
   
   })
 
 broncho.calc <- reactive({
   
   as.numeric(broncho$LD_dose)
   
   broncho[1, 6] <- ifelse(input$actual.wt <= 100, input$actual.wt*5, 500)
   broncho[1, 8] <- ifelse(input$actual.wt <= 100, input$actual.wt*5, 500)
   broncho[1, 9] <- broncho[1, 8]*3

   broncho[2, 6] <- ifelse(input$actual.wt <= 16.6, input$actual.wt*15, 250)
   broncho[2, 8] <- round(broncho[2, 6]/200, 2)
   broncho[2, 9] <- broncho[2, 6]/50
   
   broncho <- broncho %>% 
     mutate(LD_dose = paste(LD_dose, LD_unit),
            LD_vol = paste(LD_vol, "ml"),
            LD_rate = paste(LD_rate, "ml/hr"))
   
   broncho$prescription <- c("", "")
   
   
   broncho$check <- c("", "")
   
   return(broncho)
 })
 
 output$broncho.Kable <- renderText({
   
   table.broncho <- broncho.calc()
   
   st_dil_pop <- c("or 0.9% Sodium Chloride", "or 0.9% Sodium Chloride")
   
   dose_load_pop <- c("Omit loading if adequate level on oral theophylline. Max 500 mg", 
                      "Maximum loading dose 250 micrograms")
   
   table.broncho$st_dilution <- cell_spec(table.broncho$st_dilution,
     popover = spec_popover(
       content = st_dil_pop,
       title = NULL,
       position = "right",
       trigger = "hover"
     )
   )
   
   table.broncho <- table.broncho %>%
     select("Drug" = drug,
            "Formulation" = formulation,
            "Standard Dilution (Note)" = st_dilution,
            "Loading Dose (Note)" = dose_load,
            "Maintenance Dose" = dose_infusion,
            "Dose" = LD_dose,
            "Volume" = LD_vol,
            "Rate" = LD_rate,
            "Duration" = duration,
            "Infusion Rate" = maintenance_rate)
   
   kable(table.broncho, "html", escape = F) %>% 
     kable_styling("striped", full_width = T, font_size = 14, bootstrap_options = c("condensed", "responsive")) %>% 
     add_header_above(c(" " = 5, "Loading Dose" = 4, "Maintenance" = 1)) %>%
     row_spec(1:nrow(table.broncho), color = "black", background = "white") %>%
     column_spec(1, bold = T, width = "10em") %>%
     column_spec(3, color = "red", width = "14em") %>% 
     column_spec(4, color = "red", width = "12em") %>%
     column_spec(6:8, background = mod) %>%
     footnote(general = "Some comments on IV Aminophylline and IV Salbutamol",
              number = c("Both can be diluted in either 5% Glucose or 0.9% Sodium Chloride", 
                         "Omit aminophylline loading if adequate levels on oral theophylline.
                         Maxium loading dose is 500 mg",
                         "Maximum loading dose for IV salbutamol is 250 micrograms (1.25 mls)")
              )
   
   })
 
 output$printout <- downloadHandler(
   filename = function() {("ScotSTAR_printout.html") # change this to pdf if pagedown is working
     # paste('my-report', sep = '.', switch(
     #   input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
     # ))
   },
   
   content = function(file) {
     src <- normalizePath('printout.Rmd')
     src2 <- normalizePath("printout.css")
     label <- normalizePath("pxLabel.png")
     
     # temporarily switch to the temp dir, in case you do not have write
     # permission to the current working directory
     owd <- setwd(tempdir())
     on.exit(setwd(owd))
     file.copy(src, 'printout.Rmd', overwrite = TRUE)
     file.copy(src2, "printout.css", overwrite = TRUE)
     file.copy(label, "pxLabel.png", overwrite = TRUE)
     
     
     library(rmarkdown)
     out <- render('printout.Rmd', 
                   params = list(year = age.year(), 
                                 month = age.month(),
                                 day = age.day(),
                                 weight = input$actual.wt, 
                                 allergies = print.allergies() # note to self - can use reactive elements for params, but output$ won't work
                                 ),
                   'html_document')
     # library(pagedown)
     # out <- pagedown::chrome_print(out, "insulin-instructions.pdf", async = TRUE)
     file.rename(out, file)
   }
 )
 
}

shinyApp(ui = ui, server = server)