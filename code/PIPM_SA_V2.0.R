# This is a SOCR PIPM Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#   http://shiny.rstudio.com/
#   https://socr.shinyapps.io/RShinyApp_PIPM/
#   https://shiny.med.umich.edu/apps/dinov/RShinyApp_PIPM/ 
#   http://myumi.ch/O49zG

library(shiny)
library(randomForest)
library(DT)

options(DT.options = list(pageLength = 22))

mean_data <<- read.csv("data_mean.csv", stringsAsFactors = F,header =T)
avg_val <<- c(mean_data[1,])
names(avg_val) <<- names(mean_data)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # utilityFunction(),
    
    # Application title
    titlePanel( 
        div(column(width = 2, tags$a(
                    href="https://nursing.umich.edu/",
                    tags$img(
                        src = "https://umich.edu/includes/panels/gallery/images/block-m-maize.png", 
                        align="left", height = 120)
                    )
            ),
            column(width = 8, tags$a(
                    href="https://SOCR.umich.edu/",
                    h1("Statistics Online Computational Resource (SOCR)", align="center")
                    )
            ),
            column(width = 2, tags$a(
                    href="https://SOCR.umich.edu/",
                    tags$img(
                        src = "http://wiki.socr.umich.edu/images/1/12/SOCR_Logo_April_2018.png", 
                        align="right", height = 150)
                    )
            ),
            br(), br(), br(), hr(), br()
        ),
        windowTitle="SOCR PIPM App"
    ), 
    
    div( column(width = 2, div(
            class = "header", checked = NA, align="center",
            actionButton("ReadMe", "ReadMe/Help"),
            hr(),
            tags$a(href="https://shiny.med.umich.edu/apps/dinov/RShinyApp_PIPM/", 
                        "http://myumi.ch/O49zG")
                )
            ),
        column(width = 10, 
           div(h2("Interactive Pressure Injury Prediction Model (PIPM) App", align="center")),
           div(h3("Zerihun Bekele, Christine Anderson, Dana Tschannen, and Ivo Dinov", align="center"))
        ),
        br(), br(),br(),br(),hr(), br()
    ),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h2("Upload input file"),
                     
            fileInput("file1", "Choose CSV File",
                       accept = c(
                       "text/csv",
                       "text/comma-separated-values,text/plain", ".csv")),
                     
            div(style="background-color:mistyrose; font-size:1.5em; text-align: center; height: 150%; padding: 12px;",
                         textOutput("placeholder"), hr()),
            h3("Or input below"),
                     numericInput("bpsinvasive_min_avg", "bpsinvasive_min_avg",avg_val$bpsinvasive_min_avg, min=0),
                     numericInput("min.rn.hppd", "min.rn.hppd",avg_val$min.rn.hppd, min=0),
                     numericInput("max.total.hppd", "max.total.hppd",avg_val$max.total.hppd, min=0),
                     numericInput("periop_tempmean_avg", "periop_tempmean_avg",avg_val$periop_tempmean_avg, min=0),
                     numericInput("bpdinvasive_max_avg", "bpdinvasive_max_avg",avg_val$bpdinvasive_max_avg, min=0),
                     numericInput("mean.rn.hppd", "mean.rn.hppd",avg_val$mean.rn.hppd, min=0),
                     numericInput("max.rn.hppd", "max.rn.hppd",avg_val$max.rn.hppd, min=0),
                     numericInput("periop_diasbpmean_avg", "periop_diasbpmean_avg",avg_val$periop_diasbpmean_avg, min=0),
                     numericInput("painmean_avg", "painmean_avg",avg_val$painmean_avg, min=0),
                     numericInput("min.total.hppd", "min.total.hppd",avg_val$min.total.hppd, min=0),
                     numericInput("painmax_avg", "painmax_avg",avg_val$painmax_avg, min=0),
                     numericInput("periop_sbpmax_avg", "periop_sbpmax_avg",avg_val$periop_sbpmax_avg, min=0),
                     numericInput("hr_min_avg", "hr_min_avg",avg_val$hr_min_avg, min=0),
                     numericInput("periop_diasbpmax_avg", "periop_diasbpmax_avg",avg_val$periop_diasbpmax_avg, min=0),
                     numericInput("periop_diasbpmax", "periop_diasbpmax",avg_val$periop_diasbpmax, min=0),
                     numericInput("periop_spo2mean_avg", "periop_spo2mean_avg", avg_val$periop_spo2mean_avg, min=0),
                     numericInput("periop_rrmax_avg", "periop_rrmax_avg", avg_val$periop_rrmax_avg, min=0),
                     numericInput("periop_rrmean_avg", "periop_rrmean_avg",avg_val$periop_rrmean_avg, min=0),
                     numericInput("periop_rrmax", "periop_rrmax",avg_val$periop_rrmax, min=0),
                     numericInput("periop_hrmax_avg", "periop_hrmax_avg",avg_val$periop_hrmax_avg, min=0),
                     
             hr(),
             actionButton("predict", "Predict"),
             actionButton("reset","Reset")
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            h2("Feature importance"),
            img(src = "feat_importance.png", height = 500, width = 500),
            #h3("Input data and prediction"),
            div(style="background-color:Gainsboro ; font-size:1.4em; height: 150%; padding: 8px;", checked=NA,
                textOutput("tableresult")),
            dataTableOutput('table')
            
        )
    ),
    
    shinyUI(bootstrapPage(
        div(
            # include The SOCR footer HTML
            includeHTML("SOCR_footer_tracker.html")
        )
    )),
    
    div("Main developer: Zerihun Bekele: ",
             tags$a(href="https://www.linkedin.com/in/zerihunalemayehu/", "Linkedin"), 
             " | ", 
             tags$a(href="https://twitter.com/zeru_alem", "Twitter"), br(), hr(), align='center'
             )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # utilityFunction()
    
    observeEvent(input$ReadMe, {
        showModal(modalDialog(
            title = "Help / ReadMe",
            HTML('<div>
                Hospitalization-related <b>pressure injuries (PIs)</b> present significant patient
             suffering, increased medical costs, and co-morbidities. <br /> <br />
             This interactive webapp provides clinicians, researchers and healthcare providers
             with an AI forecasting of patients developing PIs. This app may be useful to address
             preemptively hospitalization problems due to bed-immobility, sensory impairment, 
             bed positioning, and length of confinement.<br />
             <br />
             This app is developed by researchers at the School of Nursing and the Statistics Online Computational Resource.
             The prediction is based on training a random forest classifier using a large 
             electronic medical records database at the University of Michigan. <br /><br />
             
             Clinicians may either manually enter meta-data about the patients they want to predict the
             odds of developing PIs, or upload in bulk the data for a number of patients in CSV column format.
             The <b>top 20 most salient patient characteristics determining the risk of developing PIs</b> are
             show in the variable-importance plot and listed in the table below.<br /><br />
             
             The AI will forecast the likelihood of each patient to develop hospital-acquired PIs. 
             <br />
             '),
            
            HTML('<br/> <div align="center">
                 <a href="http://socr.umich.edu/HTML5/"><b>SOCR Apps</b></a> </div>'),
            easyClose = TRUE
        ))
    })
    
    inputData <- reactive({ 
        inFile <- input$file1
        read.csv(inFile$datapath, header = T, stringsAsFactors = FALSE)
        
    })
    
    output$table <- renderDataTable({
        inFile <- input$file1
        if (is.null(inFile))
            return(NULL)
        
        dt <- inputData()
        pred_result <- predict(model, dt, type='prob', predict.all=T)
        
        dt['PI probability (%)'] <- round(100*pred_result$aggregate[,2],2)
        dt['95% Prediction margin (+/-)'] <- round(196*apply(pred_result$individual,1,function(x) sd(as.numeric(x)))/sqrt(200),2)
        dt <- t(dt)
        
        rownames(dt) <- c(names(mean_data),'PI probability (%)','95% Prediction margin (+/-)')
        colnames(dt) <- paste("Case", 1:ncol(dt))
        
        output$tableresult <- renderText({"Table:Input data. Top two rows are model's output."})
        
        dt[c('PI probability (%)','95% Prediction margin (+/-)', names(mean_data)),]
        
    })
    
    model <- readRDS("rf_fit_bal_model_top20.rds")
    
    observeEvent(
        input$predict, {
            user_input <- c(input$bpsinvasive_min_avg, input$min.rn.hppd, input$max.total.hppd,
                            input$periop_tempmean_avg, input$bpdinvasive_max_avg, input$mean.rn.hppd,
                            input$max.rn.hppd, input$periop_diasbpmean_avg, input$painmean_avg,
                            input$min.total.hppd, input$painmax_avg, input$periop_sbpmax_avg, 
                            input$hr_min_avg, input$periop_diasbpmax_avg, input$periop_diasbpmax,
                            input$periop_spo2mean_avg, input$periop_rrmax_avg, input$periop_rrmean_avg,
                            input$periop_rrmax, input$periop_hrmax_avg)
            
            dt <- mean_data
            k=0
            for (i in user_input) {
                k=k+1
                dt[1,k] <- i
            }
            
            
            output$placeholder <- renderText({
                mi <- round(196*apply(predict(model, dt, type='prob',predict.all=T)$individual,1,function(x) sd(as.numeric(x)))/sqrt(200),2)
                paste("Predicted Probability of Pressure Injury: \r",
                      as.character(100*predict(model, dt, type='prob')[2]), "%", "+/-",mi)
                
            })
            
        })
    
    # observeEvent(
    #     input$reset,{
    #         updateNumericInput("periop_hrmax", value=avg_val$periop_hrmax)
    #         updateNumericInput("bpdinvasive_max", value=avg_val$bpdinvasive_max)
    #         updateNumericInput("bpdinvasive_max_avg", value=avg_val$bpdinvasive_max_avg )
    #         updateNumericInput("periop_hrmax_avg", value=avg_val$periop_hrmax_avg)
    #         updateNumericInput("periop_tempmean_avg", value=avg_val$periop_tempmean_avg)
    #         updateNumericInput("periop_rrmean_avg", value=avg_val$periop_rrmean_avg)
    #         updateNumericInput("periop_spo2mean_avg", value=avg_val$periop_spo2mean_avg)
    #         updateNumericInput("periop_rrmax_avg", value=avg_val$periop_rrmax_avg)
    #         updateNumericInput("bpsnoninvasive_max", value=avg_val$bpsnoninvasive_max)
    #         updateNumericInput("hr_min_avg", value=avg_val$hr_min_avg)
    # 
    #         updateNumericInput("painmax_avg", value=avg_val$painmax_avg)
    #         updateNumericInput("min.total.hppd", value=avg_val$min.total.hppd )
    #         updateNumericInput("n_miss_turn_avg", value=avg_val$n_miss_turn_avg)
    #         updateNumericInput("min.rn.hppd", value=avg_val$min.rn.hppd)
    #         updateNumericInput("periop_diasbpmax_avg", value=avg_val$periop_diasbpmax_avg)
    #         updateNumericInput("periop_diasbpmax", value=avg_val$periop_diasbpmax)
    #         updateNumericInput("max.total.hppd", value=avg_val$max.total.hppd)
    #         updateNumericInput("max.rn.hppd", value=avg_val$max.rn.hppd)
    #         updateNumericInput("mean.rn.hppd", value=avg_val$mean.rn.hppd)
    #         updateNumericInput("mean.total.hppd", value=avg_val$mean.total.hppd)
    #     })
}

# Run the application 
shinyApp(ui = ui, server = server)

# Deploy app
# library(rsconnect)
# rsconnect::deployApp('RShinyApp_PIPM/PIPM_SA_V2.0.R')
