# DemoEnsemblePlot UI

ui <- fluidPage(
  sidebarPanel(
    h3("Select Workflow"),
    selectizeInput(inputId = "wf.id", "Workflow ID", c())
  ),
  mainPanel(navbarPage(title = NULL,
                       tabPanel("Info", 
                                DT::dataTableOutput("ens_table")
                       ),
                       tabPanel("Ensemble Analysis", 
                                htmlOutput("ens_pdf_1"),
                                htmlOutput("ens_pdf_2")
                       ),
                       tabPanel("Sensitivity Analysis",
                                htmlOutput("sen_pdf_1")
                       ),
                       tabPanel("Variance Decomposition",
                                htmlOutput("vdc_pdf_1")
                       )
  )
  )
)