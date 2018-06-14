# DemoEnsemblePlot server

library(PEcAn.all)
library(PEcAn.visualization)

options(shiny.maxRequestSize=30*1024^2) #maximum file input size

dir.create("www")

server <- function(input, output, session) {
  
  bety <- betyConnect()
  rv <- reactiveValues()
  
  observe({
    wfs <- tbl(bety, "workflows") %>% collect 
    query <- parseQueryString(session$clientData$url_search)
    updateSelectizeInput(session, "wf.id", 
                         choices = sort(unique(wfs$id), decreasing = TRUE), 
                         selected = query[["workflow_id"]])
  })
  
  observeEvent({input$wf.id}, ignoreInit = TRUE, {
    if(is.na(as.numeric(input$wf.id))){
      ens <- ""
    }else{
      rv$wf.id <- input$wf.id
      ens <- tbl(bety, "ensembles") %>% filter(workflow_id == rv$wf.id) %>% collect
      output$ens_table <- DT::renderDataTable(DT::datatable(as.matrix(ens)))
    }
  })
  
  observeEvent({rv$wf.id}, ignoreInit = TRUE, {
    ens <- tbl(bety, "ensembles") %>% filter(workflow_id == rv$wf.id) %>% collect
    if("ensemble" %in% ens$runtype){
      wf <- workflow(bety, rv$wf.id) %>% collect
      ens_pdf_1 <- dir( wf %>% pull(folder), 
                              pattern = "ensemble.analysis.*pdf", full.names = TRUE)
      file.copy(ens_pdf_1, "www/ens_pdf_1.pdf", overwrite = TRUE)
      if(file.exists("www/ens_pdf_1.pdf") & file.size("www/ens_pdf_1.pdf") != 0){
        output$ens_pdf_1 <- renderUI({
          tags$iframe(style="height:600px; width:100%; scrolling=yes", 
                      src="ens_pdf_1.pdf")
        })
      }
      
      ens_pdf_2 <- dir(workflow(bety, rv$wf.id) %>% pull(folder), 
                       pattern = "ensemble.ts.*pdf", full.names = TRUE)
      file.copy(ens_pdf_2, "www/ens_pdf_2.pdf", overwrite = TRUE)
      if(file.exists("www/ens_pdf_2.pdf") & file.size("www/ens_pdf_2.pdf") != 0){
        output$ens_pdf_2 <- renderUI({
          tags$iframe(style="height:600px; width:100%; scrolling=yes", 
                      src="ens_pdf_2.pdf")
        })
      }
    }
  })
  
  observeEvent({rv$wf.id}, ignoreInit = TRUE, {
    ens <- tbl(bety, "ensembles") %>% filter(workflow_id == rv$wf.id) %>% collect
    if("sensitivity analysis" %in% ens$runtype){
      
      # just for example, pick the first pft 
      pft_path <- dir(file.path(workflow(bety, rv$wf.id) %>% pull(folder), "pft"), full.names = TRUE)[1]
      
      sen_pdf_1 <- dir(pft_path, pattern = "sensitivity.*pdf", full.names = TRUE)
      file.copy(sen_pdf_1, "www/sen_pdf_1.pdf", overwrite = TRUE)
      if(file.exists("www/sen_pdf_1.pdf") & file.size("www/sen_pdf_1.pdf") != 0){
        output$sen_pdf_1 <- renderUI({
          tags$iframe(style="height:600px; width:100%; scrolling=yes", 
                      src="sen_pdf_1.pdf")
        })
      }
      
      vdc_pdf_1 <- dir(pft_path, pattern = "variance.*pdf", full.names = TRUE)
      file.copy(vdc_pdf_1, "www/vdc_pdf_1.pdf", overwrite = TRUE)
      if(file.exists("www/vdc_pdf_1.pdf") & file.size("www/vdc_pdf_1.pdf") != 0){
        output$vdc_pdf_1 <- renderUI({
          tags$iframe(style="height:600px; width:100%; scrolling=yes", 
                      src="vdc_pdf_1.pdf")
        })
      }
    }
  })
  
  
}