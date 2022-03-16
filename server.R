rm(list = ls())
source("FUN_GenomeSite_comparison.R")
load("default.RData")
options(shiny.maxRequestSize=30*1024^2) 

server <- function(input, output, session) {
 
  # check if apply example
  observeEvent(input$doExample,{
    assign("doExample",TRUE,globalenv())
    cat("Example setting checked\n")
  })
  
  # Anchor and Target table input 
  anchor_table <- eventReactive(c(input$doSearch, input$anchor_file, input$doExample),{
    ## record the start time
    t1 <- proc.time()
    cat("Import example:", doExample,"\n")
    tryCatch(
      {
        if(!is.null(input$anchor_file$datapath)& !doExample){
          anchor.df <- input$anchor_file$datapath %>% read.csv(sep = "\t") 
          cat("Imported anchor file checked\n")
        }else{
          cat("Example anchor file checked\n")
        }
        colnames(anchor.df) <- c("id","chr","start","end","strand")
      },
      warning = function(war){
        print(war)
      },
      error = function(err){
        print(conditionMessage(err));
        anchor.df <- NULL
      }
    )
    list(
      "anchor"=anchor.df,
      "t1"=t1
    )
  })
  target_table <- eventReactive(c(input$doSearch, input$target_file, input$doExample),{
    tryCatch(
      {
        if(!is.null(input$target_file$datapath) & !doExample){
          target.df <- input$target_file$datapath %>% read.csv(sep = "\t") 
          cat("Imported target file checked\n")
        }else{
          cat("Example target file checked\n")
        }
        colnames(target.df) <- c("id","chr","start","end","strand")
      },
      warning = function(war){
        print(war)
      },
      error = function(err){
        print(conditionMessage(err));
        target.df <- NULL
      }
    )
    target.df
  })
  
  # Comparison  
  output_Table <- eventReactive(c(input$doSearch, input$doExample),{
    anchor.lt <- anchor_table()
    anchor.df <- anchor.lt$anchor
    t1 <- anchor.lt$t1
    target.df <- target_table()
    
    output.df <- NULL
    tryCatch(
      {
        if(!is.null(anchor.df) & !is.null(target.df)){
          cat("Comparing ...\n")
        
          lt <- list(
            "anchor" = anchor.df,
            "target" = target.df
          )
          
          output.df <- Genomesite_comparison(
            lt, up_nt = input$up_nt, down_nt = input$down_nt,
            applyParallel = as.logical(input$doParallel),
            core = input$ncore
          ) %>% as_tibble()
          
          cat("Comparation finished \n")
        }else{
          output.df <- NULL
        }
      },
      warning = function(war){
        print(war)
      },
      error = function(err){
        print(conditionMessage(err));
        output.df <- NULL
        cat("err")
      }
    )
    
    ## record the execution time
    t2 <- proc.time()
    t <- t2-t1
    print_time <- paste0("Execution time: ",round(t[3][[1]],2), " s")
    
    ## reset doExample
    cat("Reset example setting\n")
    assign("doExample",FALSE,globalenv())
    
    ## return
    list(
      "output" = output.df,
      "time" = print_time
    )
    cat("******************** \n")
  })

  # export to ui
  
  ## comparation result
  output$output_table <- renderDataTable(
    output_Table()[[1]],
    options = list(
      pageLength = 25,
      searching = T,
      row.names = F
    ),
    escape = FALSE
  )
  
  ## anchor table
  output$anchor_table <- renderDataTable(
    anchor_table()[[1]],
    options = list(
      pageLength = 25,
      searching = T,
      row.names = F
    ),
    escape = FALSE
  )
  
  ## target table
  output$target_table <- renderDataTable(
    target_table(),
    options = list(
      pageLength = 25,
      searching = T,
      row.names = F
    ),
    escape = FALSE
  )
  
  ## show execution time
  output$print_time <- renderUI(
    output_Table()[[2]]
  )
  
  ## download output df
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_output", ".tsv")
    },
    content = function(file) {
      write.csv(output_Table()[[1]], file, sep = "\t",
                quote = FALSE, row.names = FALSE)
    }
  )
}
