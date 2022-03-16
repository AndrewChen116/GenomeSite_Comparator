source("FUN_GenomeSite_comparison.R")
options(shiny.maxRequestSize=30*1024^2) 

server <- function(input, output, session) {
  
  # Start time
  # start_time <- eventReactive(c(input$DoSearch, input$DoExample),{
  #   cat("start_time\n")
  #   t1 <- proc.time()
  # })
  
  # Anchor and Target table input 
  Anchor_Table <- eventReactive(c(input$DoSearch, input$anchor_file, input$DoExample),{
    cat("Anchor_Table\n")
    t1 <- proc.time()
    tryCatch(
      {
        if(!is.null(input$anchor_file$datapath)){
            anchor.df <- input$anchor_file$datapath %>% read.csv(sep = "\t") 
        }
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
  Target_Table <- eventReactive(c(input$DoSearch, input$target_file, input$DoExample),{
    cat("Target_Table\n")
    tryCatch(
      {
        if(!is.null(input$target_file$datapath)){
          target.df <- input$target_file$datapath %>% read.csv(sep = "\t") 
        }
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
  output_Table <- eventReactive(c(input$DoSearch, input$DoExample),{
    cat("output_Table\n")
    #start_time()
    anchor.lt <- Anchor_Table()
    anchor.df <- anchor.lt$anchor
    t1 <- anchor.lt$t1
    target.df <- Target_Table()
    
    output.df <- NULL
    tryCatch(
      {
        if(T){
          
          lt <- list(
            "anchor" = anchor.df,
            "target" = target.df
          )
          
          output.df <- Genomesite_comparison(
            lt, f_nt = input$f_nt, b_nt = input$b_nt,
            applyParallel = as.logical(input$DoParallel),
            core = input$ncore
          ) %>% as_tibble()
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
    
    t2 <- proc.time()
    t <- t2-t1
    print_time <- paste0("Execution time: ",round(t[3][[1]],2), " s")
    
    list(
      "output" = output.df,
      "time" = print_time
    )
    
  })
  
  # End time 
  # print_time <- eventReactive(c(input$DoSearch, input$DoExample),{
  #   cat("print_time\n")
  #   t1 <- start_time()
  #   t2 <- proc.time()
  #   t <- t2-t1
  #   print_time <- paste0("Execution time: ",round(t[3][[1]],2), " s")
  #   
  #   print_time
  # })
  
  ## Check
  # info <- NULL
  # print_A <- reactive({
  #   if(is.null(input$xmlfile$datapath)){
  #     info <- NULL
  #   }else if(c(!grepl(".xml$",input$xmlfile$datapath)) %>% as.numeric() %>% sum()){
  #     info <- '<font color="red">Wrong format!</font>'
  #   }else if(is.null(xmlTable())){
  #     info <- '<font color="red">Please check again!</font>'
  #   }else{
  #     info <- "Successfully input data"
  #   }
  #   HTML(info)
  # })
  # 
  # 
  # print_T <- reactive({
  #   if(is.null(input$jsonfile$datapath)){
  #     info <- NULL
  #   }else if(c(!grepl(".json$",input$jsonfile$datapath)) %>% as.numeric() %>% sum()){
  #     info <- '<font color="red">Wrong format</font>'
  #   }else if(is.null(jsonTable())){
  #     info <- '<font color="red">Please check again!</font>'
  #   }else{
  #     info <- "Successfully input data"
  #   }
  #   HTML(info)
  # })
  # 
 
  ## Output setting 
  # output$print_A <- renderUI(
  #   print_A()
  # )
  # output$print_T <- renderUI(
  #   print_T()
  # )

  
  output$output_table <- renderDataTable(
    output_Table()[[1]],
    options = list(
      pageLength = 25,
      searching = T,
      row.names = F
    ),
    escape = FALSE
  )
  
  output$anchor_table <- renderDataTable(
    Anchor_Table()[[1]],
    options = list(
      pageLength = 25,
      searching = T,
      row.names = F
    ),
    escape = FALSE
  )
  
  output$target_table <- renderDataTable(
    Target_Table(),
    options = list(
      pageLength = 25,
      searching = T,
      row.names = F
    ),
    escape = FALSE
  )
  
  output$print_time <- renderUI(
    output_Table()[[2]]
  )
  
}