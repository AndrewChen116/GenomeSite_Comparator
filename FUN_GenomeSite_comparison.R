Genomesite_comparison <- function(lt, f_nt, b_nt, applyParallel=F, core=4){
  # setting start time 
  start.time <- Sys.time()
  
  # load required package
  require(tidyverse)
  require(parallel)
  
  # dataset preprocess
  colnames(lt[["anchor"]]) <- c("ID", "CHR", "start", "end", "strand")
  colnames(lt[["target"]]) <- c("ID", "CHR", "start", "end", "strand")
  lt[["anchor"]]$start <- lt[["anchor"]]$start %>% as.numeric()
  lt[["anchor"]]$end <- lt[["anchor"]]$end %>% as.numeric()
  lt[["target"]]$start <- lt[["target"]]$start %>% as.numeric()
  lt[["target"]]$end <- lt[["target"]]$end %>% as.numeric()
  
  # generate list for parallel function 
  if(applyParallel){
    
    ## calculate the n_row for dataset spliting 
    n_row <- (lt[["target"]] %>% nrow() / core) %>% floor()
    p_lt <- list()
    
    ## split dataset
    for(i in 1:core){
      if(i < core){
        p_lt[[i]] <- list()
        a <- n_row*(i-1)+1
        b <- n_row*i
        p_lt[[i]][["anchor"]] <- lt[["anchor"]]
        p_lt[[i]][["target"]] <- lt[["target"]][a:b,]
      }else{
        p_lt[[i]] <- list()
        a <- n_row*(i-1)+1
        b <- nrow(lt[["target"]])
        p_lt[[i]][["anchor"]] <- lt[["anchor"]]
        p_lt[[i]][["target"]] <- lt[["target"]][a:b,]
      }
    }
  }
  
  # function for comparison analysis 
  applied.FUN <- function(lt){
    require(tidyverse)
    
    ## dataset input
    anchor.df <- lt[["anchor"]]
    target.df <- lt[["target"]]
    
    ## dataset setting 
    range.df <- anchor.df %>% 
      mutate(
        "forward" = anchor.df$start-f_nt,
        "backward" = anchor.df$end+b_nt
      )
    output.df <- cbind(
      target.df[1,1:5],
      range.df[1,1:5]
    )
    colnames(output.df)[1:5] <- colnames(output.df)[1:5] %>% paste0("T_",.)
    colnames(output.df)[6:10] <- colnames(output.df)[6:10] %>% paste0("A_",.)
    
    ## compare
    for (i in 1:nrow(range.df)) {
      overlap.df <- c(
        target.df$CHR == range.df$CHR[i] &
          target.df$strand == range.df$strand[i] &
          target.df$start <= range.df$backward[i] & 
          target.df$end >= range.df$forward[i] 
      ) %>% target.df[.,]
      
      if(nrow(overlap.df)>0){
        overlap.df <- overlap.df %>% cbind(range.df[i,1:5])
        colnames(overlap.df)[1:5] <- colnames(overlap.df)[1:5] %>% paste0("T_",.)
        colnames(overlap.df)[6:10] <- colnames(overlap.df)[6:10] %>% paste0("A_",.)
        
        output.df <- output.df %>% rbind(overlap.df)
      }

    }
    
    ## remove the first row which were applied for cold start
    output.df <- output.df[-1,]
    
    return(output.df)
  }
  applied.env <- new.env()
  applied.env[["f_nt"]] <- f_nt
  applied.env[["b_nt"]] <- b_nt

  # comparison analysis
  if(applyParallel){

    cl <- makeCluster(core)
    
    clusterExport(cl, "f_nt", envir = applied.env)
    clusterExport(cl, "b_nt", envir = applied.env)
    output.lt <- parLapply(
      cl, p_lt, applied.FUN
    )
    stopCluster(cl)
    
    for (i in 1:core) {
      if(i==1){
        output.df <- output.lt[[1]]
      }else{
        output.df <- rbind(
          output.df,
          output.lt[[i]]
        )
      }
    }
    output.df <- output.df %>% arrange(A_ID)

  }else{
    output.df <- applied.FUN(lt)
    output.df <- output.df %>% arrange(A_ID)
  }
  
  print(Sys.time()-start.time)
  return(output.df)
}