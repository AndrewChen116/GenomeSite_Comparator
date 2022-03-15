
# input example datasets
  anchor.df <- read.csv(
    "datasets/ex_anchor.tsv",
    sep = "\t"
  )[1:1000,-6]
  target.df <- read.csv(
    "datasets/ex_target.tsv",
    sep = "\t"
  )[1:10000,]

# pre-process
  anchor.df$ID <- paste0("A-",anchor.df$CHR,"-",sprintf("%09d",anchor.df$end))
  target.df$ID <- paste0("T-",target.df$CHR,"-",sprintf("%05d",target.df$ID))
  
# generate list for function 
  lt <- list(
    "anchor" = anchor.df,
    "target" = target.df
  )
# test function
  output_1.df <- Genomesite_comparison(
    lt, 20, 10, applyParallel = F, core = 4
  )
  output_2.df <- Genomesite_comparison(
    lt, 20, 10, applyParallel = T, core = 4
  )
# write example table 
  write.table(
    anchor.df,
    "datasets/ex_anchor_2.tsv",
    sep = "\t",
    quote = F,
    row.names = F
  )
  write.table(
    target.df,
    "datasets/ex_target_2.tsv",
    sep = "\t",
    quote = F,
    row.names = F
  )
  