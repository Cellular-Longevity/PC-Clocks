clocksDir <- "/Users/jessgraves/code/PC-Clocks-main/" #where the clocks directory was downloaded to
                                                                         #must end with '/'
dataDir <- paste0(clocksDir, "transposed_data")
# load(file = paste(clocksDir,"CalcAllPCClocks.RData", sep = ""))

source(paste(clocksDir, "run_calcPCClocks.R", sep = ""))

# IMPORTANT FORMATTING NOTE: If you are not using the example methylation and Pheno data, you will need to have specific
#     formatting. Please ensure that your Methylation dataframe/ matrix is of the methylation beta values and row names
#     are sample names, and column names are CpGs.
#     For the pheno data, ensure that the data frame/ matrix has rows as samples, and columns as whatever phenotype
#     variables you have/ wish. This can also include the original CpG clocks if you used the online Horvath calculator
#     as well. HOWEVER, the pheno matrix MUST have a column named "Age", and a column named "Female" (capital required),
#     especially if you want to calculate GrimAge and its components. Female = 1 is F and Female = 0 is M.
#
#     If you don't have this information, you can also just set it so Females is all 1 (all samples labeled female) and
#     all the same Age. Just know that this won't be accurate for PCGrimAge or components, and that you can't run
#     the acceleration calculations with calcPCClock_Accel.
#
#     The code below is going to ask if you would like to check the order of your datPheno and datMeth samples to ensure
#     they line up. For this to work, you will need to type the column name of datPheno with the names of the samples or 
#     'skip'.


###### our data
# install.packages("arrow")
library(arrow) # for reading in parquet
library(tidyverse)

# phenotype data
p0 <- read_parquet("/Users/jessgraves/Downloads/datPheno.parquet", as_tibble = TRUE)
p <- p0 %>% 
  # Dropping the date cols because na_if doesn't like them and I don't think we need them to work
  dplyr::select(-contains("_at")) %>% 
  na_if("NA") %>%
  mutate(Age = age, 
         Female = ifelse(sex == "F", 1, 0))

# get file lists of methylated data
files <- list.files(dataDir, full.names = T, pattern = ".parquet")
files_only <- list.files(dataDir, full.names = F, pattern = ".parquet")
# get study groups
study_group <- unlist(lapply(files, function(f) unlist(str_split(unlist(str_split(f, ".parquet"))[[1]], "/"))[[7]]))

# # starting with example 1
start <- Sys.time()
d1.0 <- read_parquet(files[1], as_tibble = T) 
  cpgs <- d1.0$`__index_level_0__`
d1 <- d1.0 %>% dplyr::select(!contains("index")) %>% t() %>% data.frame()
colnames(d1) <- cpgs

# p %>% filter(project_id == study_group[1]) %>% dplyr::select(id) %>% pull()

# test run
res1 <- calcPCClocks(path_to_PCClocks_directory = clocksDir,
                    datMeth = d1, 
                    datPheno = p %>% filter(project_id == study_group[1]))


# Custom function to loop through
calc_clock_jg <- function(x){
  message("Reading in df")
   start <- Sys.time()
    df.0 <- read_parquet(files[x], as_tibble = T) 
     cpgs <- df.0$`__index_level_0__`
    df <- df.0 %>% dplyr::select(!contains("index")) %>% t() %>% data.frame()
   colnames(df) <- cpgs
    end <- Sys.time()
    
    p.df <- p %>% filter(project_id == study_group[x])
    # print duration because it's slow
  message(paste0(round(as.numeric((end-start)), 2), " elapsed"))
  
  message("Running calcPCClocks")
  res <- calcPCClocks(path_to_PCClocks_directory = clocksDir,
                      datMeth = df, 
                      datPheno =  p.df)
  
  message("Saving output")
  # save_location <- paste0("/Users/jessgraves/code/PC-Clocks-main/output/results_", files_only[x])
  save_location <- paste0("/Users/jessgraves/code/PC-Clocks-main/output_new/results_", files_only[x])
  write_parquet(res, save_location)
}

# calc_clock_jg(1)

# looping through files and runs...
# This is not a smart way to do this...
# but my computer keeps crashing, so where we are. 

# set runs
runs <- c(1:10)
# update vector as you move forward
# runs <- c(11:(11+9))
# 21 DID NOT WORK 
# runs <- c(48:(48+9))
runs

for(i in seq_along(runs)){
  start <- Sys.time()
  calc_clock_jg(runs[i])
  print(round(as.numeric(Sys.time() - start), 2))
  print(paste0("finished ", i))
}
