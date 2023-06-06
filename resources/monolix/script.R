# AUTHOR: Combefr1 (updated by ochseda2)
#   DATE: March2023 (updated May 10, 2023)
#   PROJECT/STUDY: BICEPS # nolint
#   DESCRIPTION: automatic creation and
#     Run of Monolix demo project for BICEPS study
#   change settings and create files
#   collection of results
#    INPUT  Monolix Demos
#    /home/combefr1/lixoft/monolix/monolix2021R2/demos/8.case_studies
#    hiv_project.mlxtran
#    hcv_project.mlxtran
#    arthritis_projet.mlxtran
#    PKVK_project.mlxtran
#    tgi_project.mlxtran
#    OUTPUT ../BICEPS/..
#    Runs for monolix models


main <- function(test_type = "small", expert = TRUE, plotFlag = FALSE) {
  cat("Start: ", date(), "\n")
  # library loads
  sessionInfo()
  library(lixoftConnectors)
  library(tibble)
  library(dplyr)
  library(ggplot2)
  initializeLixoftConnectors(
    software = "monolix",
    path = "/CHBS/apps/EB/software/Monolix/2021R2",
    force = TRUE
  )
  
  mlxtran.inputs <-  tribble(
    ~model,                       ~longrunning,
    "hiv_project.mlxtran",        F,
    "hcv_project.mlxtran",        F,
    "arthritis_project.mlxtran",  F,
    "PKVK_project.mlxtran",       F,
    "tgi_project.mlxtran",         F,
    "PK_ANC_LU_JCP_2021.mlxtran", T
    
  )
  
  input.dir <- "resources/monolix"
  output.dir <- "outputs/monolix" # activity folder where you want to create all files
  
  # and run the analysis
  output_name <- "BICEPS_results"
  output_pdf <- paste0(output_name, ".pdf")
  output_pdf_performance <- paste0(output_name, "_performance.pdf")
  output_pdf_estimation <- paste0(output_name, "_estimation.pdf")
  output_csv <- paste0(output_name, ".csv")
  
  # setup output files locations and check out
  
  outfiles <- list(
    outfile_pdf = file.path(output.dir, output_pdf),
    outfile_pdf_performance = file.path(output.dir, output_pdf_performance),
    outfile_pdf_estimation = file.path(output.dir, output_pdf_estimation),
    outfile_csv = file.path(output.dir, output_csv)
  )
  
  
  source(file.path(input.dir, "helper_funs.R"))
  
  # actual computation
  # first create a template folder in the activity folder
  
  dir.create(file.path(output.dir, "templates"), recursive=TRUE)
  # first create a runs folder in the activity folder
  dir.create(file.path(output.dir, "runs"))
  
  # create the central project mlxtran files.
  # for projects not in monolix library, user can  directly save
  # mlxtran projects in the template folder and execute the next function to list
  # all the files and execute
  for(iter in 1:nrow(mlxtran.inputs)){
    create_projects(mlxtran.inputs[iter,],input.dir = input.dir,
                    output.dir = output.dir, expert = expert)
  }
  
  
  paramCombo <- tribble(
    ~Nodes, ~Cores, ~Walltime, ~Memory, ~Wrapper,
    1,      12,     100,      500,    "mlxbsub2021", # 1 default options
    1,      1,      100,     1000,   "mlxbsub2021", # 2 minimal CPU resource
    1,      6,      100,     500,    "mlxbsub2021", # 3 reasonable CPU resource
    1,      12,     100,      500,    "mlxbsub2021", # 4 high CPU resource
    1,      16,     100,      500,    "mlxbsub2021", # 5 Max CPU resource (no MPI)
    4,      12,     100,      500,    "mlxbsub2021", # 6 Max CPU resource (MPI)
    4,      3,      100,      500,    "mlxbsub2021", # reasonable CPU resource (MPI) matching job 4
    2,      6,      100,      500,    "mlxbsub2021", # reasonable CPU resource (MPI) matching job 4   
    1,      12,     100,      500,    "mlxbsub2023r1", # 1 default options
    1,      1,      100,     1000,   "mlxbsub2023r1", # 2 minimal CPU resource
    1,      6,      100,     500,    "mlxbsub2023r1", # 3 reasonable CPU resource
    1,      12,     100,      500,    "mlxbsub2023r1", # 4 high CPU resource
    1,      16,     100,      500,    "mlxbsub2023r1", # 5 Max CPU resource (no MPI)
    4,      12,     100,      500,    "mlxbsub2023r1", # 6 Max CPU resource (MPI)
    4,      3,      100,      500,    "mlxbsub2023r1", # reasonable CPU resource (MPI) matching job 4
    2,      6,      100,      500,    "mlxbsub2023r1", # reasonable CPU resource (MPI) matching job 4
    
  )
  
  if (test_type == "small") {
    paramCombo <- paramCombo[3, ]#faster as it goes to the short run
  }
  
  # list all the files created in the "template" folder
  MLXfilenames <- list.files(file.path(output.dir, "templates"),
                             pattern = ".mlxtran")
  
  jobIDs <- c()
  for (Model in MLXfilenames) {
    jobIDs <- c(jobIDs,
                .run_project(MLXfilename = Model,
                  output.dir = output.dir,
                  paramCombo = paramCombo
                )
    )
  }
  
  # run a first with default options, then different ones
  jobIDs_done <- paste0("ended(", jobIDs, ")")
  Sys.sleep(30) #waiting a few seconds to let the scheduler time to start the execution
  
  # and wait until all jobs are completed
  # this wait part is optional and the following function can be executed while jobs are stil running
  # results will be inclomplete
  command <- paste0("bwait -t 4320 -w \" ",
                    paste(jobIDs_done, collapse = "&&"),
                    " \" ")
  print(command)
  system(command
         ,
         intern = TRUE
  ) # wait for 20 hours
  
  
  # gather results
  # if your R session crashed, you can get the jobID of all the monolix jobs in the activity folder
  # by using:
  allLSFlogs <- list.files(file.path(output.dir, "runs"),
                           recursive = TRUE,
                           pattern = ".lsf")
  
  jobIDs <- as.numeric(gsub(".*[.](\\d+)[.]lsf", "\\1", allLSFlogs))
  
  # get all the logs and primary  metrics
  all_results <- lapply(jobIDs, FUN = get_logs, output.dir=output.dir)
  
  # Extract all the results and bind them in one dataset
  Final_results <- lapply(X = all_results,
                          FUN = extract_one_results) %>%
    bind_rows()
  
  # preparing for plotting 
  # we rename the labels for plotting
  Final_results <- Final_results %>% 
    mutate(Metric = case_when(Metric == "CPU_TIME_seconds" ~ "CPU time (s)",
                              Metric == "CPU_Efficiency" ~ "CPU Efficiency (%)",
                              Metric == "Pending_time_seconds" ~ "Pending time (s)",
                              Metric == "Running_time_seconds" ~ "Running time (s)",
                              Metric == "Params" ~ "Max diff Est/init (%)",
                              Metric == "RSE" ~ "Max of RSEs (%)",
                              Metric == "AVG_MEM" ~ "Average Mem (MB)", 
                              Metric == "EFFICIENCY_MEM" ~ "Mem Efficiency (%)",
                              Metric == "CPU_PEAK" ~  "CPU Peak", 
                              Metric == "MAX_MEM" ~ "Max Mem (MB)",
                              .default = Metric)) %>% 
    mutate(V = as.character(V))%>%
    mutate(Version = case_when(V == "2021" ~ "Monolix Suite 2021", 
                               V == "2023" ~ "Monolix Suite 2023", 
                               .default = V))%>%
    mutate(Value = case_when(Value == "FALSE" ~ "0",
                             Value == "TRUE" ~ "1",
                             .default = Value))
  
  #transformation of time for plotting, from s to h or d if large time
  temp1 <- Final_results %>%
    filter(Metric %in%c("Running time (s)","Pending time (s)", "CPU time (s)"))%>%
    mutate(Value = as.numeric(Value), 
           mlxfolder2 = gsub(mlxfolder, pattern = " expert", replacement = ""))%>%
    group_by(mlxfolder2, Metric)%>%
    mutate(MEANVALUE = mean(Value))%>%
    ungroup()%>%
    mutate(Value =  case_when(MEANVALUE < 200 ~ Value, 
                              MEANVALUE > 86400 ~ Value/60/60/24, 
                              (MEANVALUE <= 86400&&MEANVALUE >= 200)~ Value/60/60,
                              .default = Value),
    Metric = case_when(MEANVALUE < 200 ~ Metric, 
                        MEANVALUE > 86400 ~ gsub(Metric, pattern = "\\(s\\)", replacement = "(d)"), 
                        (MEANVALUE <= 86400&&MEANVALUE >= 200)~ gsub(Metric, pattern = "\\(s\\)", replacement = "(h)"),
                        .default = Metric)
    )%>%mutate(Value = as.character(Value))
    
    
  Final_results2 <- Final_results %>%filter(!(Metric %in%c("Running time (s)","Pending time (s)", "CPU time (s)")))%>%
    bind_rows(temp1)%>%
    mutate(mlxfolder = case_when(mlxfolder == "Long PK ANC LU JCP 2021"~ "PK-ANC; Lu et al", 
                                 mlxfolder == "Long PK ANC LU JCP 2021 expert"~ "PK-ANC; Lu et al expert", 
                                 .default = mlxfolder))
  # create labels for cluster settings
  Labels <- Final_results2 %>%
    group_by(N, n) %>%
    slice(1) %>%
    mutate(
      Settings = paste0("N = ", N, "; n = ", n),
      totalcores = N * n
    ) %>%
    ungroup() %>%
    arrange(totalcores) %>%
    select(Settings) %>%
    pull()
  # add a few features for plotting and remove the "Weekend" value (not used as of now)
  Final_results2 <- Final_results2 %>%
    mutate(
      RunSettings =
        factor(paste0("N = ", N, "; n = ", n),
               levels = Labels
        ),
      Value2 = as.numeric(Value)
    )
  # we now want the mean value over the N replicates for each model/scenario
  Summary_Final_results <- Final_results2 %>%
    group_by(RunSettings, mlxfolder, Status, Metric, Version) %>%
    summarise(Value = mean(as.numeric(Value2)))%>%mutate(CPUR = RunSettings)
  
  # get a proper project name, getting rid of all the cluster settings for plotting
  names <- unique(gsub(Final_results2$mlxfolder, pattern = " expert", replacement = ""))
  names <- gsub(names, pattern = "_", replacement = " ")
  
  # plot results
  # to get previous results?
  # temp <- read.csv(outfiles$outfile_csv)
  
  # Summary_Final_results <- bind_rows(temp, Summary_Final_results)
  # plotting of all the results extracted
  #
  Summary_Final_results_short <- Summary_Final_results%>%filter(Metric %in%c(
    "CPU Efficiency (%)","CPU time (s)" ,"CPU time (h)" ,"CPU time (d)" , 
    "Mem Efficiency (%)", "Max Mem (MB)",
    "Pending time (s)","Pending time (h)","Pending time (d)",
    "Running time (s)"   , "Running time (h)"   , "Running time (d)"   ,
    "Max diff Est/init (%)", "Max of RSEs (%)"                  
  ))%>%  
  mutate(Status= case_when(Status == "run time limit"~"Run time limit", 
                             .default = Status))%>% 
    mutate(Metric = factor(Metric, 
                           levels = c(
                             "CPU Efficiency (%)",
                             "CPU time (s)" , "CPU time (h)" ,"CPU time (d)" ,
                             "Mem Efficiency (%)", "Max Mem (MB)",
                             "Pending time (s)","Pending time (h)","Pending time (d)",
                             "Running time (s)"   ,"Running time (h)"   ,"Running time (d)"   ,
                             "Max diff Est/init (%)", "Max of RSEs (%)"                  
                           )), 
           Status = factor(Status, levels =c( "Success", "Failed",  "Running", "Run time limit"))
           )
  
  # the first plot is quite difficult to read, so we use only the main metrics
  if (plotFlag) {
    pdf(outfiles$outfile_pdf, width = 7, height = 12)
    for (iter in names) {
      temp <- Summary_Final_results_short[
        grep(Summary_Final_results_short$mlxfolder, pattern = iter),
      ]
      p1 <- ggplot(data = temp, aes(y = Value, x = CPUR)) +
        geom_line(aes(group = Version,color = Version)) +
        geom_point(aes(group = Status,shape = Status)) +
        facet_grid(Metric ~ mlxfolder, scales = "free") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust = 1))
      print(p1)
    }
    dev.off() # close the pdf file / plots
    
    pdf(outfiles$outfile_pdf_performance, width = 7, height = 10)
    for (iter in names) {
      temp <- Summary_Final_results_short[
        grep(Summary_Final_results_short$mlxfolder, pattern = iter),
      ]
      temp2 <- temp%>%filter(Metric%in% c(
        "CPU Efficiency (%)","CPU time (s)" , 
        "CPU time (h)" ,"CPU time (d)" ,
        "Mem Efficiency (%)", "Max Mem (MB)",
        "Pending time (s)","Pending time (h)","Pending time (d)",
        "Running time (s)", "Running time (h)"   ,"Running time (d)"    
      ))
      p2 <- ggplot(data = temp2, aes(y = Value, x = CPUR)) +
        geom_line(aes(group = Version,color = Version)) +
        geom_point() +
        facet_grid(Metric ~ mlxfolder, scales = "free") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust = 1))
      print(p2)
    }
    dev.off() # close the pdf file / plots
    
    pdf(outfiles$outfile_pdf_estimation, width = 7, height = 4)
    for (iter in names) {
      temp <- Summary_Final_results_short[
        grep(Summary_Final_results_short$mlxfolder, pattern = iter),
      ]
      temp3 <- temp%>%filter(Metric%in% c(
        "Max diff Est/init (%)", "Max of RSEs (%)"                  
      ))
      p3 <- ggplot(data = temp3, aes(y = Value, x = CPUR)) +
        geom_line(aes(group = Version,color = Version)) +
        geom_point() +
        facet_grid(Metric ~ mlxfolder, scales = "free") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust = 1))
      print(p3)
    }
    dev.off() # close the pdf file / plots
    
  }
  
  
  # and we write the table of results.
  # this file will be the one to share back to the organizer of the benchmark
  # along with Rout file
  write.csv(Summary_Final_results, outfiles$outfile_csv)
  
  
  # Conclusion: System parameters
  cat("\n Print environment:\n")
  print(R.version)
  warnings()
  cat("end: ", date(), "\n")
  sessionInfo()
  commandArgs(trailingOnly = FALSE)
}


