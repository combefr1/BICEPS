#* this function will create N*3 replicates of the initial model, N
#* for each set of algorithm settings (default and expert or default only)
#* alternatively, user can save their project in the template folder
#' Title
#' @param mlxtran.name the project file name (eg project.mlxtran)
#' @param input.dir location of monolix resources
#' @param output.dir location of run projects
#' @param expert shall we create files with expert settings
#' @return NULL
create_projects <- function(mlxtran.input,
                            input.dir,
                            output.dir,
                            expert = FALSE) {
  # load monolix templates
  filepath <- file.path(input.dir, "templates", mlxtran.input$model)
  loadProject(projectFile = filepath)
  if(mlxtran.input$longrunning){
    mlxtran.input$model = paste0("Long_",mlxtran.input$model)
  }
  # save the project in the template
  outputpath <- file.path(output.dir, "templates")
  saveProject(projectFile = file.path(outputpath, mlxtran.input$model))
  filename <- tools::file_path_sans_ext(mlxtran.input$model)
  
  # we setup more iterations without auto stop criteria (all iterations will be done)
  if (expert&(!mlxtran.input$longrunning)) {
    setPopulationParameterEstimationSettings(
      nbexploratoryiterations = 2000,
      nbsmoothingiterations = 500,
      exploratoryAutoStop = FALSE
    )
    setConditionalModeEstimationSettings(nboptimizationiterationsmode = 400)
    setStandardErrorEstimationSettings(
      miniterations = 50,
      maxiterations = 400
    )
    setLogLikelihoodEstimationSettings(nbfixediterations = 10000)
    saveProject(projectFile = file.path(
      outputpath,
      paste0(
        filename,
        "_expert.mlxtran"
      )
    ))
  }
  if (expert&mlxtran.input$longrunning) {
    setPopulationParameterEstimationSettings(
      nbexploratoryiterations = 500,
      nbsmoothingiterations = 500,
      exploratoryAutoStop = FALSE
    )
    setConditionalModeEstimationSettings(nboptimizationiterationsmode = 200)
    setStandardErrorEstimationSettings(
      miniterations = 50,
      maxiterations = 200
    )
    setLogLikelihoodEstimationSettings(nbfixediterations = 1000)
    saveProject(projectFile = file.path(
      outputpath,
      paste0(
        filename,
        "_expert.mlxtran"
      )
    ))
  }
  message(
    paste(unlist(strsplit(filename, split = "_")), collapse = " "),
    " created, ",
    if (expert) "including with expert settings"
  )
}


# this function will start the monolix run for each model
# the XX_option is aiming at adjusting for the user's specific option command
# by defaut, starting a monolix execution uses the following command
# mxbsub2021 -N 1 -n 12 -W 300 -M 1000 -p project.mxltran
#'
#' @param output.dir
#' @param replicates
#' @param wrapper wrapper to start a monolix execution
#' @param project_option project name option
#' @param nodes_option
#' @param nodes Total number of nodes to use (default: 1)
#' @param cores_option
#' @param cores Number of cores/threads to use per nodes (default: 4)
#' @param MIN_option 
#' @param MIN Maximum job wallclock run time in minutes (default: 300)
#' @param MEM_option
#' @param MEM Maximum job memory (MB per core - default: 1000)
#'
#' @return
#' @export
#'
#' @examples

# function to execute a monolix project with N replicates
.run_project <- function(MLXfilename,
                      output.dir,
                      replicates=3, 
                      paramCombo) {
  # first, we save the project under a new name in the runs folder
  root_MLXfilename <- tools::file_path_sans_ext(MLXfilename)
  new_MLXfilepath <- file.path(output.dir, "runs", root_MLXfilename)
  dir.create(new_MLXfilepath)
  filepath <- file.path(output.dir, "templates", MLXfilename)
  loadProject(projectFile = filepath)
  if(grepl(pattern = "Long_", filepath)){
    paramCombo$Walltime = paramCombo$Walltime*100
  }
  
  jobs_ID <- c()
  
  for(i in 1:nrow(paramCombo)){
    nodes = paramCombo[[i, "Nodes"]]
    cores = paramCombo[[i, "Cores"]]
    MIN = paramCombo[[i, "Walltime"]]
    MEM = paramCombo[[i, "Memory"]]
    wrapper = paramCombo[[i, "Wrapper"]]
    Version = gsub(
      gsub(wrapper, pattern = "mlxbsub", replacement = ""), 
      pattern = "r1", replacement = "")
    # for each replicate
    
    for (iter in 1:replicates) {
      # store project
      new_filename <- paste0(
        root_MLXfilename, "_",
        "V",Version, "_",
        "N", nodes, "_",
        "n", cores, "_",
        "W", MIN, "_",
        "M", MEM, "_",
        "replicate", iter,
        ".mlxtran"
      )
      saveProject(file.path(new_MLXfilepath, new_filename))
      wrapperPrefix <- if (wrapper == "mlxbsub2021") {
        "; module purge; "
      } else {
        "; module purge; source /CHBS/apps/busdev_apps/init.sh;"
      }
      # then we start the execution
      job_id <- system(
        paste(
          "cd ", new_MLXfilepath, 
          wrapperPrefix,wrapper,
          "-N", nodes,
          "-n", cores,
          "-W", MIN,
          "-M", MEM,
          "-p", new_filename
        ),
        intern = TRUE
      )
      # extract job_id
      job_id <- job_id[6]
      job_id <- trimws(sub("bjobs", "", job_id))
      jobs_ID <- c(jobs_ID, job_id)
    }
  }
  return(jobs_ID)
}

.parseSettingsString <- function(settings, flag) {
  pattern <- paste0(".*_", flag, "([0-9]+)(_.*|$)")
  as.numeric(gsub(pattern, "\\1", settings))
}

.extractFromLog <- function(pattern, log, num = FALSE) {
  # put everything into one long string
  lines <- paste(log, collapse = " ")
  
  # perform gsub
  out <- gsub( 
    gsub(pattern, "\\1", lines),
    pattern = "\\s+",
    replacement ="")
  if (num) {
    as.numeric(out)
  } else {
    out
  }
}

get_metrics_from_log <- function(log, patterns) {
  CPU_TIME_seconds <- .extractFromLog(patterns$CPU_TIME, log, num=TRUE)
  CPU_Efficiency <- .extractFromLog(patterns$CPU_Efficiency, log, num=TRUE)
  MAX_MEM <- .extractFromLog(patterns$MAX_MEM, log, num=TRUE)
  AVG_MEM <- .extractFromLog(patterns$AVG_MEM, log, num=TRUE)
  CPU_PEAK <- .extractFromLog(patterns$CPU_PEAK, log, num=TRUE) 
  EFFICIENCY_MEM <- .extractFromLog(patterns$EFFICIENCY_MEM, log, num=TRUE) 
  
  list(
    CPU_TIME_seconds = CPU_TIME_seconds,
    CPU_Efficiency = CPU_Efficiency,
    MAX_MEM = MAX_MEM,
    AVG_MEM = AVG_MEM,
    EFFICIENCY_MEM = EFFICIENCY_MEM, 
    CPU_PEAK = CPU_PEAK)
}

# this function aims at reading the log of monolix execution based on the job ID number, and
# extract all necessary information to compare efficiency
# following the code later on, this function is by default applied when all the jobs are completed.
# still it can be applied while some jobs are still running or pending
# furthermore, this function is matching Novartis cluster and might require adjustment to
# be successful with the user's cluster
#'
#' @param jobID 
#'
#' @return
#' @export
#'
#' @examples
get_logs <- function(jobID, output.dir, tryBjobs=TRUE) {
  
  all_patterns <- list(
    bjobs = list(
      CPU_TIME = ".*CPU time used is ([0-9.-]+) sec.*",
      CPU_PEAK = ".*CPU PEAK: ([0-9.-]+).*",
      CPU_Efficiency = ".*CPU Efficiency: ([0-9.-]+).*",
      MAX_MEM = ".*MAX MEM: (\\d+).*",
      AVG_MEM = ".*AVG MEM: (\\d+).*",
      EFFICIENCY_MEM = ".*MEM Efficiency: ([0-9.-]+).*",
      name = ".*Job Name <(.*?)>(.*User)?.*",
      success = "Done successfully",
      fail = "Completed <exit>",
      end = ".* (\\w+ \\w+\\s+\\d+ \\d+:\\d+:\\d+): (Done successfully|Completed <exit>).*",
      start =  ".* (\\w+ \\w+\\s+\\d+ \\d+:\\d+:\\d+): Started.*",
      submit = ".* (\\w+ \\w+\\s+\\d+ \\d+:\\d+:\\d+): Submitted.*"
    ),
    ofile = list(
      CPU_TIME = ".*CPU time :\\s*([0-9.-]+) sec.*",
      CPU_PEAK = "DOES NOT EXIST",
      CPU_Efficiency = ".*Run time :\\s*([0-9.-]+) sec.*",
      MAX_MEM = ".*Max Memory :\\s*([0-9.-]+) MB.*",
      AVG_MEM = ".*Average Memory :\\s*([0-9.-]+) MB.*",
      EFFICIENCY_MEM = "DOES NOT EXIST",
      name = ".*Job \\d*: <(.*?)> in.*",
      success = "Successfully completed",
      fail = "Exited|killed",
      start = ".*Started at (.*?) \\d{4}.*",
      submit = ".*was submitted from host <.*?> by user <.*?> in cluster <.*?> at (.*?) \\d{4}.*",
      end = ".*Terminated at (.*?) \\d{4}.*"
    )
  )
  
  if (tryBjobs) {
    log_results <- system(paste0("bjobs -all ", jobID), intern = TRUE)
    message("Reading log .o file")
  }
  if (!tryBjobs || length(log_results) == 0) {
    # logs are not available anymore as the job was finished more than 24h ago.
    # we read data from the log file
    ofileMode <- TRUE
    
    filename <- list.files(
      file.path(output.dir, "runs"), recursive = TRUE,
      pattern = paste0("o", jobID))
    log_results <- readLines(
      file.path(output.dir, "runs",filename))
    patterns <- all_patterns$ofile
  } else {
    ofileMode <- FALSE
    
    patterns <- all_patterns$bjobs
  }
  filename <- list.files(
    file.path(output.dir, "runs"), recursive = TRUE,
    pattern = paste0("o", jobID))
  MLXname <- .extractFromLog(patterns$name, log_results)

    Submit_time <- .extractFromLog(patterns$submit, log_results)
    Started_time <- .extractFromLog(patterns$start, log_results)
    
    Submit_time <- strptime(Submit_time, format = "%a %b %d %H:%M:%S")
    Started_time <- strptime(Started_time, format = "%a %b %d %H:%M:%S")
 

  mlxfolder <- unlist(strsplit(MLXname, "_V", fixed = TRUE))[1]
  mlxresultfolder <- file.path(output.dir, "runs", mlxfolder, MLXname)
  # parse settings string
  V <- .parseSettingsString(MLXname, "V")
  N <- .parseSettingsString(MLXname, "N")
  n <- .parseSettingsString(MLXname, "n")
  W <- .parseSettingsString(MLXname, "W")
  M <- .parseSettingsString(MLXname, "M")
  rep <- .parseSettingsString(MLXname, "replicate")
  # default values for our metrics
  metrics <- list(
    CPU_TIME_seconds=NA,
    CPU_PEAK = NA,
    CPU_Efficiency=NA,
    MAX_MEM=NA,
    AVG_MEM=NA,
    EFFICIENCY_MEM=NA
  )
  
  # if the job is successful we collect all data from the log
  if (length(log_results[grep(patterns$success, log_results)]) == 1) {
    print(paste0("Job number ", jobID, ", ", MLXname))
    metrics <- get_metrics_from_log(log_results, patterns)

    
    Ending_time <- .extractFromLog(patterns$end, log_results)  
    # get logs from the monolix results and extract main information
    indata <- read.csv(file.path(mlxresultfolder, "populationParameters.txt" ))
    
    # have a closer look TODO
    init_values <- readLines(paste0(mlxresultfolder, ".mlxtran" ))
    init_values <- init_values[grep(init_values, pattern = "[{]value=[0-9.-]+, method=\\w+[}]")]
    # get a char vector of 'tuples'
    init_values <- tibble(
      inits=gsub(
        "^(\\w+) = [{]value=([0-9.-]+),.*", "\\1, \\2",
        init_values)
    )%>%
      tidyr::separate(inits, c("parameter", "value"), ", ") %>% 
      mutate(value = as.numeric(value))
    
    indata <- init_values %>%
      merge(indata, by = "parameter", suffixes = c("_init", ""))%>%
      filter(!value_init==0)
    
    Params <- indata %>%
      mutate(percentdiff = abs(round((value - value_init) / value_init * 100, 0))) %>%
      summarize(MAX = max(percentdiff)) %>%
      select(MAX) %>% pull()
    
    Status <- "Success"
    RSE <- max(indata$rse_sa, na.rm = TRUE)
    
  } else if (length(grep(patterns$fail, log_results)) == 1) {
    # failed
    print(paste0("Job number ", jobID, ", ", MLXname, ", failed"))
    error_message <- grep("Completed <exit>", log_results)
    print(log_results[c(error_message, error_message + 1)])
    # gather results
    Status <- "Failed"
    Params <- NA
    RSE <- NA
    Ending_time <- .extractFromLog(patterns$end, log_results)
  } else if (length(grep(": Started", log_results)) == 1) {
    # running
    print(paste0("Job number ", jobID, ", ", MLXname, ", still running"))
    Status <- "Running"
    metrics <- get_metrics_from_log(log_results, patterns)
    Params <- NA
    Ending_time <- NA
    RSE <- NA
  }else if(length(grep(" job killed after reaching LSF run time limit.", log_results)) == 1){
    print(paste0("Job number ", jobID, ", ", MLXname, "killed after reaching LSF run time limit."))
    Status <- "run time limit"
    Started_time <- NA
    Params <- NA
    Ending_time <- NA
    RSE <- NA
  } else { 
    # pending
    print(paste0("Job number ", jobID, ", ", MLXname, ", still pending"))
    Status <- "Pending"
    Started_time <- NA
    Params <- NA
    Ending_time <- NA
    RSE <- NA
    
    # the following applies if the job is not successful (either failed, running or pending)
  }
  # based on the previous jobs dates, we calculate pending and running time
  if(ofileMode){
    metrics$EFFICIENCY_MEM <- metrics$MAX_MEM/(M*N*n)*100
    metrics$CPU_Efficiency <- metrics$CPU_TIME_seconds / (metrics$CPU_Efficiency * (N * n)) * 100
    
  }
  
  WEEKEND <- weekdays(Submit_time) %in% c("Saturday", "Sunday")

  Ending_time <- strptime(Ending_time, format = "%a %b %d %H:%M:%S")
  if (!is.na(Started_time)) {
    Pending_time <- as.numeric(difftime(Started_time, Submit_time, units = "secs"))
    if (!is.na(Ending_time)) {
      Running_time <- as.numeric(difftime(Ending_time, Started_time, units = "secs"))
    } else {
      Running_time <- NA
    }
  } else {
    Pending_time <- NA
    Running_time <- NA
  }
  
  # gather results in one list
  results <- list(
    jobID = jobID,
    Model_name = MLXname,
    Status = Status,
    Params = Params,
    RSE = RSE,
    CPU_TIME_seconds = metrics$CPU_TIME_seconds,
    CPU_Efficiency = metrics$CPU_Efficiency,
    CPU_PEAK = metrics$CPU_PEAK,
    MAX_MEM = metrics$MAX_MEM,
    AVG_MEM = metrics$AVG_MEM,
    EFFICIENCY_MEM = metrics$EFFICIENCY_MEM,
    Pending_time_seconds = Pending_time,
    Running_time_seconds = Running_time,
    WEEKEND = as.character(WEEKEND),
    V = V,
    N = N,
    n = n,
    W = W,
    M = M,
    rep = rep
  )
  return(results)
}


# this function is aiming at gathering in one table the results from the "get_logs()"
# output: a list with one element for each run
extract_one_results <- function(one_res.) {
  raw_results <- one_res.
  
  raw_results$V <- NULL
  raw_results$N <- NULL
  raw_results$n <- NULL
  raw_results$W <- NULL
  raw_results$M <- NULL
  raw_results$rep <- NULL
  raw_results$Model_name <- NULL
  raw_results$jobID <- NULL
  raw_results$Status <- NULL
  Value <- unlist(raw_results, use.names = TRUE)
  result <- data.frame(Value)
  result$Metric <- row.names(result)
  result$V <- one_res.$V
  result$N <- one_res.$N
  result$n <- one_res.$n
  result$M <- one_res.$M
  result$W <- one_res.$W
  result$Status <- one_res.$Status
  result$Model_name <- one_res.$Model_name
  result$rep <- one_res.$rep
  result$jobID <- one_res.$jobID
  result$mlxfolder <- gsub(unlist(strsplit(one_res.$Model_name, "_V", fixed = TRUE))[1],
                           pattern = "_", replacement = " "
  )
  row.names(result) <- NULL
  row.names(result) <- NULL
  return(result)
}