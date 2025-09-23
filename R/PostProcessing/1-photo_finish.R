
args <- commandArgs(trailingOnly=TRUE)
# args[1] : path towards outputs
# args[2] : number of replicates
# args[3] : number of time steps
# args[4] : region
# args[5] : output (name of the directory where each "photo finish" will be stored)
# args[6] : model (M0, M1, ...)

n_rep <- as.numeric(args[2])
n_steps <- as.numeric(args[3])

if(!dir.exists(paste0(args[1], "/", args[5]))){
    print("Creating output directory")
    dir.create(paste0(args[1], "/", args[5]))}

for(i in 1:n_rep){
  pth <- paste0(args[1], "/config_", args[6], "_", args[4], "_America_run_", i, "/plots/richness/")
  if(file.exists(pth)){
    ls_files <- list.files(pth)
    last_step <- n_steps - length(ls_files)
    file.copy(from = paste0(pth, "richness_t_", last_step, ".png"),
              to = paste0(args[1], "/", args[5], "/RUN_", i, "_richness_t_", last_step, ".png"))
  }
  else{
      print(paste0("File doesn't exist: ", pth))
  }
}
