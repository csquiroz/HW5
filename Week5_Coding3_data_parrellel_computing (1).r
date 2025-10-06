# --- Setup: load base parallel, read data --------------------------------

library(parallel)                          # built-in; no install needed

fish <- read.csv("Data/fish_long.csv")     # adjust path if needed
species <- unique(fish$Species)            # list of species we'll loop over


# --- A tiny bootstrap function (no pipes, base R only) --------------------

boot_mean <- function(species_name, n_boot = 500, sample_size = 100) {
  # Pull the Length_cm vector for just this species
  x <- fish$Length_cm[fish$Species == species_name]
  
  # Do n_boot resamples WITH replacement; compute the mean each time
  # replicate(...) returns a numeric vector of bootstrap means
  means <- replicate(n_boot, mean(sample(x, size = sample_size, replace = TRUE)))
  
  # Return the average of those bootstrap means (a stable estimate)
  mean(means)
}


# --- SERIAL version: one core, one species after another ------------------

t_serial <- system.time({                   # time the whole serial run
  res_serial <- lapply(                     # loop over species in the main R process
    species,                                # input: vector of species names
    boot_mean,                              # function to apply
    n_boot = 500,                           # number of bootstrap resamples per species
    sample_size = 100                       # bootstrap sample size
  )
})

# head(res_serial)


# --- PARALLEL version: many cores using a PSOCK cluster (portable) --------

n_cores <- max(1, detectCores() - 1)        # use all but one core (be nice to your laptop)
cl <- makeCluster(n_cores)                  # start worker processes

clusterSetRNGStream(cl, iseed = 123)        # make random numbers reproducible across workers

# Send needed objects to workers (function + data + species vector)
clusterExport(cl, varlist = c("fish", "boot_mean", "species"), envir = environment())

t_parallel <- system.time({                 # time the parallel run
  res_parallel <- parLapply(                # same API as lapply(), but across workers
    cl,                                     # the cluster
    species,                                # each worker gets one species (or more)
    boot_mean,                              # function to run
    n_boot = 500,                           # same bootstrap settings as serial
    sample_size = 100
  )
})

stopCluster(cl)                             # always stop the cluster when done


# --- Compare runtimes & show speedup --------------------------------------

# Extract elapsed (wall) time and compute speedup = serial / parallel
elapsed_serial   <- unname(t_serial["elapsed"])
elapsed_parallel <- unname(t_parallel["elapsed"])
speedup <- elapsed_serial / elapsed_parallel

cat("Serial elapsed (s):   ", round(elapsed_serial, 3), "\n")
cat("Parallel elapsed (s): ", round(elapsed_parallel, 3), " using ", n_cores, " cores\n", sep = "")
cat("Speedup:               ", round(speedup, 2), "x\n", sep = "")

