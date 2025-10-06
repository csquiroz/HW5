#### Homework 5 - Zoo 800 ####
### 02-Oct-25 ###
### Cody Quiroz ###

## Problem 1 ##
library("readxl")
fish_csv <- read.csv("fish.csv") #import csv
fish_xlsx <- read_excel("fish.xlsx") #import xlsx
fish_rds <- readRDS("fish.rds") #import rds

head(fish_csv[1:5,]) #print only the first 5 rows of csv

## Problem 2 ##
dir.create("Output", showWarnings = FALSE) #create Output folder for new files
write.csv(fish_csv, "Output/fish_new.csv") #save as csv
writexl::write_xlsx(fish_csv, "Output/fish_new.xlsx") #save as xlsx
saveRDS(fish_csv, "Output/fish_new.rds") #save as rds
file.info("Output/fish_new.csv", "Output/fish_new.xlsx", "Output/fish_new.rds") #view the size of each file for comparison

#Of all the file types, .rds is by far the most compact. The .csv and .xlsx are very similar in size. I think that .xlsx files are best for sharing because of the formatting.


## Problem 3 ##
library("dplyr")
library("ggplot2")

fish_output <- read.csv("fish.csv") %>%
  filter(
    Species %in% c("Walleye", "Yellow Perch", "Smallmouth Bass"), #filter for select fishes
    Lake %in% c("Erie", "Michigan") #filter for lake
  ) %>%
  select("Species", "Lake", "Year", "Length_cm", "Weight_g") %>% #select only these columns
  mutate(Length_mm = Length_cm * 10, #create length (mm) column
       Length_group = cut(Length_mm, #create group column by length (mm)
                          breaks = c(-Inf, 200, 400, 600, Inf), #sets the breaks
                          labels = c("<200", "200-400", "400-600", ">600")) #name the groups 
       ) %>%
  group_by(Species, Year) %>% #pulls species/year columns
  summarise(
    mean_weight = mean(Weight_g, na.rm = TRUE), #calc mean weight
    median_weight = median(Weight_g, na.rm = TRUE), #calc median weight
    sample_size = n() #find sample size
  ) 
  print(fish_output) #print output
  
#plotting avg fish weight over time by species
ggplot(fish_output, aes(x = Year, y = mean_weight, color = Species)) + #making plot
  geom_line() + #making line plot
  labs(
    x = "Year", #labeling
    y = "Average Weight (g)"
  ) +
  theme_minimal()

write.csv(fish_output, "Output/fish_output.csv", row.names = FALSE) #saving data to Output folder

## Problem 4 ##

files <- list.files(path = "Multiple_files", pattern = "\\.csv$", full.names = TRUE) #call Multiple_files and assign all to variable
fish_all <- files %>% #read & combine all
  lapply(read.csv) %>%     #read each csv
  bind_rows()              #combine into a df


## Problem 5 ##
# Task 1 #
fish_data <- read.csv("fish_bootstrap_parallel_computing.csv") #call csv
fish_erie <- fish_data %>% #filter for lake erie
  filter(Lake == "Erie")
species_list <- unique(fish_erie$Species) #find unique species
n_resamples <- 10000 #set resample number
sample_size <- 200 #set sample size
set.seed(123)  #for reproducibility
bootstrap_results <- lapply(species_list, function(sp) { #perform resampling using species
  sp_data <- filter(fish_erie, Species == sp) #only species in lake erie
  replicate(
    n_resamples,
    mean(sample(sp_data$Weight_g, sample_size, replace = TRUE), na.rm = TRUE) #mean of weight
  )
})

names(bootstrap_results) <- species_list

bootstrap_summary <- data.frame( #summary by getting mean of means per species
  Species = species_list,
  Mean_bootstrap_weight = sapply(bootstrap_results, mean)
)

print(bootstrap_summary) #take a look

# Task 2 #
library(parallel)

#Serial mode
species_list <- unique(fish_erie$Species) #get species
boot_mean_weight <- function(species_name, n_resamples = 500, sample_size = 100) { #find means and make function
  x <- fish_erie$Weight_g[fish_erie$Species == species_name]
  if (length(x) < sample_size) return(NA)
  means <- replicate(n_resamples, mean(sample(x, size = sample_size, replace = TRUE), na.rm = TRUE))
  mean(means, na.rm = TRUE)
}
set.seed(123) #reproducibility
t_serial <- system.time({ #find time to run serial
  res_serial <- lapply(species_list, boot_mean_weight, n_resamples = 10000, sample_size = 200)
})

#Parallel mode
detectCores() #how many cores my computer has
n_cores <- max(1, detectCores() - 1) #find cores
cl <- makeCluster(n_cores)
clusterSetRNGStream(cl, iseed = 123) 
clusterExport(cl, varlist = c("fish_erie", "boot_mean_weight", "species_list"), envir = environment()) #export
t_parallel <- system.time({ #find time to run parallel
  res_parallel <- parLapply(cl, species_list, boot_mean_weight, n_resamples = 10000, sample_size = 200)
})
stopCluster(cl) #end

#Comparisons
elapsed_serial <- unname(t_serial["elapsed"]) #time for serial
elapsed_parallel <- unname(t_parallel["elapsed"]) #time for parallel
speedup <- elapsed_serial / elapsed_parallel #compare

cat("Cores used: ", n_cores, "\n") #how many cores used
cat("Serial elapsed (s): ", round(elapsed_serial, 3), "\n") #time for serial
cat("Parallel elapsed (s): ", round(elapsed_parallel, 3), "\n") #time for paralel
cat("Speedup: ", round(speedup, 2), "x\n") #comparison
