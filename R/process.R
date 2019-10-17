library(rjson)
library(tidyverse)

source("downsample.R")
source("denoise.R")

persons = c("01", "04", "05", "06", "07", "08", "10", "11", "12")
decay_wave_lengths = c(2.5, 5, 7.5, 10, 25, 35, 50, 75)
decay_freqs = 1 / decay_wave_lengths

for (p in persons) {
  df <- read.csv(str_interp("./data/Subject${person}_On-RoadDrive_pp.csv", list(person=p)))
  df <- subset(df, select=c("Frame.", "Time", "Perspiration"))
  df_downsampled = downsample_using_mean(df, c("Perspiration"))
  
  pp <- as.numeric(df_downsampled[["Perspiration"]])
  
  pp <- pp[!is.na(pp)]
  len_pp <- length(pp)
  Time <- seq(1, len_pp)
  
  data_tb = cbind(Time, pp)
  
  # Enable Impluse removing
  for (decay_f in decay_freqs) {
    pp_nr = remove_noise(pp, removeImpluse = T, decay_f)
    data_tb <- cbind(data_tb, pp_nr)
  }
  
  # Disable Impluse removing
  for (decay_f in decay_freqs) {
    pp_nr = remove_noise(pp, removeImpluse = F, decay_f)
    data_tb <- cbind(data_tb, pp_nr)
  }
  
  df_out <- data.frame(data_tb)
  names(df_out) <- c(c("Time", "pp"), paste0("pp_nr", decay_wave_lengths), paste0("pp_nr_noi", decay_wave_lengths))
  
  # Store to JSON for visualization
  pp_json <- toJSON(unname(split(df_out, 1:nrow(df_out))))
  write(pp_json, str_interp("../../apps/visualization/data/Participant${person}/pp_nr.json", list(person=p)))
}
