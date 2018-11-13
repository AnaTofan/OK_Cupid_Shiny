library(dplyr)
library(rvest)
library(rlang)
library(tm)
library(wordcloud)
library(rflow)
library(ggplot2)
library(SnowballC)
library(RColorBrewer)
library(wesanderson)
library(scales)
library(stringr)
library(fs)
library(viridis)

# in utils all the functions that 
# hide the complexity of the application can be found
source("constants.R")
source("utils.R")
# try to set up proper cache for time consuming functions due to the
# computations performed on a large dataset
# If the cache is not yet configured, create a new one
tryCatch({
    rflow::set_current_eddy("cupid_eddy")
}, error = function(e) {
    configure_function_cache("cache", "cupid_eddy")
    rflow::set_current_eddy("cupid_eddy")
})

cupid_data_flow <- rflow::flow_fn("3-profiles.csv",
                                  fn = import_clear_data)
cupid_data <- cupid_data_flow %>%
                   collect()

# Run once to generate files with descriprion by job
if (!fs::dir_exists("description_by_job")) {
    for (job in levels(as.factor(cupid_data$job))) {
        generate_essay_file(cupid_data, job)
    }
}


