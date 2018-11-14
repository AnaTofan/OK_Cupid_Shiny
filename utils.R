# this functions save computational time while cacheing 
# the results of seasonal partitioning procedure
configure_function_cache <- function(cache_dir, cache_name) {
    cache_location <- rflow::cache_file(cache_dir = cache_dir)
    crosby_cache_eddy <- rflow::new_eddy(cache_name, 
                                         cache = cache_location)
    crosby_cache_eddy
}

import_clear_data <- function(file_source) {
    
    cupid_data <- read.csv(file_source, 
                           header = TRUE,
                           na.strings = c(""," ","NA"))
    
    cupid_data[cupid_data$income == -1, "income"] <- NA
    cupid_data <- cupid_data[cupid_data$age < 70, ]
    cupid_data <- cupid_data %>% 
      mutate(religious_affil = gsub(" [A-z ]*", "", as.character(religion)))
    cupid_data$agecat <- NA
    cupid_data$agecat[cupid_data$age > 17 & cupid_data$age <= 21] <- "18-21"
    cupid_data$agecat[cupid_data$age > 21 & cupid_data$age <= 25] <- "22-25"
    cupid_data$agecat[cupid_data$age > 25 & cupid_data$age <= 29] <- "26-29"
    cupid_data$agecat[cupid_data$age > 29 & cupid_data$age <= 33] <- "30-33"
    cupid_data$agecat[cupid_data$age > 33 & cupid_data$age <= 37] <- "34-37"
    cupid_data$agecat[cupid_data$age > 37 & cupid_data$age <= 41] <- "38-41"
    cupid_data$agecat[cupid_data$age > 41 & cupid_data$age <= 45] <- "42-45"
    cupid_data$agecat[cupid_data$age > 45 & cupid_data$age <= 49] <- "46-49"
    cupid_data$agecat[cupid_data$age > 49 & cupid_data$age <= 53] <- "50-53"
    cupid_data$agecat[cupid_data$age > 53 & cupid_data$age <= 57] <- "54-57"
    cupid_data$agecat[cupid_data$age > 57 & cupid_data$age <= 61] <- "58-61"
    cupid_data$agecat[cupid_data$age > 61 & cupid_data$age <= 65] <- "62-65"
    cupid_data$agecat[cupid_data$age > 65 & cupid_data$age <= 69] <- "66-69"
    cupid_data$agecat[cupid_data$age > 69] <- "70+"
    cupid_data$agecat <- as.factor(cupid_data$agecat)
    cupid_data
}


group_and_count_factors <- function(data, group_var, remove_na = TRUE) {
    new_data <- data
    if (remove_na) {
        new_data <- new_data[!is.na(new_data[[group_var]]), ]
    }
    new_column_name <- paste0(group_var, '_number')
    new_data[[group_var]] <- as.factor(new_data[[group_var]])
    group_var <- sym(group_var)
    new_column_name <- sym(new_column_name)
    # omit unmentioned
    grouped_data <- new_data %>%
        dplyr::group_by(!!group_var) %>%
        dplyr::summarise(count = length(!!group_var))
    grouped_data
}


group_and_count_factors2 <- function(data_o, group_var,
                                     group_var2, remove_na = TRUE) {
    data <- data_o
    if (remove_na) {
        data <- data[!is.na(data[[group_var]]), ]
        data <- data[!is.na(data[[group_var2]]), ]
    }
    new_column_name <- paste0(group_var, '_number')
    data[[group_var]] <- as.factor(data[[group_var]])
    data[[group_var2]] <- as.factor(data[[group_var2]])
    group_var <- sym(group_var)
    group_var2 <- sym(group_var2)
    
    new_column_name <- sym(new_column_name)
    # omit unmentioned
    grouped_data <- data %>%
        dplyr::group_by(!!group_var, !!group_var2) %>%
        dplyr::summarise(count = n())
    grouped_data
}


aggregate_text_by_column <- function(cupid_data, column1, column2) {
    
    if (!is.factor(cupid_data[[column1]])) {
        cupid_data[[column1]] <- as.factor(cupid_data[[column1]])
    }
    cupid_data <- cupid_data[!is.na(cupid_data[[column1]]), ]
    cupid_data <- cupid_data[!is.na(cupid_data[[column2]]), ]
    aggregated_by_column <- aggregate(
        cupid_data[[column2]], cupid_data[column1], paste, collapse = ' ')

    aggregated_by_column <- aggregated_by_column %>%
        dplyr::mutate(x =
                    gsub(pattern = "<.*>",
                         replacement = " ", x = x))
    
    aggregated_by_column <- aggregated_by_column %>%
        dplyr::mutate(x =
                          gsub(pattern = "\n",
                               replacement = " ", x = x))
    
    aggregated_by_column
}


getTermMatrix <- function(jobs) {
    
    main_job <- gsub("([A-Za-z]+).*", "\\1", jobs)
    job_path <- paste0("description_by_job/", main_job, "_essay.txt")
    essay <- readLines(job_path,
                      encoding = "UTF-8")
    essay <- paste(essay, collapse = ' ')
    essay <- gsub(pattern = "<br />",
                  replacement = " ", x = essay)
    essay <- gsub(pattern = "href",
                  replacement = " ", x = essay)
    essay <- gsub(pattern = "<a",
                  replacement = " ", x = essay)
    essay <- gsub(pattern = "</a>",
                  replacement = " ", x = essay)
    myCorpus = Corpus(VectorSource(essay))
    myCorpus = tm_map(myCorpus, content_transformer(tolower))
    myCorpus = tm_map(myCorpus, removePunctuation)
    myCorpus = tm_map(myCorpus, removeNumbers)
    myCorpus = tm_map(myCorpus, removeWords,
                      c(stopwords("en"),
                        "book", "books", "read", "like",
                        "movie", "movies", "food", "classilink"))
    myCorpus <- tm_map(myCorpus, stripWhitespace)
    #myCorpus <- tm_map(myCorpus, stemDocument)
    myDTM = TermDocumentMatrix(myCorpus
                               ,control = list(minWordLength = 4,
                                               wordLengths = c(4,Inf)))
   
    m = as.matrix(myDTM)
    m <- sort(rowSums(m), decreasing = TRUE)
    m  
}

generate_essay_file <- function(cupid_data, job) {
    main_job <- gsub("([A-Za-z]+).*", "\\1", job)
    cupid_new <- cupid_data %>%
        sample_n(10000) %>%
        dplyr::filter(job == job,
                      !is.na(essay2)) %>%
        dplyr::select(essay2)
    
    file_path <- paste0("description_by_job/", main_job, "_essay.txt")
	fs::dir_create("description_by_job/")
	fs::file_create(file_path)
    write.table(cupid_new, file = file_path, sep = " ")
}


modify_offspring <- function(cupid_data) {
    data <- cupid_data
    data <- data[!is.na(data$offspring), ]
    data$offspring <- as.character(data$offspring)
    
    which_has <- which(str_detect(data$offspring, "^has"))
    data[which_has, "offspring"] <- "has kids"
    
    which_doesnt <- which(str_detect(data$offspring, "^does"))
    data[which_doesnt, "offspring"] <- "doesn't have"
    
    which_wants <- which(str_detect(data$offspring, "might want kids"))
    data[which_wants, "offspring"] <- "wants kids"
    data$offspring <- as.factor(data$offspring)
    data
}


create_density_plot <- function(cupid_data, column,
                                column2, exclude_na  = TRUE) {
    cupid_data_wna <- cupid_data
    if (exclude_na) {
        cupid_data_wna <- cupid_data_wna[!is.na(cupid_data[[column]]),]
        cupid_data_wna <- cupid_data_wna[!is.na(cupid_data[[column2]]),]
        if (column2 == "income") {
            cupid_data_wna <- cupid_data_wna[cupid_data[[column2]] != -1,]
        }
    }
    
    if (column == "offspring") {
        cupid_data_wna <- modify_offspring(cupid_data_wna)
    }
    # The palette with grey:
    cbPalette <- c("#E69F00", "#56B4E9", "#009E73",
                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  #browser()
    # Plot
   # cupid_data_wna[[column]] <- as.factor(cupid_data_wna[[column]])
    colors <- c(viridis::viridis(n = 7))
    ggplot2::ggplot(cupid_data_wna, aes_string(column2, fill = column)) +
        ggplot2::geom_density(position = "fill", size = 0.9, 
                              color = "#0c0226") +
     
        ggplot2::ggtitle(paste0("Density of ", column ," by age")) +
        ggplot2::theme_minimal() +
        scale_color_viridis(discrete = TRUE, option = "D") +
        scale_fill_viridis(discrete = TRUE) 
}

create_bar_count_plot <- function(cupid_data, bar_col,
                                  count_col, remove_na = TRUE) {
    
    new <- group_and_count_factors(cupid_data, bar_col, remove_na)
   
    ggplot2::ggplot(new, 
                    aes_string(x = 
                    paste0("reorder(",bar_col,",",count_col ,")"),
                    y = count_col)) +
        ggplot2::geom_bar(stat = "identity", fill = "#DF013A") +
        ggplot2::coord_flip() +
        ggplot2::ggtitle(paste0("Number of users by ", bar_col)) +
        ggplot2::xlab(bar_col) +
        ggplot2::theme_minimal()
} 


create_stacked_bar_plot <- function(cupid_data, column,
                                  column2, remove_na = TRUE) {
    
    
    new2 <- group_and_count_factors2(cupid_data, column,
                                     column2, remove_na)
 
    ggplot(new2, aes_string(x = column, y = "count", fill = column2)) + 
        geom_bar(position = "fill", stat = "identity", color = "#8A0829",
                 size = 0.7) +
        scale_y_continuous(labels = percent_format()) +
        ggplot2::coord_flip() +
        scale_fill_brewer(palette = "PuRd") +
        ggplot2::ggtitle(paste0("Stack of ", column2, " categories ",
                                "grouped by ", column))  + 
        ggplot2::ylab("Percentage (%)")  +
        ggplot2::theme_minimal()
}


