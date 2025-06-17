library(tidyverse)
library(readxl)
library(sf)
library(rjson)

# once you load all the function below, you can run the main function
# make_ozfs(nza_files_folder = "", 
#           geom_files_folder = "", 
#           new_folder_to_save_to = "", 
#           col_descriptions_path = "",
#           extra_overlay_geom_file = NULL)


## takes a folder full of nza files and
## a folder full of geometry files that match the nza files in order
## and writes to a new folder all the ozfs .zoning files it created
make_ozfs <- function(nza_files_folder, 
                      geom_files_folder, 
                      new_folder_to_save_to, 
                      col_descriptions_path,
                      extra_overlay_geom_file = NULL){
  
  nza_files_list <- list.files(nza_files_folder, full.names = TRUE)
  geom_files_list <- list.files(geom_files_folder, full.names = TRUE)
  
  # import col_descriptions file
  if (file.exists(col_descriptions_path)){
    splt <- strsplit(col_descriptions_path, "[.]")[[1]]
    ext <- splt[[length(splt)]]
    if (ext == "csv"){
      col_descriptions <- read.csv(col_descriptions_path)
    } else if (ext == "xlsx"){
      col_descriptions <- read_excel(col_descriptions_path)
    } else{
      stop("col_descriptions must be .xlsx or .csv")
    }
  } else{
    stop("col_descriptions must be .xlsx or .csv")
  }
  
  # establish use_type_indicators
  use_type_indicators <- list(`1_unit` = "_1", 
                              `2_unit` = "_2",
                              `3_unit` = "_3",
                              `4_plus` = "_4",
                              `townhome` = "_th")
  
  # start empty lists to track errors
  ozfs_errors <- c()
  ozfs_warnings <- c()
  geom_errors <- c()
  geom_warnings <- c()
  overlay_errors <- c()
  overlay_warnings <- c()
  
  # loop through each NZA file
  for (i in 1:length(nza_files_list)){
    nza_file_path <- nza_files_list[[i]]
    
    file_name <- basename(nza_files_list[[i]])
    file_name_no_ext <- sub(".xlsx","",file_name)
    
    ozfs_list_format <- tryCatch(
      {
        # Code that might throw an error
        atlas_to_ozfs(nza_file_path, 
                      col_descriptions, 
                      use_type_indicators,
                      version_date = "2024-08-14")
      }, warning = function(w) {
        return(c("warning",e$message))
      }, error = function(e) {
        # Code to run if an error occurs
        return(c("error",e$message))
        
      }
    )
    
    if (ozfs_list_format[[1]] == "warning"){
      ozfs_warnings <- c(ozfs_warnings, ozfs_list_format[[2]])
    }
    if (ozfs_list_format[[1]] == "error"){
      ozfs_errors <- c(ozfs_errors, ozfs_list_format[[2]])
    }
    
    ozfs_list_with_geom <- tryCatch(
      {
        # Code that might throw an error
        add_geometry_to_ozfs(geom_files_list[[i]], ozfs_list_format)
      }, warning = function(w) {
        return(c("warning",e$message))
      },
      error = function(e) {
        # Code to run if an error occurs
        return(c("error",e$message))
        
      }
    )
    
    if (ozfs_list_with_geom[[1]] == "warning"){
      geom_warnings <- c(geom_warnings, ozfs_list_with_geom[[2]])
    }
    if (ozfs_list_with_geom[[1]] == "error"){
      geom_errors <- c(geom_errors, ozfs_list_with_geom[[2]])
    }
    
    
    if (!is.null(extra_overlay_geom_file)){
      ozfs_list_with_geom <- tryCatch(
        {
          # Code that might throw an error
          add_extra_overlays(extra_overlay_geom_file, ozfs_list_with_geom)
        }, warning = function(w) {
          return(c("warning",e$message))
        },
        error = function(e) {
          # Code to run if an error occurs
          return(c("error",e$message))
          
        }
      )
      
      if (ozfs_list_with_geom[[1]] == "warning"){
        overlay_warnings <- c(ozfs_warnings, ozfs_list_format[[2]])
      }
      if (ozfs_list_with_geom[[1]] == "error"){
        overlay_errors <- c(ozfs_errors, ozfs_list_format[[2]])
      }
    }
    
    new_file_directory <- paste0(new_folder_to_save_to, file_name_no_ext, ".zoning")
    write_list_as_json(ozfs_list_with_geom, new_file_directory)
  }
  
  if (!is.null(extra_overlay_geom_file)){
    list(ozfs_errors = ozfs_errors,
         ozfs_warnings = ozfs_warnings,
         geom_errors = geom_errors,
         geom_warnings = geom_warnings,
         overlay_errors = geom_errors,
         overlay_warnings = geom_warnings)
  } else{
    list(ozfs_errors = ozfs_errors,
         ozfs_warnings = ozfs_warnings,
         geom_errors = geom_errors,
         geom_warnings = geom_warnings)
  }
  
}

## Returns a list following OZFS standards 
## that is ready to be switched to a geojosn
atlas_to_ozfs <- function(nza_file_path, #Path to one NZA file (must be xlsx or csv)
                          col_descriptions, #col_descriptions data frame
                          use_type_indicators, #use_type_indicators list
                          version_date = "2024-08-14"){ #the date the zoning ordinance was updated
  
  if (file.exists(nza_file_path)){
    splt <- strsplit(nza_file_path, "[.]")[[1]]
    ext <- splt[[length(splt)]]
    if (ext == "csv"){
      atlas_df <- read.csv(nza_file_path)
    } else if (ext == "xlsx"){
      atlas_df <- read_excel(nza_file_path)
    } else{
      stop("nza_file_path must be .xlsx or .csv")
    }
  } else{
    stop("nza_file_path must be .xlsx or .csv")
  }
  
  # start the geojson list with an empty features list
  ozfs_format <- list(type = "FeatureCollection",
                      version = "0.5.0",
                      muni_name = atlas_df[[1,"muni_name"]],
                      date = version_date,
                      definitions = list(height = list(list(condition = "roof_type == 'hip'",
                                                            expression = "0.5 * (height_top + height_eave)"),
                                                       list(condition = "roof_type == 'mansard'",
                                                            expression = "height_deck"),
                                                       list(condition = "roof_type == 'gable'",
                                                            expression = "0.5 * (height_top + height_eave)"),
                                                       list(condition = "roof_type == 'skillion'",
                                                            expression = "0.5 * (height_top + height_eave)"),
                                                       list(condition = "roof_type == 'gambrel'",
                                                            expression = "0.5 * (height_top + height_eave)")),
                                         bldg_type = list(list(condition = "total_units == 1",
                                                               expression = "1_unit"),
                                                          list(condition = "total_units == 2",
                                                               expression = "2_unit"),
                                                          list(condition = "total_units == 3",
                                                               expression = "3_unit"),
                                                          list(condition = "total_units > 3",
                                                               expression = "4_plus"),
                                                          list(condition = list("total_units > 2", "outside_entrys == total_units"),
                                                               expression = "townhome"))),
                      features = list())
  
  
  # loop through each row of atlas_df
  for (i in 1:nrow(atlas_df)){
    atlas_row_df <- atlas_df[i,]
    ozfs_format$features[[i]] <- organize_feature(atlas_row_df, 
                                                  col_descriptions, 
                                                  use_type_indicators)
    
  }
  
  ozfs_format
}

## Returns a list representing one feature of the geojson 
## (one feature is one zoning district)
organize_feature <- function(atlas_row_df, 
                             col_descriptions, 
                             use_type_indicators){
  
  # create a list with a column for 
  separated_uses <- use_type_indicators
  uses_permitted <- c()
  for (i in 1:length(use_type_indicators)){
    use_name <- names(use_type_indicators)[[i]]
    indicator <- use_type_indicators[[i]]
    
    filtered_columns <- atlas_row_df[,grep(indicator, names(atlas_row_df))]
    
    names(filtered_columns) <- gsub(indicator,"",names(filtered_columns))
    
    if (!is.na(filtered_columns$use_permitted) & filtered_columns$use_permitted == "Allowed/Conditional"){
      uses_permitted <- c(uses_permitted, use_name)
    }
    
    separated_uses[[i]] <- filtered_columns
  }
  
  
  # start with a bare list for the features data to fill
  features_list <- list(type = "Feature", 
                        properties = list(),
                        geometry = list())
  
  # add the properties
  
  # dist_name
  if (!is.na(atlas_row_df[[1,"dist_name"]])){
    features_list$properties[["dist_name"]] <- atlas_row_df[[1,"dist_name"]]
  }
  
  # dist_abbr
  if (!is.na(atlas_row_df[[1,"dist_abbr"]])){
    features_list$properties[["dist_abbr"]] <- atlas_row_df[[1,"dist_abbr"]]
  }
  
  # planned_dev
  if (is.na(atlas_row_df[[1,"dist_abbr"]])){
    features_list$properties$planned_dev <- FALSE
  } else if (atlas_row_df[[1,"dist_abbr"]] == "PD" | atlas_row_df[[1,"dist_abbr"]] == "PRD"){
    features_list$properties$planned_dev <- TRUE
  } else{
    features_list$properties$planned_dev <- FALSE
  }
  
  # overlay
  if (is.na(atlas_row_df[[1,"overlay"]])){
    features_list$properties$overlay <- FALSE
  } else if (atlas_row_df[[1,"overlay"]] == "No"){
    features_list$properties$overlay <- FALSE
  } else{
    features_list$properties$overlay <- TRUE
    return(features_list)
  }
  
  # res_uses
  if (length(uses_permitted) > 0){
    features_list$properties$res_uses <- uses_permitted
  } else {
    features_list$properties$res_uses <- "none"
  }
  
  # constraints
  
  # filter col_descriptions to just the constraint columns this district has
  constraints_df <- col_descriptions |>
    filter(col_name %in% gsub("_[^_]+$","",names(atlas_row_df)))
  
  if (nrow(constraints_df) > 0){
    features_list$properties$constraints <- make_constraints(constraints_df, separated_uses)
  }
  
  if (length(features_list$properties$constraints) == 0){
    features_list$properties$constraints <- NULL
  }
  
  return(features_list)
}


make_constraints <- function(constraints_df, separated_uses){
  
  constraints_list <- list()
  for (i in 1:nrow(constraints_df)){
    constraint_name <- constraints_df$col_name[[i]]
    min_or_max <- constraints_df$min_or_max[[i]]
    
    if (min_or_max == "min"){
      minmax_vals <- c("min_val","max_val")
    } else{
      minmax_vals <- c("max_val","min_val")
    }
    
    for (minmax_loop in 1:2){
      
      if (minmax_loop == 2){
        new_constraint_name <- paste0(constraint_name, "_minmax")
      } else{
        new_constraint_name <- constraint_name
      }
      
      filtered_separated_uses <- separated_uses
      for (j in 1:length(separated_uses)){
        constraint_data <- separated_uses[[j]]
        filtered_data <- constraint_data |>
          select(grep(constraint_name,names(constraint_data), value = TRUE))
        
        if (minmax_loop == 1){
          filtered_data <- filtered_data |> 
            select(!grep("minmax",names(filtered_data), value = TRUE))
        } else {
          filtered_data <- filtered_data |> 
            select(grep("minmax",names(filtered_data), value = TRUE))
        }
        
        # if filtered_data is blank or NA, 
        # it means there is no value for that constraint
        # we make either of those scenarios NULL so that it gets the proper use groupings
        if (ncol(filtered_data) == 0){
          filtered_data <- NA
        } else if (rowSums(is.na(filtered_data)) == ncol(filtered_data)){
          filtered_data <- NA
        }
        
        
        
        filtered_separated_uses[[j]] <- filtered_data
      }
      
      # Create a unique key for each data frame by serializing to a character string
      key_vec <- sapply(filtered_separated_uses, function(x) paste(serialize(x, NULL), collapse = "-"))
      
      # Group the list element names by the serialization key
      grouped_uses <- split(names(filtered_separated_uses), key_vec)
      
      rule_list <- list()
      grouped_uses_count <- length(grouped_uses)
      for (use_group in grouped_uses){
        
        conditions_string <- paste0("bldg_type == ", use_group)
        use_condition <- paste(conditions_string, collapse = " or ")
        
        one_use <- use_group[[1]]
        use_df <- filtered_separated_uses[[one_use]]
        
        if (class(use_df)[[1]] == "logical"){
          grouped_uses_count <- grouped_uses_count - 1
          next
        }
        
        organized_rules <- organize_rules(use_df, new_constraint_name)
        
        if (grouped_uses_count > 1 & !is.null(organized_rules)){
          
          for (rule_num in 1:length(organized_rules)){
            organized_rules[[rule_num]]$condition <- append(organized_rules[[rule_num]]$condition, list(use_condition))
          }
        }
        
        rule_list <- append(rule_list, organized_rules)
      }
      
      # only add the organized rule list to the constraints list if it has values
      if (length(rule_list) > 0){
        constraints_list[[constraint_name]][[minmax_vals[[minmax_loop]]]] <- rule_list
      } 
    }
    
  }
  
  return(constraints_list)
  
}

organize_rules <- function(df_with_rules, constraint_name){
  
  # this is to check if the df_with_rules is NA
  # which means there was no value recorded
  if (class(df_with_rules)[[1]] == "logical"){
    return(NULL)
  }
  
  # find out how many rules there are
  counter <- 1
  df <- df_with_rules[1,grep(paste0("rule", counter), names(df_with_rules))]
  
  
  while (ncol(df) > 0){
    counter <- counter + 1
    df <- df_with_rules[1 ,grep(paste0("rule", counter), names(df_with_rules))]
  }
  
  rules_count <- counter - 1
  
  df_just_rules <- df_with_rules[1 ,grep(paste0(constraint_name, "_rule"), names(df_with_rules))]
  df_no_rules <- df_with_rules[1 ,constraint_name]
  
  if (!is.na(df_no_rules[[1,1]])){
    rule_list <- list(list(expression = list(df_with_rules[1,1][[1]])))
    return(rule_list)
  } else if (rowSums(is.na(df_just_rules)) == ncol(df_just_rules)){
    return(NULL)
  }
  
  # loop through each rule and make it a list
  all_rule_list <- list()
  for (i in 1:rules_count){
    # create a list that we will keep adding to
    rule_list <- list()
    
    # New df with an isolated rule
    rule_df <- df_with_rules[ ,grep(paste0(constraint_name,"_rule", i), names(df_with_rules))]
    
    # if it has one of the fields, we will add it to rule_list
    
    logical_operator <- NULL
    # logical_operator
    if (sum(grep("logical_operator", names(rule_df))) > 0 ){
      # assign value to logical_operator
      logical_operator <- rule_df[[1, grep("logical_operator",names(rule_df))]]
    }
    
    # conditions
    if (sum(grep("condition", names(rule_df))) > 0 ){
      # make a df to assign multiple values to the array of conditions
      condition_df <- rule_df[ , grep("condition",names(rule_df))]
      
      if (is.null(logical_operator)){
        condition_value <- condition_df[[1,1]]
      } else{
        condition_list <- c()
        for (j in 1:ncol(condition_df)){
          condition_list <- c(condition_list,condition_df[[1,j]])
          condition_value <- paste(condition_list, collapse = paste0(" ", tolower(logical_operator), " "))
        }
        
      }
      rule_list$condition <- append(rule_list$condition,list(condition_value))
      
    }
    
    # criterion
    if (sum(grep("criterion", names(rule_df))) > 0 ){
      
      criterion <- rule_df[[1, grep("criterion",names(rule_df))[1]]]
      
      if (criterion == "dependent"){
        if (sum(grep("more_restrictive", names(rule_df))) > 0 ){
          rule_list$condition <- append(rule_list$condition, rule_df[[1, grep("more_restrictive",names(rule_df))[1]]])
        } else{ # there is no explanation for some reason
          rule_list$condition <- append(rule_list$condition, "Special condition that wasn't stated")
        }
      } else{
        rule_list$criterion <- rule_df[[1, grep("criterion",names(rule_df))[1]]]
      }
    }
    
    # expression(s)
    if (sum(grep("expression", names(rule_df))) > 0 ){
      # make a df to see if it is more than one expression and to extract data
      df_expression <- rule_df[ , grep(paste0("expression"), names(rule_df))]
      
      for (j in 1:ncol(df_expression)){
        rule_list$expression[[j]] <- as.character(df_expression[[1,j]])
      }
      
    }
    # add each rule list to the total rules list
    all_rule_list[[i]] <- rule_list
  }
  
  return(all_rule_list)
}

write_list_as_json <- function(list, file_directory){
  json <- toJSON(list)
  write(json, file_directory)
}

# I might need to change this
# Where it will just assume each geometry and each excel 
# file already have the overlays it needs.
add_geometry_to_ozfs <- function(boundary_file_path, ozfs_list){
  
  boundaries <- rjson::fromJSON(file = boundary_file_path)
  for (i in 1:length(ozfs_list$features)){
    zoning_dist_abbr <- ozfs_list$features[[i]]$properties$dist_abbr
    city <- ozfs_list$features[[i]]$properties$muni_name
    for (j in 1:length(boundaries$features)){
      boundary_dist_name <- boundaries$features[[j]]$properties$`Abbreviated District Name`
      if (zoning_dist_abbr == boundary_dist_name){
        ozfs_list$features[[i]]$geometry <- boundaries$features[[j]]$geometry
      }
    }
  }
  ozfs_list
}

# Takes the output of the add_geometry_to_ozfs() function searches through 
# an sf object with extra overlays to add any features to the ozfs format. 
# The sf object must have dist_name, dist_abbr, muni_name, and geometry
add_extra_overlays <- function(extra_overlay_geom_file, ozfs_list){
  extra_overlays <- fromJSON(file = extra_overlay_geom_file)
  
  city_idx <- c()
  for (k in 1:length(extra_overlays$features)){
    if (extra_overlays$features[[k]]$properties$muni_name == ozfs_list$muni_name){
      city_idx <- c(city_idx, k)
    }
  }
  
  for (i in city_idx){
    overlay_feature_i <- extra_overlays$features[[i]]
    abbr <- overlay_feature_i$properties$dist_abbr
    
    # find the feature in ozfs_list
    feature <- 0
    for (j in 1:length(ozfs_list$features)){
      if (ozfs_list$features[[j]]$properties$dist_abbr == abbr){
        feature <- ozfs_list$features[[j]]
        break
      }
    }
    
    if (class(feature) == "list"){ # if the feature exists, we add geometry
      ozfs_list$features[[j]]$geometry <- overlay_feature_i$geometry
    } else{ # if the feature doesn't exist, we create a new feature and add it
      # add a new feature
      overlay_feature_i$properties$muni_name <- NULL
      overlay_feature_i$properties$planned_dev <- ifelse(abbr %in% c("PD","PRD"),TRUE,FALSE)
      overlay_feature_i$properties$overlay <- TRUE
      
      # add a the overlay feature to the end of the features
      ozfs_list$features[[length(ozfs_list$features) + 1]] <- overlay_feature_i
    }
    
    
  }
  
  return(ozfs_list) 
}
