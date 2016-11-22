library(rethomics)
library(tools)

############# Fetch data and add conditions
#files <- list.files("/Users/diana/Dropbox/Phd Stuff/ESA/data_exp/all_db_mf", pattern="*.db",full.names = T)
files <- list.files("/home/diana/Dropbox/Phd Stuff/Presentations/ESA/data_exp/all_db_mf", pattern="*.db",full.names = T)
query <- data.table(path=files)
list_of_fields <- strsplit(basename(files),'[-.]')
fields_mat <- do.call("rbind", list_of_fields)

conditions <- as.data.table(fields_mat[,c(10,12)])
setnames(conditions, c("DAM","fly_id"))
query <- cbind(query, conditions)

dt_maze <- loadEthoscopeData(query, FUN = motionAnnotation)

dt_maze[, side := ifelse(x < 0.68, "left", "right")]
dt_maze[, sleep_deprived := ifelse(DAM %in% c("005", "016", "004"), "sleep_deprived", "control")]
dt_maze[, sex := ifelse(DAM %in% c("008", "016", "004", "009"), "female", "male")]



# All flies start at time 0.00
dt_maze[, t:= t - min(t), by=key(dt_maze)]


bdt_side <- boutAnalysis(side, dt_maze) 


curateSparseRoiData <- function (data, window = 60, min_points = 20) 
{
  d <- copy(data)
  d[, `:=`(t_w, window * floor(t/window))]
  sparsity <- d[, `:=`(t_w, window * floor(t/window))]
  d[, `:=`(sparsity, .N), by = t_w]
  d <- d[sparsity > min_points, ]
  d$t_w <- NULL
  d$sparsity <- NULL
  d
}



motionAnnotation <- function (data, time_window_length = 1, motion_classifier_FUN = maxVelocityClassifier, ...) 
{
  d <- copy(data)
  ori_keys <- key(d)
  d <- curateSparseRoiData(d)
  if (nrow(d) < 1) 
    return(NULL)
  d[, `:=`(t_round, time_window_length * floor(d[, t]/time_window_length))]
  setkeyv(d, "t_round")
  d_small <- motion_classifier_FUN(d, ...)
  if (key(d_small) != "t_round") 
    stop("Key in output of motion_classifier_FUN MUST be `t_round'")
  setnames(d_small, "t_round", "t")
  d$t <- NULL
  d_small <- d_small[unique(d)]
  t_out <- seq(from = d_small[1, t], to = d_small[.N, t], 
               by = time_window_length)
  time_map <- data.table(t = t_out, key = "t")
  missing_val <- time_map[!d_small]
  d_small <- d_small[time_map, roll = T]
  d_small[, `:=`(is_interpolated, FALSE)]
  d_small[missing_val, `:=`(is_interpolated, TRUE)]
  d_small[is_interpolated == T, `:=`(moving, FALSE)]
  setkeyv(d_small, ori_keys)
  na.omit(d_small)
}

