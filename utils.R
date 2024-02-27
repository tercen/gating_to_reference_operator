
#####
### Workflow queries
get_workflow_id <- function(ctx) {
  if (is.null(ctx$task)) {
    return(ctx$workflowId)
  } else {
    workflowIdPair <-
      Find(function(pair)
        identical(pair$key, "workflow.id"),
        ctx$task$environment)
    workflowId <- workflowIdPair$value
    return(workflowId)
  }
}

get_step_id <- function(ctx) {
  if (is.null(ctx$task)) {
    return(ctx$stepId)
  } else {
    stepIdPair <-
      Find(function(pair)
        identical(pair$key, "step.id"),
        ctx$task$environment)
    stepId <- stepIdPair$value
    return(stepId)
  }
}


get_data_step <- function(ctx) {
  wf <- ctx$client$workflowService$get(get_workflow_id(ctx))
  ds <-
    Find(function(s)
      identical(s$id, get_step_id(ctx)), wf$steps)
  return(ds)
}

get_gating_step <- function(ctx, id = 1) {
  wf <- ctx$client$workflowService$get(get_workflow_id(ctx))
  ds <- Find(function(s) !is.null(s$model$operatorSettings$operatorModel$roots), wf$steps)
  return(ds)
}

# Function to recursively traverse the tree and collect paths
collect_paths <- function(edges_df, node, current_path) {
  current_path <- c(current_path, node)
  
  children <- edges_df$child[edges_df$parent == node]
  
  if (length(children) == 0) {
    return(list(current_path))
  } else {
    all_paths <- list()
    for (child in children) {
      all_paths <- c(all_paths, collect_paths(edges_df, child, current_path))
    }
    return(all_paths)
  }
}

edgeListToPaths <- function(edges_df) {

  root_nodes <- setdiff(unique(edges_df$parent), edges_df$child)
  
  all_paths <- list()
  for (root in root_nodes) {
    all_paths <- c(all_paths, collect_paths(edges_df, root, character(0)))
  }
  
  max_length <- max(sapply(all_paths, length))
  paths_df <- do.call(rbind, lapply(all_paths, function(x) c(x, rep(NA, max_length - length(x)))))
  
  colnames(paths_df) <- paste0("Level_", 1:max_length)
  return(as.data.frame(paths_df))
}

getGateNodeFactors <- function(GateNode, parent) {
  x_name <- GateNode$crosstab$axis$xyAxis[[1]]$xAxis$graphicalFactor$factor$name
  y_name <- GateNode$crosstab$axis$xyAxis[[1]]$yAxis$graphicalFactor$factor$name
  return(c(gate_name = GateNode$name, parent = parent, x_name = x_name, y_name = y_name))
}

applyRecursively <- function(GateNode, parent = "root") {
  result <- getGateNodeFactors(GateNode, parent)
  results <- list(result)
  
  # Recursively apply the function to all children
  if (!is.null(GateNode$children)) {
    childResults <- lapply(GateNode$children, applyRecursively, parent = GateNode$name)
    results <- c(results, childResults)
  }
  return(results)
}

flattenList <- function(lst) {
  out <- list()
  for (i in seq_along(lst)) {
    if (is.list(lst[[i]])) {
      out <- rbind(out, flattenList(lst[[i]]))
    } else {
      out <- rbind(out, lst[[i]])
    }
  }
  return(out)
}


fill_na_with_repetition <- function(x) {
  last_non_na <- NULL
  for (i in 1:length(x)) {
    if (!is.na(x[i])) {
      last_non_na <- x[i]
    } else if (!is.null(last_non_na)) {
      x[i] <- last_non_na
    }
  }
  return(x)
}
