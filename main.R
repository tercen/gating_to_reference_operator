library(tercen)
library(dplyr)
source("./utils.R")

ctx <- tercenCtx()

ds_gating <- get_gating_step(ctx, id = 1)
gts <- ds_gating$model$operatorSettings$operatorModel$roots

resultsList <- lapply(gts, applyRecursively)

flattenedResults <- flattenList(resultsList) %>% as.data.frame()
flattenedResults$x_parent <- c(NA, unlist(sapply(flattenedResults[, "parent"], function(x) flattenedResults$x_name[flattenedResults$gate_name == x])))
flattenedResults$y_parent <- c(NA, unlist(sapply(flattenedResults[, "parent"], function(x) flattenedResults$y_name[flattenedResults$gate_name == x])))
flattenedResults$has_children <- flattenedResults$gate_name %in% flattenedResults$parent

df_out <- lapply(flattenedResults, unlist) %>% as_tibble

mat <- ctx$as.matrix()
df_col <- ctx$cselect() %>% tidyr::unite(col = "col_names")
df_row <- ctx$rselect() %>% tidyr::unite(col = "row_names")
colnames(mat) <- df_col$col_names
rownames(mat) <- df_row$row_names

scaled <- apply(mat, 2, function(x) {
  min_val <- min(x)
  max_val <- max(x)
  scaled <- (x - min_val) / (max_val - min_val) * 2 - 1
  return(scaled)
})

# Get and filter out unused markers
edges_df <- data.frame(
  parent = df_out$parent,
  child = df_out$gate_name
)
paths <- edgeListToPaths(edges_df)

paths_filled <- apply(paths, 1, fill_na_with_repetition) %>% t()

# get, for each leaf, the list of markers
mk_list <- apply(paths_filled, 1, function(x) {
  df_out %>% 
    filter(gate_name %in% unique(x)) %>%
    select(x_parent, y_parent) %>%
    unlist() %>%
    unique()
})


# Mask data (option)
sc_out <- scaled[idx, ]
idx <- paste0(ds_gating$model$operatorSettings$namespace, ".", paths_filled[, ncol(paths_filled)])
cn <- unlist(lapply(strsplit(colnames(sc_out), "\\."), "[", 2))
for(i in 1:nrow(paths_filled)) {
  sc_out[i, !colnames(sc_out) %in% mk_list[[i]]] <- NA
}
colnames(sc_out) <- cn


colnames(paths_filled)[length(colnames(paths_filled))] <- "Population"
out <- bind_cols(as_tibble(paths_filled), as_tibble(sc_out[idx, ]))

out %>%
  tidyr::pivot_longer(!contains(c("Level", "Population")), names_to = "Marker", values_to = "Value") %>%
  # ctx$addNamespace() %>%
  as_relation() %>%
  as_join_operator(list(), list()) %>%
  save_relation(ctx)

