group_integers <- function(values, threshold) {

  names(values) <- 1:length(values)
  # values <- sort(values, decreasing = TRUE) # Sort values in descending order

  groups_idx <- list(list("sum" = values[[names(values)[[1]]]], "indices" = c(names(values)[[1]])))

  for (val in names(values)[2:length(names(values))]) {

    which_less_thr <- which(sapply(groups_idx, `[[`, 1) + values[[val]] <= threshold)

    if (identical(which_less_thr, integer(0))) {
      groups_idx <- append(groups_idx, list(list("sum" = values[[val]], "indices" = c(val))))
    } else {
      idx <- min(which_less_thr)
      groups_idx[[idx]][["indices"]] <- c(groups_idx[[idx]][["indices"]], val)
      groups_idx[[idx]][["sum"]] <- groups_idx[[idx]][["sum"]] + values[[val]]
    }
  }

  return(groups_idx)
}

# Function to assign groups
assign_groups <- function(values, threshold) {
  groups <- group_integers(values, threshold)

  group_assignment <- rep(1:length(groups), sapply(lapply(groups, `[[`, 2), length))
  names(group_assignment) <- unlist(lapply(groups, `[[`, 2))
  group_assignment <- setNames(as.integer(names(group_assignment)), group_assignment)
  group_assignment <- sort(group_assignment)
  group_assignment <- setNames(as.integer(names(group_assignment)), NULL)

  return(group_assignment)
}

AndDiff = function(cond){
  Reduce(
    function(x, y) call("&", x, y),
    lapply(cond, function(var) call("!=", var, paste0("i.", var)))
  )
}
