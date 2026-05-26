accum <- function (pool, histvars, time_name, t, id_name)
{
  if (t == 0) {
    lapply(
      histvars,
      FUN = function(histvar) {
        pool[get(time_name) == t, `:=`((paste("cum_",
                                              histvar, sep = "")), as.double(pool[get(time_name) ==
                                                                                    t][[histvar]]))]
      }
    )
  }
  else {
    current_ids <- unique(pool[get(time_name) == t][[id_name]])
    lapply(
      histvars,
      FUN = function(histvar) {
        pool[get(time_name) == t, `:=`((paste("cum_",
                                              histvar, sep = "")),
                                       pool[pool[[time_name]] ==
                                              t &
                                              get(id_name) %in% current_ids][[histvar]] +
                                         pool[pool[[time_name]] ==
                                                t - 1 &
                                                get(id_name) %in% current_ids][[paste("cum_",
                                                                                      histvar, sep = "")]])]
      }
    )
  }
  
}

increment <- function (pool, histvars, time_name, t, id_name) 
{
    if (t == 0) {
      current_ids <- unique(pool[pool[[time_name]] == t][[id_name]])
      lapply(histvars, FUN = function(histvar) {
        pool[pool[[time_name]] == t, `:=`((paste(histvar, "_incr", sep = "")), pool[pool[[time_name]] == 
                                                                                      t & get(id_name) %in% current_ids][[histvar]])]
      })
    } else {
      current_ids <- unique(pool[pool[[time_name]] == t][[id_name]])
      lapply(histvars, FUN = function(histvar) {
        pool[pool[[time_name]] == t, `:=`((paste(histvar, "_incr", sep = "")), pool[pool[[time_name]] == 
                                                                                                  t - 1 & get(id_name) %in% current_ids][[paste(histvar, "_incr", sep = "")]] + 1)]
      })
    }
}



random <- function (pool, histvars, time_name, t, id_name) 
{
  current_ids <- unique(pool[pool[[time_name]] == t][[id_name]])
  lapply(histvars, FUN = function(histvar) {
    pool[pool[[time_name]] == t, `:=`((paste("rand", "_", histvar, sep = "")), rnorm(nrow(pool[pool[[time_name]] == 0 & get(id_name) %in% current_ids])))]
  })
  
}

incr_fit <- function(...) {}

incr_pred <- function(obs_data, newdf, fitcov, time_name, t, condition, covname, ...) {
  
  return(newdf[[paste0("lag1_", covname)]] + 1)
  
}