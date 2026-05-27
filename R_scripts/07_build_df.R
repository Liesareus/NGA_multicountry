


build_results_df = function(results,x){
# Builds dataframes w.r.t elasticity results of age groups or settings
# INPUT: results list (from run_model()), element to extract ("age" or "setting") 
# OUTPUT: dataframe with elasticity results for each period, age group or setting

  out = imap_dfr(results, function(period_element, period_name) {
    cum <- period_element[[x]]
    
    # turn cum_elasticity into a data.frame where columns = age groups
    mat <- if (is.matrix(cum) || is.data.frame(cum)) {
      as.data.frame(cum)
    } else {
      # assume list of numeric vectors (bootstraps) -> each row = one bootstrap
      as.data.frame(do.call(rbind, cum))
    }
    
    
    # pivot longer, compute median per age group, add period name
    mat %>%
      pivot_longer(everything(), names_to = x, values_to = "value") %>%
      group_by(!!sym(x)) %>%
      summarise(median_cum_elas = median(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(period = period_name) %>%
      select(period, !!sym(x), median_cum_elas)
  })
  
} 

build_final_df = function(inputs,results,x){
# Builds dataframes w.r.t elasticity results of age groups or settings and Rt values
# INPUT: results list (from run_model()), element to extract ("age" or "setting") 
# OUTPUT: dataframe with elasticity results for each period, age group or setting and Rt values

  aux = build_results_df(results,x)
  
  # extract rt data
  R_summary <- imap_dfr(inputs, function(period_element, period_name) {
    R_vals <- period_element$rt
    
    tibble(
      period = period_name,
      Rt = R_vals
    )
  })
    
  # join elasticity data with Rt data
  final_df <- aux %>%
    left_join(R_summary, by = "period")
  
  # calculate absolute contributions
  final_df %>%
    group_by(period) %>%
    mutate(abs_contribution = median_cum_elas*Rt)->final_df
  
  final_df$period=as.numeric(final_df$period)
  
  return(final_df)
}
