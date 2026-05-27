# run the NGA analysis

run_period_analysis <- function(input_list,n_sample){
q_susc_aux = list()
ngm = list()
eigen_aux = list()
sens_aux = list()
cum_elasti = list()
elas_aux = list()
ngm_set_aux = list()
elas_set=list()

for (i in seq(1,n_sample)) {
  q_susc_aux[[i]] = q_susc(rt=input_list$rt,
                           M_home=input_list$contacts$matrix_sample_home[[i]],
                           M_work=input_list$contacts$matrix_sample_work[[i]],
                           M_school=input_list$contacts$matrix_sample_school[[i]],
                           M_transport=input_list$contacts$matrix_sample_transport[[i]],
                           M_leisure=input_list$contacts$matrix_sample_leisure[[i]],
                           M_other=input_list$contacts$matrix_sample_other[[i]],
                           h=config$study$infectivity,
                           w=input_list$incidence$rel_incidence,
                           weights = input_list$weights)
  
  ngm[[i]] = NGM_SIR(a = q_susc_aux[[i]],
                     M_home=input_list$contacts$matrix_sample_home[[i]],
                     M_work=input_list$contacts$matrix_sample_work[[i]],
                     M_school=input_list$contacts$matrix_sample_school[[i]],
                     M_transport=input_list$contacts$matrix_sample_transport[[i]],
                     M_leisure=input_list$contacts$matrix_sample_leisure[[i]],
                     M_other=input_list$contacts$matrix_sample_other[[i]],
                     weights = input_list$weights,
                     h=config$study$infectivity)
  
  
  eigen_aux[[i]] = eigen_(A = ngm[[i]], 
                          norm = T)
  
  
  sens_aux[[i]] = sens(list = eigen_aux[[i]],
                       tol=1e-07)
  
  
  cum_elasti[[i]] = diag(sens_aux[[i]])
  
  
  elas_aux[[i]] = elasti(eigens=eigen_aux[[i]],
                         sens = sens_aux[[i]])
  
  ngm_set_aux[[i]] = NGM_setting(a = q_susc_aux[[i]],
                                 M_home=input_list$contacts$matrix_sample_home[[i]],
                                 M_work=input_list$contacts$matrix_sample_work[[i]],
                                 M_school=input_list$contacts$matrix_sample_school[[i]],
                                 M_transport=input_list$contacts$matrix_sample_transport[[i]],
                                 M_leisure=input_list$contacts$matrix_sample_leisure[[i]],
                                 M_other=input_list$contacts$matrix_sample_other[[i]],
                                 h=config$study$infectivity,
                                 weights = input_list$weights)
  
  elas_set[[i]] = elasti_setting(NGM_set = ngm_set_aux[[i]],
                                 sens = sens_aux[[i]],
                                 eigens = eigen_aux[[i]])
  
}


return(list(ngm=ngm,
            q_susc=q_susc_aux,
            eigen=eigen_aux,
            sens = sens_aux,
            cum_elasti=cum_elasti,
            elas = elas_aux,
            ngm_set = ngm_set_aux,
            elas_set = elas_set))
}

run_model <- function(inputs,n_sample){
  map(inputs, ~run_period_analysis(.x,n_sample))
}


