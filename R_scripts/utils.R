q_susc=function(rt,M_home,M_work,M_school,M_transport,M_leisure,M_other,h,w,weights){
  #calculates q-susceptibility given that A*M*H*w=R*w
  #INPUT: rt, contact matrix M by setting (with weights for each setting), q-infectivity h, relative incidence w
  #OUTPUT: q-susceptibility vector
  
    a=c()
    M = M_home + M_work*weights + M_school*weights + M_transport*weights + M_leisure + M_other*weights
    
    for (i in seq(nrow(M))) {
      a[i]=(rt)*(w[i]/(sum(M[i,]*h*w)))
    }
    return(a) 
}
 

NGM_SIR = function(a,M_home,M_work,M_school,M_transport,M_leisure,M_other,weights,h){
  # Builds the next generation matrix for the simple age-structured SIR model
  # INPUT: vectors age-stratified q-susceptibility (a), q-infectivity (h) , contact matrix M by setting (with weights for each setting)
  # OUTPUT: next generation matrix (see Diekmann and Britton 2013 chapter 7)
 
  M = M_home + M_work*weights + M_school*weights + M_transport*weights + M_leisure + M_other*weights
  names=colnames(M)
  n=nrow(M)
  
  if (n!=length(a) || n!=length(h)){
    stop("parameter vector size do not agree")
  }
  
  A_ = diag(a,nrow=n,ncol=n)
  H_ = diag(h,nrow=n,ncol=n)
  
  NGM = A_ %*% M %*% H_
  
  rownames(NGM)=gsub("contact_", "infected_", colnames(M))
  colnames(NGM)=gsub("contact_", "infective_", colnames(M))
  
  return(NGM)
  
}

NGM_setting = function(a,h,M_home,M_work,M_school,M_transport,M_leisure,M_other,weights){
  # Builds the transmission matrices by setting for the simple age-structured SIR model
  # INPUT: vectors age-stratified q-susceptibility (a), q-infectivity (h) , contact matrix M by setting 
  # OUTPUT: list of next generation matrices by setting 
  n=nrow(M_home) 
  
  A_ = diag(a,nrow=n,ncol=n)
  H_ = diag(h,nrow=n,ncol=n)
  
  NGM_HOME = A_ %*% M_home %*% H_
  NGM_WORK = A_ %*% (M_work*weights) %*% H_
  NGM_SCHOOL = A_ %*% (M_school*weights) %*% H_
  NGM_TRANSPORT = A_ %*% (M_transport*weights) %*% H_
  NGM_LEISURE = A_ %*% M_leisure %*% H_
  NGM_OTHER = A_ %*% (M_other*weights) %*% H_
  
  return(list(NGM_HOME=NGM_HOME,
              NGM_WORK=NGM_WORK,
              NGM_SCHOOL=NGM_SCHOOL,
              NGM_TRANSPORT=NGM_TRANSPORT,
              NGM_LEISURE=NGM_LEISURE,
              NGM_OTHER=NGM_OTHER))
}

eigen_ = function(A,norm=T){
  # Calculates eigen values and eigen vectors for both A and A^T, constrains left (v) and right (w) eigenvector
  # such that <v_i,w_i> = 1
  
  #INPUT: non-negative square matrix A
  #OUTPUT: eigenvalues of A and eigen vectors for both A and A^T
  
  # check that the matrix is primitive ( see Caswell 2005, pg 158)
  if(any(A<0)){
    stop("matrix A has negative entries")
  }
  if(any(A%^%(nrow(A)^2-2*nrow(A)+2)<=0)){      
    stop("matrix A is not primitive")
  }
  R = eigen(A)
  
  R_value = max(Re(R$values))                              # The dominant eigenvalue should be real and positive
  R_vector = Re(R$vectors[,which(Re(R$values)==R_value)])  # Its associated left eigenvector should be real and positive (Perron-Frobenius theorem)
  
  # divide each entry of the right dominant eigenvector w by the sum of the entries in order to be
  # interpreted as the stable population
  
  if(norm==T){
    R_vector=R_vector/sum(R_vector) # normalize such that ||w_1|| = 1
  }

  # calculate the left eigenvectors such that <v_i,w_i> = 1
  
  L=eigen(t(A))
  L_value=max(Re(L$values))                                      # The dominant eigenvalue should be real and positive
  L_vector=Re(L$vectors[,which(Re(L$values)==L_value)])         # Its associated left eigenvector should be real and positive (Perron-Frobenius theorem)
  
  L_vector=L_vector/as.numeric((t(L_vector)%*%R_vector))
  
  out=list(value=R_value,w=R_vector,v=L_vector,A=A)
  
  return(out)
}

sens = function(list,tol=1e-07){
   # Computes sensitivity of eigenvalues with respect to entries of matrix A (see caswell)
  
  # INPUT: list of the type "eigens" output of function eigen_ and tolerance (tol) of the approximation of w^Tv to the Identity matrix
  # OUTPUT: list of type sens with original matrix, eigenvalues and sensitivity matrix
  
  # check if <v_i,w_i> = 1
  
  lower=(t(list$w) %*% list$v)-tol
  top=(t(list$w) %*% list$v)+tol
  bol_=between(1,lower,top)
  
  
  if (bol_!=T) {
    stop("dominant eigenvectors have dot product different than 1")
  }
  
  sens=list$v %*% t(list$w) # vw^T
  colnames(sens) <- colnames(list$A)
  rownames(sens) <- rownames(list$A)
  
  return(sens)
}

elasti = function(eigens,sens){
  # calculates elasticities to entries of the matrix
  #INPUT: sens list from sens function
  #OUTPUT: elasticity matrix
  
  Rt = eigens$value
  A = eigens$A
  sens_M = sens
  
  E = (1/Rt)*A*sens_M
  
  return(E)
}

elasti_setting = function(NGM_set,sens,eigens){
  # calculates elasticity by setting
  #INPUT: contact matrices by setting, sensitivity matrix from sens function, eigens list from eigen_ function
  #OUTPUT: list of elasticities by setting
  
  Rt = eigens$value
  
  
  
  E_home = (1/Rt)*NGM_set$NGM_HOME*sens
  E_work = (1/Rt)*NGM_set$NGM_WORK*sens
  E_school = (1/Rt)*NGM_set$NGM_SCHOOL*sens
  E_transport = (1/Rt)*NGM_set$NGM_TRANSPORT*sens
  E_leisure = (1/Rt)*NGM_set$NGM_LEISURE*sens
  E_other = (1/Rt)*NGM_set$NGM_OTHER*sens
  
  # elas_setting=list(E_home=E_home,
  #                   E_work=E_work,
  #                   E_school=E_school,
  #                   E_transport=E_transport,
  #                   E_leisure=E_leisure,
  #                   E_other=E_other)
  
  elas_cum_set = c(E_home=sum(E_home),
                   E_work=sum(E_work),
                   E_school=sum(E_school),
                   E_transport=sum(E_transport),
                   E_leisure=sum(E_leisure),
                   E_other=sum(E_other))
  
  return(elas_cum_set)
}
