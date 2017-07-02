mlag <- function(m,  nlag = 1){
  if( is.null(dim(m)) ) {
    n = length(m)
    if(nlag > 0) {
      m[(nlag+1):n] = m[1:(n-nlag)]
      m[1:nlag] = NA
    } else if(nlag < 0) {
      m[1:(n+nlag)] = m[(1-nlag):n]
      m[(n+nlag+1):n] = NA
    }
  } else {
    n = nrow(m)
    if(nlag > 0) {
      m[(nlag+1):n,] = m[1:(n-nlag),]
      m[1:nlag,] = NA
    } else if(nlag < 0) {
      m[1:(n+nlag),] = m[(1-nlag):n,]
      m[(n+nlag+1):n,] = NA
    }
  }
  return(m);
}


make.xts <- function(x,order.by){
  tzone = Sys.getenv('TZ')
  orderBy = class(order.by)
  index = as.numeric(as.POSIXct(order.by, tz = tzone))
  if( is.null(dim(x)) ) {
    if( length(order.by) == 1 )
      x = t(as.matrix(x))
    else
      dim(x) = c(length(x), 1)
  }
  x = as.matrix(x)
  x = structure(.Data = x,
                index = structure(index, tzone = tzone, tclass = orderBy),
                class = c('xts', 'zoo'), .indexCLASS = orderBy, tclass=orderBy, .indexTZ = tzone, tzone=tzone)
  return( x )
}


ifna <- function(x,y)
{
  return(iif(is.na(x) | is.nan(x) | is.infinite(x), y, x))
}

iif <- function(cond,  truepart,  falsepart){
  if(length(cond) == 1) { if(cond) truepart else falsepart }
  else {
    if(length(falsepart) == 1) {
      temp = falsepart
      falsepart = cond
      falsepart[] = temp
    }
    if(length(truepart) == 1)
      falsepart[cond] = truepart
    else {
      cond = ifna(cond,F)
      if(is.xts(truepart))
        falsepart[cond] = coredata(truepart)[cond]
      else
        falsepart[cond] = truepart[cond]
    }
    return(falsepart);
  }
}


bt.forecast.garch.volatility <- function(ret.log, est.period = 252){
  nperiods = nrow(ret.log)
  if(is.null(nperiods)){
    nperiods <- length(ret.log)
    garch.vol = NA * ret.log
    for( i in (est.period + 1) : nperiods ) {
      temp = as.vector(ret.log[ (i - est.period + 1) : i])
      r.last =  tail( temp, 1 )
      fit = tryCatch( garch(temp, order = c(1, 1), control = garch.control(trace = F)),
                      error=function( err ) FALSE, warning=function( warn ) FALSE )
      if( !is.logical( fit ) ) {
        if( i == est.period + 1 ) garch.vol[1:est.period] = fitted(fit)[,1]
        garch.vol[i] = garch.predict.one.day(fit, r.last)
      } else {
        fit = tryCatch( garchFit(~ garch(1,1), data = temp, include.mean=FALSE, trace=F),
                        error=function( err ) FALSE, warning=function( warn ) FALSE )
        if( !is.logical( fit ) ) {
          if( i == est.period + 1 ) garch.vol[1:est.period] = sqrt(fit@h.t)
          garch.vol[i] = garchFit.predict.one.day(fit, r.last)
        }
      }
      if( i %% 100 == 0) cat(i, '\n')
    }
    garch.vol[] = ifna.prev(coredata(garch.vol))
    return(garch.vol)
  } else {
    garch.vol = NA * ret.log
    for( i in (est.period + 1) : nperiods ) {
      temp = as.vector(ret.log[ (i - est.period + 1) : i, ])
      r.last =  tail( temp, 1 )
      fit = tryCatch( garch(temp, order = c(1, 1), control = garch.control(trace = F)),
                      error=function( err ) FALSE, warning=function( warn ) FALSE )
      if( !is.logical( fit ) ) {
        if( i == est.period + 1 ) garch.vol[1:est.period] = fitted(fit)[,1]
        garch.vol[i] = garch.predict.one.day(fit, r.last)
      } else {
        fit = tryCatch( garchFit(~ garch(1,1), data = temp, include.mean=FALSE, trace=F),
                        error=function( err ) FALSE, warning=function( warn ) FALSE )
        if( !is.logical( fit ) ) {
          if( i == est.period + 1 ) garch.vol[1:est.period] = sqrt(fit@h.t)
          garch.vol[i] = garchFit.predict.one.day(fit, r.last)
        }
      }
      if( i %% 100 == 0) cat(i, '\n')
    }
    garch.vol[] = ifna.prev(coredata(garch.vol))
    return(garch.vol)
  }
}

ifna.prev <- function(y) {
  y1 = !is.na(y)
  y1[1]=T
  return( y[cummax( (1:length(y)) * y1 )]	)
}

ntop <- function(data,topn = 1,dirMaxMin = TRUE){
  temp = coredata(data)
  if(topn == ncol(data)) {
    index = is.na(temp)
    temp[index] = 0
    temp[!index] = 1
    out = data
    out[] = ifna(temp / rowSums(temp),0)
    return( out )
  }
  for( i in 1:nrow(data) ) {
    x = temp[i,]
    o = sort.list(x, na.last = TRUE, decreasing = dirMaxMin)
    index = which(!is.na(x))
    x[] = NA
    if(len(index)>0) {
      n = min(topn, len(index))
      x[o[1:n]] = 1/n
    }
    temp[i,] = x
  }
  temp[is.na(temp)] = 0
  out = data
  out[] = temp
  return( out )
}


new.constraints <- function
(
  n,
  A = NULL,
  b = NULL,
  type = c('=', '>=', '<='),
  lb = NA,
  ub = NA
)
{
  meq = 0
  if ( is.null(A) || is.na(A) || is.null(b) || is.na(b) ) {
    A = matrix(0, n, 0)
    b = c()
  } else {
    if ( is.null(dim(A)) ) dim(A) = c(length(A), 1)
    if ( type[1] == '=' ) meq = length(b)
    if ( type[1] == '<=' ) {
      A = -A
      b = -b
    }
  }
  if ( is.null(lb) || is.na(lb) ) lb = rep(NA, n)
  if ( length(lb) != n ) lb = rep(lb[1], n)
  if ( is.null(ub) || is.na(ub) ) ub = rep(NA, n)
  if ( length(ub) != n ) ub = rep(ub[1], n)
  return( list(n = n, A = A, b = b, meq = meq, lb = lb, ub = ub) )
}



add.constraints <- function
(
  A,
  b,
  type = c('=', '>=', '<='),
  constraints
)
{
  if(is.null(constraints)) constraints = new.constraints(n = nrow(A))
  if(is.null(dim(A))) A = matrix(A)
  if(length(b) == 1) b = rep(b, ncol(A))
  if ( type[1] == '=' ) {
    constraints$A = cbind( A, constraints$A )
    constraints$b = c( b, constraints$b )
    constraints$meq = constraints$meq + length(b)
  }
  if ( type[1] == '>=' ) {
    constraints$A = cbind( constraints$A, A )
    constraints$b = c( constraints$b, b )
  }
  if ( type[1] == '<=' ) {
    constraints$A = cbind( constraints$A, -A )
    constraints$b = c( constraints$b, -b )
  }
  return( constraints )
}

add.variables <- function
(
  n,
  constraints,
  lb = NA,
  ub = NA
)
{
  constraints$A = rbind( constraints$A, matrix(0, n, length(constraints$b)) )
  if ( is.null(lb) || is.na(lb) ) lb = rep(NA, n)
  if ( length(lb) != n ) lb = rep(lb[1], n)
  if ( is.null(ub) || is.na(ub) ) ub = rep(NA, n)
  if ( length(ub) != n ) ub = rep(ub[1], n)
  constraints$lb = c(constraints$lb, lb)
  constraints$ub = c(constraints$ub, ub)
  constraints$n = constraints$n + n
  return( constraints )
}



solve.QP.bounds <- function
(
  Dmat,
  dvec,
  Amat,
  bvec,
  meq=0,
  factorized=FALSE,
  binary.vec = 0,
  lb = -Inf,
  ub = +Inf
)
{
  Amat1 = Amat
  bvec1 = bvec
  n = length(dvec)
  if( length(lb) == 1 ) lb = rep(lb, n)
  if( length(ub) == 1 ) ub = rep(ub, n)
  lb = ifna(lb, -Inf)
  ub = ifna(ub, +Inf)
  index = which( ub < +Inf )
  if( length(index) > 0 ) {
    bvec = c(bvec, -ub[index])
    Amat = cbind(Amat, -diag(n)[, index])
  }
  index = which( lb > -Inf )
  if( length(index) > 0 ) {
    bvec = c(bvec, lb[index])
    Amat = cbind(Amat, diag(n)[, index])
  }
  if ( binary.vec[1] == 0 ) {
    qp.data.final = solve.QP.remove.equality.constraints(Dmat, dvec, Amat, bvec, meq)
    Dmat = qp.data.final$Dmat
    dvec = qp.data.final$dvec
    Amat = qp.data.final$Amat
    bvec = qp.data.final$bvec
    meq = qp.data.final$meq
    sol = try(solve.QP(Dmat, dvec, Amat, bvec, meq, factorized),TRUE)
    if(inherits(sol, 'try-error')) {
      ok = F
      sol = list()
    } else {
      tol = 1e-3
      ok = T
      check = sol$solution %*% Amat - bvec
      if(meq > 0) ok = ok & all(abs(check[1:meq]) <= tol)
      ok = ok & all(check[-c(1:meq)] > -tol)
    }
    if(!ok) {
      require(kernlab)
      index.constant.variables = which(!is.na(qp.data.final$solution))
      if( length(index.constant.variables) > 0 ) {
        Amat1 = Amat[,1:ncol(Amat1)]
        bvec1 = bvec[1:ncol(Amat1)]
        lb = lb[-index.constant.variables]
        ub = ub[-index.constant.variables]
      }
      sv = ipop(c = matrix(-dvec), H = Dmat, A = t(Amat1),
                b = bvec1, l = ifna(lb,-100), u = ifna(ub,100),
                r = c(rep(0,meq), rep(100, length(bvec1) - meq))
      )
      sol$solution = primal(sv)
    }
    x = qp.data.final$solution
    x[qp.data.final$var.index] = sol$solution
    sol$solution = x
  } else {
    qp_data = qp_new(binary.vec, Dmat = Dmat, dvec = dvec,
                     Amat=Amat, bvec=bvec, meq=meq, factorized=factorized)
    sol = binary_branch_bound(binary.vec, qp_data, qp_solve,
                              control = bbb_control(silent=T, branchvar='max', searchdir='best' ))
    qp_delete(qp_data)
    sol$value = sol$fmin
    sol$solution = sol$xmin
  }
  return(sol)
}


solve.QP.remove.equality.constraints <- function
(
  Dmat,
  dvec,
  Amat,
  bvec,
  meq=0
)
{
  qp.data = list()
  qp.data$Amat = Amat
  qp.data$bvec = bvec
  qp.data$Dmat = Dmat
  qp.data$dvec = dvec
  qp.data$meq = meq
  Amat1 = t(qp.data$Amat)
  bvec1 = qp.data$bvec
  Dmat1 = qp.data$Dmat
  dvec1 = qp.data$dvec
  meq1 = qp.data$meq
  qp.data$solution = rep(NA, ncol(Amat1))
  qp.data$var.index = 1:ncol(Amat1)
  while(T) {
    one.non.zero.index = which( rowSums(Amat1!=0) == 1 )
    if( length(one.non.zero.index) == 0 ) break
    temp0 = rowSums(Amat1[one.non.zero.index,])
    temp = abs( temp0 )
    bvec1[one.non.zero.index] = bvec1[one.non.zero.index] / temp
    Amat1[one.non.zero.index,] = Amat1[one.non.zero.index,] / temp
    temp0.index = matrix(1:ncol(Amat1), nr=ncol(Amat1), nc=length(one.non.zero.index))[t(Amat1[one.non.zero.index,]!=0)]
    equality.constraints = rep(NA, ncol(Amat1))
    lb = ub = rep(NA, ncol(Amat1))
    index = temp0 > 0
    temp = order(bvec1[one.non.zero.index[index]], decreasing = FALSE)
    lb[temp0.index[index][temp]] = bvec1[one.non.zero.index[index]][temp]
    index = temp0 < 0
    temp = order(-bvec1[one.non.zero.index[index]], decreasing = TRUE)
    ub[temp0.index[index][temp]] = -bvec1[one.non.zero.index[index]][temp]
    remove.index = which(lb == ub)
    if( length(remove.index) > 0 ) {
      equality.constraints[remove.index] = lb[remove.index]
      Dmat1 = Dmat1[-remove.index, -remove.index,drop=F]
      dvec1 = dvec1[-remove.index]
      bvec1 = bvec1 - Amat1[,remove.index,drop=F] %*% equality.constraints[remove.index]
      Amat1 = Amat1[,-remove.index,drop=F]
      qp.data$solution[ qp.data$var.index[remove.index] ] = lb[remove.index]
      qp.data$var.index = which(is.na(qp.data$solution))
      if( ncol(Amat1) > 0 ) {
        remove.index = which( rowSums(Amat1!=0) == 0 & bvec1 == 0 )
        if(length(remove.index)>0) {
          bvec1 = bvec1[-remove.index]
          Amat1 = Amat1[-remove.index,,drop=F]
          if( meq1 > 0 ) meq1 = meq1 - len(intersect((1:meq1), remove.index))
        }
      } else break
    } else break
  }
  qp.data$Amat = t(Amat1)
  qp.data$bvec = bvec1
  qp.data$Dmat = Dmat1
  qp.data$dvec = dvec1
  qp.data$meq = meq1
  return(qp.data)
}