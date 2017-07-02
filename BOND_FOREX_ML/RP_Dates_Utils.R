firstDayMonth=function(x)
{           
  day = format(x,format="%d")
  monthYr = format(x,format="%Y-%m")
  y = tapply(day,monthYr, min)
  first=as.Date(paste(row.names(y),y,sep="-"))
  return(first)
}

firstDayYear=function(x)
{           
  year = as.character(format(x,format="%Y"))
  first=as.Date(paste(year,"-01-01",sep=""))
  return(first)
}


dateroll_compute <- function(my_dataset, my_investment_horizon, week_trading_day =2, month_trading_day =1){ # 2 is monday by default
  my_length<-dim(my_dataset)[1]
  # we trade weeekly
  if (my_investment_horizon == 5){
    isMonday <- which(wday(my_dataset$DATE) == week_trading_day)
    for (l in 1:length(isMonday)){
      while(any(is.na(my_dataset[isMonday[l],-1])) & isMonday[l] <= my_length){
        isMonday[l] <- isMonday[l] +1
      }
    }
    # we remove the spurious dates
    isMonday <- isMonday[-which(isMonday>my_length)]
    return(unique(isMonday))
  }
  
  # we trade monthly
  if (my_investment_horizon == 21){
    isFirstDayOfMonth <- which(my_dataset$DATE %in% firstDayMonth(my_dataset$DATE))
    for (l in 1:length(isFirstDayOfMonth)){
      while(any(is.na(my_dataset[isFirstDayOfMonth[l],-1])) & isFirstDayOfMonth[l] <= my_length){
        isFirstDayOfMonth[l] <- isFirstDayOfMonth[l] +1
      }
    }
    # we remove the spurious dates
    if (sum(isFirstDayOfMonth>my_length)>0){
      isFirstDayOfMonth <- isFirstDayOfMonth[-which(isFirstDayOfMonth>my_length)]
    }
    return(unique(isFirstDayOfMonth))
  }
  
  # we trade yearly
  if (my_investment_horizon == 252){
    isFirstDayOfYear <- which(my_dataset$DATE %in% firstDayYear(my_dataset$DATE))
    for (l in 1:length(isFirstDayOfYear)){
      while(any(is.na(my_dataset[isFirstDayOfYear[l],-1])) & isFirstDayOfYear[l] <= my_length){
        isFirstDayOfYear[l] <- isFirstDayOfYear[l] +1
      }
    }
    # we remove the spurious dates
    if (sum(isFirstDayOfYear>my_length)>0){
      isFirstDayOfYear <- isFirstDayOfYear[-which(isFirstDayOfYear>my_length)]
    }
    return(unique(isFirstDayOfYear))
  }
}