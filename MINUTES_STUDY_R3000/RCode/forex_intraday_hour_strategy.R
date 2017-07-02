#### Forex intraday hour strategy

getSymbols_fxhistoricaldata <- function
(
  Symbols,
  type = spl('hour,day'),
  env = .GlobalEnv,
  auto.assign = TRUE,
  download = FALSE,
  name.has.type = TRUE
)
{
  type = type[1]
  type0 = paste0(type,'_')
  temp.folder = paste(getwd(), 'temp', sep='/')
  dir.create(temp.folder, F)
  for (i in 1:len(Symbols)) {
    if(download) {
      url="//api.fxhistoricaldata.com/v1/indicators?instruments=EURCHF&amp;expression=open,high,low,close&amp;item_count=100000&amp;format=csv&amp;timeframe=hour"
      url = paste('http://www.fxhistoricaldata.com/instruments/', Symbols[i], '?t=', type, sep='')
      filename = paste(temp.folder, '/', Symbols[i], '_', type, '.zip', sep='')
      download.file(url, filename,  mode = 'wb')
      unzip(filename, exdir=temp.folder)
    }
    filename = paste(temp.folder, '/', Symbols[i], '_', type, '.csv', sep='')
    temp = read.delim(filename, header=TRUE, sep=',')
    colnames(temp) = gsub('[X\\.|\\.]', '', colnames(temp))
    out = make.xts(temp[,spl('OPEN,LOW,HIGH,CLOSE')],
                   strptime(paste(temp$DATE, temp$TIME), format='%Y%m%d %H:%M:%S'))
    cat(i, 'out of', len(Symbols), 'Reading', Symbols[i], '\n', sep='\t')
    if (auto.assign) {
      assign(paste0(gsub('\\^', '', Symbols[i]), iif(name.has.type,type0,'')), out, env)
    }
  }
  if (!auto.assign) {
    return(out)
  } else {
    return(env)
  }
}



###############################################################################
# Load Systematic Investor Toolbox (SIT)
# https://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod')   

EURUSD = getSymbols_fxhistoricaldata('EURUSD', 'hour', auto.assign = F, download=T)
SPY = getSymbols('SPY', src = 'yahoo', from = '1980-01-01', auto.assign = F)

#*****************************************************************
# Reference intraday period
#****************************************************************** 
plota(EURUSD['2012:03:06 10::2012:03:06 21'], type='candle', main='EURUSD on 2012:03:06 from 10 to 21')