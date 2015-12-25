#The functions in this file are for evaluating portfolios. They combine 
#applications from portfolio theory and other financial concepts.

YahooPricesToReturns = function(series) {
  mycols = grep('Adj.Close', colnames(series))
  closingprice = series[,mycols]
  N = nrow(closingprice)
  percentreturn = as.data.frame(closingprice[2:N,]) / as.data.frame(closingprice[1:(N-1),]) - 1
  mynames = strsplit(colnames(percentreturn), '.', fixed=TRUE)
  mynames = lapply(mynames, function(x) return(paste0(x[1], ".PctReturn")))
  colnames(percentreturn) = mynames
  as.matrix(na.omit(percentreturn))
}

SimulatePortfolio = function(assetData, portfolioWeights, initialWealth=10000, 
            nboot=500, dailyRebalance = FALSE, n.days = 20) {
  #assetData must be returns data. Returns were intended to be daily returns
  finalwealth = rep(0,nboot)
  for (i in 1:nboot) {
    wealth = initialWealth
    wealthtracker = initialWealth
    portfolio = portfolioWeights * wealth
    for (j in 1:n.days) {
      day_return = resample(assetData, 1)[1,]
      portfolio = portfolio * (1+day_return)
      wealth = sum(portfolio)
      if (dailyRebalance == TRUE) {
        portfolio = portfolioWeights * wealth
      }
      wealthtracker = c(wealthtracker, wealth)
    }
    finalwealth[i] = wealthtracker[length(wealthtracker)]
  }
  return(finalwealth)
}


MonteCarloToReturns = function(initialValue, finalPortfolioValues) {
  return((finalPortfolioValues-initialValue)/initialValue)
}

PortfolioConfidenceLevel = function(finalwealths, confidenceLevel = .05, initialValue = 10000) {
  returns = MonteCarloToReturns(initialValue, finalwealths)
  returns = sort(returns)
  n = length(returns)
  idx = as.integer(confidenceLevel * n)
  return(returns[idx])
}

portfolioMetrics = function(finalwealths, initialValue = 10000, ndays = 20) {
  #print(paste('The mean wealth after 20 days is', mean(finalwealth)))
  returnOverPeriod = (mean(finalwealth)-initialValue)/initialValue
  effective_rate = (1+returnOverPeriod)**(260/ndays) - 1
  APR = returnOverPeriod * (260/ndays)
  #print(paste('The annualized rate of return is', APR))
  print(paste('The annualized effective rate of return is', effective_rate))
  portfolioReturns = MonteCarloToReturns(initialValue, finalwealth)
  print(paste('The standard deviation of the portfolio is ', (sd(portfolioReturns)/sqrt(ndays))*sqrt(260)*100, '%, annualized'))
  print(paste('The 5% confidence level over 20 trading days is:', PortfolioConfidenceLevel(finalwealth,initialValue = initialValue, confidenceLevel = .05)))
  #quantile(MonteCarloToReturns(initialValue =10000, finalwealth), .05)
}
portfolioMetrics.ExpectedReturn = function(finalwealths, initialValue = 10000, ndays = 20) {
  returnOverPeriod = (mean(finalwealth)-initialValue)/initialValue
  return((1+returnOverPeriod)**(260/ndays) - 1)
}
portfolioMetrics.sd = function(finalwealths, initialValue = 10000, ndays = 20) {
  portfolioReturns = MonteCarloToReturns(initialValue, finalwealth)
  sd = sd(portfolioReturns)
  dailysd = sd/sqrt(ndays)
  annualizedsd = dailysd*sqrt(260)
  return(annualizedsd)
}

multipleAssetVariance = function(weights, assetData) {
  weighted.covariances = NULL
  for (i in 1:length(weights)) {
    for (j in 1:length(weights)) {
      cov = weights[i]*weights[j]* cov(assetData[,i],assetData[,j])
      weighted.covariances = c(weighted.covariances, cov)
    }
  }
  sd.portfolio = sqrt(sum(weighted.covariances))
  #return the annualized standard deviation. (260 trading days in a year)
  return(sd.portfolio*sqrt(260))
}