#install_github("ironholds/pageviews")

plotViewsAndVol <- function(pageName,tickerSymbol)
{
  library(devtools)
  library(pageviews)
  library(ggplot2)
  library(quantmod)
  library(dplyr)
  library(grid)
  
  viewsInfo <- article_pageviews(article = pageName,
                                 start="2007070100",
                                 end="2024012600",
                                 user_type="user")
  #print(head(viewsInfo))
  
  viewsInfo <- subset(viewsInfo, select=c(article, date, views)) #only the columns that we require
  
  #print(head(viewsInfo))
  
  #viewsData <- viewsData[order(viewsData$views, decreasing=TRUE),]
  #viewsData <- viewsData[order(viewsData$date, decreasing=FALSE),]
  
  #pageName
  #print(pageName)
  
  companyViews <- subset(viewsInfo,article == pageName)
  
  print(head(companyViews))
  
  company <- getSymbols(tickerSymbol, auto.assign=FALSE)
  companyVol <- Vo(company)
  
  #str(companyVol)
  companyVol <- fortify(companyVol)
  
  #print(head(companyVol))
  
  companyVol <- rename(companyVol, date=Index)
  old_name <- paste0(tickerSymbol,".Volume")
  colnames(companyVol)[colnames(companyVol)==old_name] <- "Volume"
  
  print(head(companyVol))
  
  companyMerge <- inner_join(companyVol, companyViews, by = "date")

  print(head(companyMerge))
  
  companyMerge <- subset(companyMerge, select=c(date, article, views,Volume))
  
  #plotting variables
  viewsPlot <- ggplot(data=companyMerge,
                      aes(x=date,
                          y=views)) + 
    geom_line() + labs(title = "Number of Page Visits")
  
  print(viewsPlot)
  
  tradingPlot <- ggplot(data=companyMerge,
                        aes(x=date,
                            y=Volume)) + 
    geom_line() + labs(title = "Trade volume")
  
  print(tradingPlot)
  
  #library(grid)
  grid.newpage()
  grid.draw(rbind(ggplotGrob(viewsPlot),
                  ggplotGrob(tradingPlot)))
  
  viewsPlot <- ggplot(data=companyMerge,
                      aes(x=date,
                          y=views)) + 
    geom_line() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank()) + labs(title = "Number of page visits")
  
  grid.newpage()
  grid.draw(rbind(ggplotGrob(viewsPlot), 
                  ggplotGrob(tradingPlot)))
  
}