
plotSummaryBar_compare_hc <- function(plotData, regressionData, confInt = FALSE, ...) {
  
  axismin <- isolate(input$xaxis_limitsmin4)
  if (axismin != "") axismin <- as.numeric(axismin)
  
  axismax <- isolate(input$xaxis_limitsmax4)
  if (axismax != "") axismax <- as.numeric(axismax)
  
  cols <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#a65628")
  
  maintitle_val <-  .rdata[["plotSummary_compare_title"]]
  
  titleX <- {
    if (input$xaxis_title4 != "") {
      input$xaxis_title4
    } else {
      gsub("\\s+\\(.*\\)\\s*$", "", translate(plotData$measure[1], input$lang))
    }
  }
  titleY <- {
    if (input$yaxis_title4 != "") {
      input$yaxis_title4
    } else {
      ""
    } 
  }
  
  show_labels <- TRUE
  
  plotData <- plotData %>% 
    mutate(year = as.factor(year), color = colorize(indic_name, cols[1:length(unique(indic_name))]))
  
  plot_data <- count(plotData, indic_name) %>% 
    inner_join(plotData, ., by = "indic_name") %>%
    mutate(measure_name = map_chr(measure, translate, lang = input$lang),
           country_orig = country,
           country = paste0(country, " (", source, " ", year, ")"))
  
  seriesData <- plot_data %>% 
    group_by(indic_name, dimension) %>% 
    select(., country, country_orig, inequal, measure_name, source, year, indic_name, indic_title, dimension, color)
  
  # not all dimensions will have complete data for a confrontation, if the focus country is absent then the data must be omitted
  # dimensions = unique(seriesData$dimension)
  # 
  # for(singleDimension in dimensions) {
  #   
  #   dimensionData = seriesData %>% 
  #     filter(dimension == singleDimension)
  #   
  #   if (dimensionData$country_orig[[1]] != .rdata[['benchmark_countries']][1]) { # ELE (dimensionData$country_orig[[1]] != .rdata[["all_countries"]][1]) {
  #     seriesData = filter(seriesData, dimension != singleDimension)
  #   }
  # }
  
  if(nrow(seriesData) == 0) return()
  
  plotDataChart <- seriesData %>% do(chart = {
    
    d <- .
    
    # getting current dimension
    currentDimension = toString(unique(select(d, dimension)))
    currentRegressionData = filter(regressionData, dimension == currentDimension)
    
    # we need at least a country with two different entries to be able to calculate any regression
    regressionEntriesCount = table(currentRegressionData$country)
    
    isRegressionPossible = FALSE
    for(item in regressionEntriesCount) if(item > 1) isRegressionPossible = TRUE
    
    regressionPlus = list()
    regressionMinus = list()
    regressionSteady = list()
    
    if(isRegressionPossible) {
      
      trends = trend_bar(currentRegressionData, incbad = FALSE, 2000, "inequal", "year", "country")
      
      # adding the position for the trend markers
      trends$trend <- mutate( trends$trend, position = max(d$inequal) + 2 )
      
      countryList = list()
      countryNameList = list()
      for(item in list_parse(unique(select(d, country_orig, country)))) {
        countryList <- append(countryList, item[[1]])
        countryNameList <- append(countryNameList, item[[2]])
      }
      
      regressionList = list_parse(select(trends$trend, name = rank, trend = cat, y = position))
      
      for(item in regressionList) {
        
        x = match( item$name, countryList ) - 1
        
        if(is.na(x) || is.na(item$trend)) next
        
        item$x <- x
        item$name <- countryNameList[x + 1]
        
        if(item$trend == "+") regressionPlus[[ length(regressionPlus) + 1 ]] <- item
        if(item$trend == "=") regressionSteady[[ length(regressionSteady) + 1 ]] <- item
        if(item$trend == "-") regressionMinus[[ length(regressionMinus) + 1 ]] <- item
      }
    }
    
    catgs <- getCats(d$country)
    
    hc <- highchart() %>%
      
      hc_chart(inverted = TRUE) %>%
      
      hc_xAxis(type = "category", reversed = TRUE) %>%
      
      hc_add_series(data = list_parse(select(d, name = country, y = inequal)), 
                    type = "column", 
                    color = "#6c9dc6", #unique(d$color),
                    name = unique(d$measure_name)) 
    
    if(length(regressionPlus) > 0) {
      hc <- hc %>% hc_add_series(data = regressionPlus, 
                                 type = "scatter",
                                 color = "red",
                                 name = "Increased",
                                 tooltip = list(pointFormat = "<br>Country: <b>{point.name}</b><br>"),
                                 marker = list(symbol = "url(triangle-up.png)")) 
    }
    
    if(length(regressionSteady) > 0) {
      hc <- hc %>% hc_add_series(data = regressionSteady,
                                 type = "scatter",
                                 color = "orange",
                                 name = "No noticeable change",
                                 tooltip = list(pointFormat = "<br>Country: <b>{point.name}</b><br>"),
                                 marker = list(symbol = "circle"))
    }
    
    if(length(regressionMinus) > 0) {
      hc <- hc %>% hc_add_series(data = regressionMinus,
                                 type = "scatter",
                                 color = "green",
                                 name = "Decreased",
                                 tooltip = list(pointFormat = "<br>Country: <b>{point.name}</b><br>"),
                                 marker = list(symbol = "url(triangle-down.png)"))
    }
    
    hc
  })
  
  ncountry <- length(unique(seriesData$country))
  plotDataChart$ncountry <- ncountry
  
  getGrid(plotDataChart, title = maintitle_val, 
          minY = axismin, maxY = axismax, titleX = titleX, 
          titleY = titleY, plot_type = "plotSummaryBar_compare_hc",
          height = 750, ...)
}




