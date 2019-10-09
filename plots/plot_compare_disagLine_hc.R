
plotDisagLine_compare_hc <- function(plotData, ...) {
  #browser()  
  
  #if(input$main_title3 == "Health Equity Summary") return()
  maintitle_val <- .rdata[['plotDisag_compare_title']]
  
  plotData <- arrange(plotData, dimension, order)
  
  isolate({
    axismin <- isolate(input$axis_limitsmin3)
    axismax <- isolate(input$axis_limitsmax3)
    
    titleX <- ifelse(!is.null(isolate(input$xaxis_title3)) && 
                       isolate(input$xaxis_title3) !="", isolate(input$xaxis_title3), "")
    titleY <- ifelse(!is.null(isolate(input$yaxis_title3)) && 
                       isolate(input$yaxis_title3) !="", isolate(input$yaxis_title3), "")
  })
  
  ncountry <- length(unique(plotData$country))
  
  # excluded_countries = .rdata[['all_countries']][!.rdata[['all_countries']]%in%unique(plotData$country)]
  # cat("\nplot_compare_disagLine_hc - plotData$country: ", ncountry, " - excluded:", excluded_countries, "\n")
  
  lang <- isolate(input$lang)
  
  # @git1181
  if (HEATversion == "whodata") {
    plotData <- mutate(
      plotData,
      country = map_chr(country, translate, lang = lang),
      dimension = map_chr(dimension, translate, lang = lang),
      subgroup = map_chr(as.character(subgroup), translate, lang = lang),
      indic_name = map_chr(indic, translate, lang = lang),
      indic_title = indic_name
    )
  }
  
  plotDataChart <- plotData %>% 
    mutate(
      country_orig = country,
      country = paste0(country, " (", source, " ", year, ")")
    ) %>% 
    # dplyr::arrange(desc(anchor), country) %>%
    dplyr::arrange(country) %>%
    ungroup() %>% 
    mutate(
      country = factor(country, levels = unique(country))
    ) %>% 
    
    # group_by(indic_name, indic_title, indic) %>% 
    group_by(indic_name, dimension, indic_title) %>% 
    
    do(chart = {
      d <- .
      
      hc <- highchart() %>% 
        hc_xAxis(type = "category", reversed = TRUE) %>% 
        hc_chart(type = "bar") %>% 
        # rotate tooltip # 19
        hc_tooltip(
          headerFormat = '',
          pointFormatter = JS(str_glue(
            "function() {{",
            "let _this = Object.assign({{}}, this);",
            "Object.keys(_this).forEach(function(key) {{ if (typeof _this[key] === 'number' && key != 'year') _this[key] = _this[key].toFixed(1) }});",
            "return _this.country_orig + ', ' + _this.source + ' ' + _this.year +",
            "  '<br/><br/>' +",
            "  '<b>' + _this.subgroup + '</b>' + (_this.popshare ? ' (' + _this.popshare + '% { translate('tooltip_affected_pop', lang) })' : '') +",
            "  '<br/><br/>' +",
            "  '<b>{ translate('tooltip_estimate', lang) }: ' + _this.y + '</b>' + (_this.upper_95ci ? '; 95% CI: ' + _this.lower_95ci + '-' + _this.upper_95ci : '') +",
            "  ",
            "  (_this.national ? '<br/><br/>{ translate('tooltip_setting_avg', lang) }: ' + _this.national : '');",
            "}}"
          ))
        )
      
      cnt <- length(unique(d$subgroup))
      
      if ((.rdata[['HEATversion']] == "whodata" && d$dimension[1] == translate("Subnational region", lang)) ||
          (.rdata[['HEATversion']] == "upload" && cnt > 7 )) {
        
        d2 <- mutate(d, name = country, y = estimate, color = colors) 
        
        hc <- hc %>% 
          hc_add_series(
            data = d2 %>% 
              select(
                name, y, national, country_orig, indic_title,
                source, year, indic_name, dimension, subgroup, 
                popshare, lower_95ci, upper_95ci
              ) %>% 
              list_parse(),
            name = d2$dimension[1],
            type = "scatter", 
            color = hex_to_rgba(unique(d2$color), alpha = 0.5)
          )
        
      } else {
        
        for (sg in unique(d$subgroup)) {
          d2 <- d %>%
            filter(subgroup == sg) %>%
            mutate(name = country, y = estimate, color = colors) 
          
          hc <- hc %>% hc_add_series(
            data = d2 %>% select(
               name, y, country_orig, national, indic_title,
              source, year, indic_name, dimension, subgroup, 
              popshare, lower_95ci, upper_95ci
            ) %>% list_parse(), 
            name = sg,
            type = "scatter", 
            color = unique(d2$color)
          )
        }
        
      }
      
      d3 <- d %>% 
        group_by(name = country) %>% 
        summarize(low = min(estimate, na.rm=TRUE), high = max(estimate, na.rm=TRUE))
      
      hc <- hc %>% 
        hc_add_series(
          data = NULL, 
          color = "transparent", 
          type = "line", 
          showInLegend = FALSE
        ) %>%
        hc_add_series(
          data = list_parse(d3), type = "errorbar", zIndex = -10, name = "range",
          showInLegend = FALSE, enableMouseTracking = FALSE, linkedTo = NULL,
          stemWidth = 1, whiskerLength = 1, color = "#606060"
        ) %>% 
        hc_plotOptions(
          scatter = list(
            marker = list(
              radius = 6
            )
          )
        )
      
      hc
    }) # end `do()` chart  
  
  # plotDataChart <- mutate(plotDataChart, dimension = "", dimension2 = unique(plotData$dimension))
  # plotDataChart <- mutate(plotDataChart, dimension = unique(plotData$dimension))
  
  plotDataChart$ncountry <- ncountry
  
  plotDataChart <- maxIndPly(plotDataChart, mutate(plotData, value = estimate))
  
  if (HEATversion == "whodata") {
    lang <- isolate(input$lang)
    
    plotDataChart <- mutate(
      plotDataChart,
      indic_name = map_chr(indic, translate, lang, html = FALSE),
      dimension2 = map_chr(dimension2, translate, lang,  html = FALSE)
    )
  }
  
  getGrid(
    plotDataChart, 
    title = maintitle_val, 
    indic_title = FALSE, 
    minY = ifelse(!is.null(axismin) && axismin != "", as.numeric(axismin), min(plotDataChart$value) - 1),
    maxY = ifelse(!is.null(axismax) && axismax != "", as.numeric(axismax), max(plotDataChart$value) + 1), 
    titleX = titleX, 
    titleY = titleY,
    plot_type = "plotDisagLine_compare_hc", 
    height_px = 750,
    ...
  )
}



