
#************************************************
# Update indicators! ----
#************************************************

update_global_indicator <- function(current, vals){
  
  
  #print(paste("In update global", Sys.time(), current, paste(vals, collapse = ", ")))
  
  vals <- unname(vals)
  
  if(current == "focus_indicator_explore"){
    
    .rdata[['focus_indicator']] <<- vals
    
  }
  
  if(current == "focus_indicator_explore_plotdtl"){
    
    if(!all(vals%in%.rdata[['focus_indicator']]))
      .rdata[['focus_indicator']] <<- vals
    
  }
  
  if(current == "focus_indicator_explore_map"){
    
    if(!vals%in%.rdata[['focus_indicator']])
      .rdata[['focus_indicator']] <<- vals
    
  }
  
  #browser()
  if(current == "focus_indicator_compare"){
    if(!vals%in%.rdata[['focus_indicator']])
      .rdata[['focus_indicator']] <<- vals
  }
  
  if(current == "focus_category_compare"){
    if(!vals%in%.rdata[['focus_category']]){
      .rdata[['focus_category']] <<- vals
      update_full_indicators(vals)
    }
  }
  
}

update_indicator_boxes <- function(current, from_indicator_react = TRUE){
  #if(grepl("Trini", .rdata$focus_country)) browser()
  
  isolate({
    focus_indicator_explore <- input$focus_indicator_explore
    focus_indicator_explore_plotdtl <- input$focus_indicator_explore_plotdtl
    focus_indicator_explore_map <- input$focus_indicator_explore_map
    focus_category_compare <- input$focus_category_compare
    focus_indicator_compare <- input$focus_indicator_compare
  })
  
  categ_all <- .rdata[['full_categories']]$category
  focus_categ_1 <- .rdata[['focus_category']]
  indic_all <- .rdata[['full_indicators']]
  focus_indic_all <- .rdata[['focus_indicator']]
  focus_indic_1 <- unname(c(na.exclude(.rdata[['focus_indicator']][1])))
  focus_indic_3 <- unname(c(na.exclude(.rdata[['focus_indicator']][1:3])))
  
  if(!is.null(focus_indicator_explore) &&
     current!="focus_indicator_explore" &&
     (!all(focus_indic_all%in%focus_indicator_explore) | !from_indicator_react)){

    updateSelectInput(session, 'focus_indicator_explore',
                      choices = indic_all,
                      selected = focus_indic_all)
    
  }
  

  if(!is.null(focus_indicator_explore_plotdtl) && 
     current!="focus_indicator_explore_plotdtl" && 
     !identical(focus_indicator_explore_plotdtl, focus_indic_3)){
    
    updateSelectInput(session, 'focus_indicator_explore_plotdtl',
                      choices = indic_all,
                      selected = focus_indic_3)
    
  }
  
  
  if(!is.null(focus_indicator_explore_map) &&
     current!="focus_indicator_explore_map" &&
     focus_indicator_explore_map != focus_indic_1){
    
    updateSelectInput(session, 'focus_indicator_explore_map', 
                      choices = indic_all,
                      selected = focus_indic_1)
    
  }
  
  #if(length(focus_indic_all)>1) browser()
  
  if(!is.null(focus_indicator_compare) && 
     current!="focus_indicator_compare" &&
     focus_indicator_compare != focus_indic_1){
    
    updateSelectInput(session, 'focus_indicator_compare', 
                      choices = indic_all,
                      selected = focus_indic_1)
    
  }
  
  if(!is.null(focus_category_compare) && 
     current!="focus_category_compare" ){
    
    updateSelectInput(session, 'focus_category_compare', 
                      choices = categ_all,
                      selected = focus_categ_1)
    
  }
  
  
  
}


#************************************************
# Update dimensions (stratifier) ----
#************************************************

# Updates the possible dimensions for a specified indicator code
update_global_dimensions <- function(indicator_code){
  if(is.null(indicator_code)) return()
  
  indicator_dimensions <- filter(unique(.rdata[['strata']][,.(indic, dimension)]), indic == indicator_code) %>% arrange(dimension)
  #browser()
  .rdata[['equity_dimensions']] <<- indicator_dimensions$dimension
  .rdata[['focus_dimension']] <<- indicator_dimensions$dimension[1]
}