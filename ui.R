#31/07/2019: tolto il tab explore --> da riga 402
#01/08/2019: commentata riga 278 

# © Copyright World Health Organization (WHO) 2016.
# This file is part of the Health Equity Assessment Toolkit (HEAT).
# HEAT is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License Version 2 as published by
# the Free Software Foundation.
# 
# HEAT is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with HEAT. If not, see http://www.gnu.org/licenses/.

#devtools::install_github('jbkunst/highcharter')


source("ui/ui_widgets_landing_page.R", local=TRUE)
#Sys.setlocale("LC_CTYPE", "russian")

# Originally this was broken into clean separate files but I found
# that loading these took longer than having them all in this one
# super-long UI
shinyUI(
  
  tagList(
    tags$script(HTML('function resetFormElement(e) {
                     e.wrap("<form>").closest("form").get(0).reset();
                     e.unwrap();
                     
                     // Prevent form submission
                     //e.stopPropagation();
                     //e.preventDefault();
                     }')),
    tags$script('
      Shiny.addCustomMessageHandler("resetFileInputHandler", function(msg) {   
        //$("#filename").parent()[0].reset();
        $("#filenametxt").val("");
        var prog = $("#filename_progress");
        prog.removeClass("complete");
        prog.css("visibility", "hidden");
        prog.find(".progress-bar").width("0%");
        $("#doUploadBtn").prop("disabled", true);
      });
      Shiny.addCustomMessageHandler("enableUploadButton", function(msg) {
        $("#filename_progress").addClass("complete");
        $("#doUploadBtn").prop("disabled", false);
      });
      Shiny.addCustomMessageHandler("toggleUploadButton", function(msg) {
        if (msg.shown === "newdata") {
          $("#doUploadBtn").prop("disabled", $("#filenametxt").val() === "");
        } else {
          $("#doUploadBtn").prop("disabled", $("#selectfolders").val() === "none");
        }
      })
    '),
    tags$head(tags$link(rel="stylesheet", type="text/css",href="spacelab.min.css")),
    tags$head(tags$link(rel="stylesheet", type="text/css",href="style.css")),
    tags$head(tags$link(rel="stylesheet", type="text/css", href =  paste0("style_", HEATversion, ".css"))), #.rdata[['extra_css']])),
    tags$head(tags$link(rel="stylesheet", type="text/css",href= 'style_hc.css')),
    # tags$head(tags$link(rel="stylesheet", type="text/css", href="font-awesome-5.3.1.css")),
    tags$head(tags$script(src = "script.js")),
    #tags$head(tags$script(src = "indonesia_subnational_boundaries01.js")),
    #tags$head(tags$script(src = "mapdata.js")),
    
    tags$head(tags$title("Health Equity Dataset")),
    
    # UNCOMMENT HERE TO RESTORE MODAL
    tags$head(
     HTML(
       '<div id="myModal" class="modal fade">
       <div class="modal-dialog">
       <div class="modal-content">
       <div class="modal-header">
       <h4 class="modal-title">Terms of use and software license agreement</h4>
       </div>
       <div class="modal-body" >
       <div id="modal-license">

       </div>
       <button  class="btn btn-primary" data-dismiss="modal">I accept</button>
       </div>
       </div>
       </div>
       </div>'
     )),
    
    # The JS below is designed to activate the modal and
    # to change the navbar if the upload version is being
    # used (remove logo and and add "Plus")
    
    tags$script(
     "$(document).ready(function() {",
     "  $('#myModal').modal('show');",
     "  $('#modal-license').load('license_agreement.html');",
     "  if ($('#backimg-upload').length) {",
     "    $('.whoimg').remove();",
     "  }",
     "});"
    ),
    
    tags$script(
      "$(document).on('shiny:connected', function(e) {",
      "$('ul.dropdown-menu').addClass('dropdown-menu-right');",
      "});"
    ),
    
    tags$script(HTML(paste0(
      collapse = "\n",
      "function isChrome() {",
      " return window.chrome !== null &&",
      "  typeof window.chrome !== 'undefined' &&",
      "  window.navigator.vendor === 'Google Inc.' &&",
      "  (typeof window.opr !== 'undefined') === false &&",
      "  (window.navigator.userAgent.indexOf('Edge') > -1) === false;",
      "}"
    ))),
    
    tags$script(HTML(paste0(
      collapse = "\n",
      "$(function() {",
      "Highcharts.wrap(Highcharts.Axis.prototype, 'getPlotLinePath', function(proceed) {",
      "  var path = proceed.apply(this, Array.prototype.slice.call(arguments, 1));",
      "  if (path) {",
      "    path.flat = false;",
      "  }",
      "  return path;",
      "});",
      "});"
    ))),
    
    if (HEATversion == "whodata") {
      tags$script(HTML("
        $(function() {
          $(`#who_heat a[data-value='explore']`).one('shown.bs.tab', function(e) {
            setTimeout(function() {
              if (!$('#disag_plot_explore_insideHC').children().length) {
                $(`#who_heat a[data-value='home']`).tab('show');
                $(`#who_heat a[data-value='explore']`).tab('show');
                
                var $disagExplore = $('#disag_plot_explore');
                var swapTabs = function() {
                  //console.log('[ SWAP TABS ]');
                  $(`a[data-value='dataplot_dtl']`).tab('show');
                  $(`a[data-value='dataplot']`).tab('show');
                  setTimeout(function() {
                    if (!$disagExplore.children('#disag_plot_explore_insideHC').children().length) {
                      swapTabs();
                    }                  
                  }, 1200);
                };
                
                swapTabs();
              }
            }, 2000);
          });
        })
      "))
    },
    
    # tags$script(HTML(paste(
    #   "$(document).on('shiny:recalculating', function(e) {",
    #   # "  console.log(e.target);",
    #   "  if (e.target.id && e.target.id === 'disage_plot_explore_dtl_insideHC') {",
    #   "    document.documentElement.classList.add('shiny-busy');",
    #   "    $(e.target).one('shiny:recalculated', function(e) { document.documentElement.classList.remove('shiny-busy') });",
    #   "  }",
    #   # "  $('#disag_plot_explore_dtl_insideHC').on('shiny:recalculating', e => console.log('hello, world!'))",
    #   "});",
    #   collapse = "\n"
    # ))),
    
    tags$script(
      HTML(
        paste(
          "$(document).ready(function() {",
          "$('.navbar-nav').append(",
          "$('<form class=\"navbar-form navbar-right\">",
          gsub(
            "\n", "",
            selectInput(
              inputId = "lang", 
              label = NULL, 
              choices = c(
                "EN" = "english"
                #"FR" = "french",
                #"PT" = "portuguese",
                #"ES" = "spanish"#,
                #"RU" = "russian"
              ),
              selectize = FALSE, 
              width = "auto"
            )
          ),
          "</form>')",
          ");",
          "});"
        )
      )
    ),
    
    navbarPage(
      
      title = tags$span(
        
        class = "navtitle",
        
        tags$a(
          
          rel = "home", 
          href = "#", 
          title = "World Health Organization",
          tags$img(class = "logo", src = "logo.png")
        ),
        
        if (HEATversion == "whodata") {
          translationOutput("navbar_whodata_title", class = "navtext")
        } else {
          translationOutput("navbar_plus_title", class = "navtext")
        }
      ),
      
      id = "who_heat", 
      inverse=TRUE, 
      collapsible = TRUE,
      
      # landing page tab ----
      tabPanel(
        title = translationOutput("navbar_button_home"),
        value = "home",
        htmlTemplate(
          filename = paste0("www/landing_page_", HEATversion, ".html"), # .rdata[['landing_page']],
          heat_plus_upload_new_db = heat_plus_upload_new_db,
          heat_plus_select_existing_db = heat_plus_select_existing_db,
          button_go_to_compare = actionButton("heat_plus_landing_button_to_compare", translationOutput("heat_plus_landing_button_to_compare"),
                                              style="color:  rgba(181, 230, 20, 0.8); background-color:  rgba(181, 230, 20, 0.8); border-color: rgba(181, 230, 20, 0.8)"),
          busyindicator = busyIndicator()
        )
      ),
      
      
      # compare tab ----
      tabPanel(
        title = translationOutput("navbar_button_compare"),
        value = "compare",
        
        # ~ control panel ----
        sidebarLayout(
          sidebarPanel(
            tags$div(style="margin-bottom:60px;"),
            translationOutput("control_panel_compare", class = "sectionhead1", parent = div),
                 # tags$div(class="sectionhead1", "Compare inequality"),
            uiOutput("focus_category_compare"), 
            uiOutput("focus_indicator_compare"),
            uiOutput("focus_dimension_compare"),
           
            # conditionalPanel(
            #   condition = "input.comparison_panel == 'inequalsum'",
            #   uiOutput("focus_source_recent_year_compare")
            # ),
            conditionalPanel(
              condition = "input.comparison_panel != 'inequalsum'",
              uiOutput("focus_source_year_compare")
            ),
           
                 #uiOutput("focus_year_compare"),
           
           conditionalPanel(
             condition = "input.comparison_panel == 'inequalsum'",
             uiOutput("focus_summeasure_compare_summary")
           ),
            
                # tags$div(class="sectionhead", "Benchmark options"),
            translationOutput("benchmark", class = "sectionhead"),
            uiOutput("benchmarkWBgroup"),
                #uiOutput("benchmarkWHOregion"),
                #uiOutput("benchmark_countries"),
            # conditionalPanel(
            #   condition = "input.comparison_panel == 'inequalsum'",
            #   uiOutput("benchmark_countries")
            # ),
            conditionalPanel(
              condition = "input.comparison_panel != 'inequalsum'",
              uiOutput("benchmarkYears")
            ),
               # uiOutput("benchmarkYears"),
            conditionalPanel(
              condition = "input.comparison_panel == 'inequaldisag'",
              #uiOutput("disag_plot_mode_compare"),
              uiOutput("ui_collapse_compare_disag_plot")
            ),
            conditionalPanel(
              condition = "input.comparison_panel == 'inequalsum'",
              #uiOutput("summary_plot_mode_compare"),
              # uiOutput("disag_plot_type_compare"),
              uiOutput("ui_collapse_compare_sum_plot")       
            )
          ), # end sidebarpanel
          
          
          mainPanel(
            tagAppendAttributes(
              class = "modal-zoom-chart", {
                zc_modal <- bsModal(
                  id = "hc_model_compare", 
                  title = "", 
                  trigger = NULL, 
                  size = "large",
                  highchartOutput("zoomhc_compare")
                )
                
                zc_modal$
                  children[[1]]$
                  children[[1]]$
                  children[[3]]$
                  children[[1]]$
                  children[[1]] <- translationOutput("close_zoom_modal2", "download_close_label")
                
                zc_modal
              }
            ),
            dataDownloadModal(
              modalId = "compdataModal_compare", 
              triggerId = "btnDownloadDisagData_compare",
              radioId = "filetype_benchmark",
              downloadId = "btnStartDownloadDisagData_compare",
              n = 7
            ),
            chartDownloadModal(
              modalId = "compplot1Modal_compare",
              triggerId = "btnDownloadDisagPlot_compare",
              selectId = "disagPlotType_compare", 
              downloadId = "btnStartDownloadDisagPlot_compare",
              n = 4
            ),
            dataDownloadModal(
              modalId = "compdataDisagModal_compare",
              triggerId = "btnDownloadDisagPlotData_compare",
              radioId = "filetype_benchmark_disag",
              downloadId = "btnStartDownloadDisagPlotData_compare",
              n = 8
            ),
            chartDownloadModal(
              modalId = "compplot2Modal_compare",
              triggerId = "btnDownloadSummaryPlot_compare",
              selectId = "summaryPlotType_compare",
              downloadId = "btnStartDownloadSummaryPlot_compare",
              n = 5
            ),
            dataDownloadModal(
              modalId = "compdataSummaryModal_compare",
              triggerId = "btnDownloadSummaryPlotData_compare",
              radioId = "filetype_benchmark_summary",
              downloadId = "btnStartDownloadSummaryPlotData_compare",
              n = 9
            ),
            tabsetPanel(
              id = "comparison_panel", 
              tabPanel(
                translationOutput(
                  id = "disaggraphs2",
                  key = "disaggraphs", 
                  parent = h6,
                  html = TRUE,
                  style = css(text_align = "center")
                ),
                # HTML("<h6 style='text-align: center;'>Disaggregated data</br>(graphs)<h6>"), 
                value = "inequaldisag", 
                busyIndicator(),
                uiOutput("btnDownloadDisagPlotData_compare"),
                htmlOutput("disag_plot_compare")
              ),
              tabPanel(
                translationOutput(
                  id = "summarygraphs2",
                  key = "summarygraphs", 
                  parent = h6, 
                  html = TRUE,
                  style = css(text_align = "center")
                ),
                # HTML("<h6 style='text-align: center;'>Summary measures</br>(graphs)<h6>"), 
                value = "inequalsum", 
                busyIndicator(),
                uiOutput("btnDownloadSummaryPlotData_compare"),
                conditionalPanel(
                  condition = "input.summary_plot_mode_compare == 'ggplot'",
                  checkboxInput(
                    inputId = "points_ccode", 
                    "Show setting codes", 
                    value = FALSE
                  )
                ),
                # uiOutput('btnDownloadSummaryPlotData_compare'),
                # uiOutput('btnDownloadSummaryPlot_compare'),
                uiOutput("summary_plot_compare")
                # div(class="container-fluid", style="overflow:visible;height:800px;", plotOutput('summary_plot_compare'))
              )
            )#endtabsetpanel
          )# end mainPanel
        )# end sidebarLayout
      ),

      #### METADATA Section
      tabPanel(
        title = "Metadata",
        value = "metadata",
        
        sidebarLayout(
          sidebarPanel(tags$div(style="margin-bottom:60px;"),
                       tags$div(class="sectionhead1", "Metadata"),
                       uiOutput("metadata_categories"),
                       uiOutput("metadata_indicators"),width=3),

          mainPanel(uiOutput("indicators_metadata"))
        )
      ),
      
      navbarMenu_drop(
        title = translationOutput("navbar_button_about", parent = span), 
        drop = if (HEATversion == "upload") -1 else -1,
        
        #tabPanel(
        #  title = translationOutput("navbar_dropdown_manual", parent = h6),
        #  value = 'gloss_panel', 
        #  includeHTML("www/manual.html")
        #),
        #        tabPanel(
        #  title = translationOutput("navbar_dropdown_notes", parent = h6),
        #  value = 'gloss_panel', 
        #  includeHTML("www/technical.html")
        #),
        #tabPanel(
        #  title = translationOutput("navbar_dropdown_compendium", parent = h6),
        #  value = 'gloss_panel',
        #  includeHTML("www/compendium.html")
        #),
        tabPanel(
          title = translationOutput("navbar_dropdown_methodology", parent = h6),
          value = 'gloss_panel', 
          includeHTML("www/methodology.html")
        ),  
        tabPanel(
          title = translationOutput("navbar_dropdown_software", parent = h6),
          value = 'gloss_panel', 
          includeHTML("www/software.html")
        ),  
        tabPanel(
          title = translationOutput("navbar_dropdown_versions", parent = h6),
          value = 'gloss_panel', 
          includeHTML("www/versions.html")
        ),
        tabPanel(
          title = translationOutput("navbar_dropdown_license", parent = h6),
          value = 'gloss_panel', 
          includeHTML("www/license.html")
        ),
        tabPanel(
          title = translationOutput("navbar_dropdown_feedback", parent = h6), 
          value = 'gloss_panel', 
          includeHTML("www/feedback.html")
        ),
        tabPanel(
          title = translationOutput("navbar_dropdown_acknowledgements", parent = h6),
          value = 'gloss_panel', 
          includeHTML("www/acknowledgement.html")
        )
      )
    ) # end navbarpage
  )  
)# End shinyUi
