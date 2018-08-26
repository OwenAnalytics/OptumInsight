######################################################################################
#
# THIS SHINY APP PRODUCES A LINE PLOT OF ANTI-COAGULANT PATTERN BY INDIVIDUAL PATIENTS.
# THE AC PATTERN IS DEFINED TO BE ALL AC PRESCRIPTIONS AFTER INDEX VTE DATE.
#
######################################################################################

library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(scales)
library(gridExtra)
options(warn = -1)


patinfo <- readRDS("working_data_patinfo.rds") 
inr_info <- readRDS("working_data_inr.rds")
confinfo <- readRDS("working_data_conf.rds")
enroll <- readRDS("working_data_member.rds")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  # Select the patid to be plotted
  output$select_patid <- renderUI({
    items <- levels(factor(unique(patinfo$patid)))
    names(items) <- items
    selectInput('pat', 'Select one or multiple patient id to be plotted',
                items, multiple = T, selected = items[1:3])
  })
  
  # Get the number of selected patients
  n_pat <- reactive({
    length(input$pat)
  })
  
  # CREATE A SUB DATA SET BY INTERACTIVE INPUT OF PATID
  selected <- reactive({
    selected_patid <- input$pat
    if (is.null(selected_patid)) return()
    d <- as.data.frame(patinfo %>% filter(patid %in% selected_patid) %>%
      mutate(category = factor(category),
             patid = factor(patid))  )
    d
  })
  
  # DISPLAY THE DATA FOR SELECTED PATIENTS
  output$select_patid_disp <- renderDataTable({
    selected()[,!names(selected()) %in% c("id","category_num")]
  })
  
  
  # CREATE A SUB DATA SET ON INR INFORMATION FOR SELECTED PATIENTS
  selected_inr <- reactive({
    selected_patid <- input$pat
    d <- as.data.frame(inr_info %>% filter(patid %in% selected_patid))
    if (nrow(d) == 0) return(NULL)
    d
  })
  
  # DISPLAY INR DATA FOR SELECTED PATIENTS
  output$select_inr_disp <- renderDataTable({
    selected_inr()[,names(selected_inr()) != c("inr_num", "category")]
  })
  
  # CREATE A SUB DATA SET ON CONFINEMENT INFORMATION FOR SELECTED PATIENTS
  selected_conf <- reactive({
    selected_patid <- input$pat
    d <- as.data.frame(confinfo %>% filter(patid %in% selected_patid))
    if (nrow(d) == 0) return(NULL)
    d
  })
  
  # DISPLAY CONFINEMENT DATA FOR SELECTED PATIENTS
  output$select_conf_disp <- renderDataTable({
    selected_conf()
  })
  
  # CREATE A SUB DATA SET ON INSURANCE ENROLLMENT FOR SELECTED PATIENTS
  selected_enroll <- reactive({
    selected_patid <- input$pat
    d <- as.data.frame(enroll %>% filter(patid %in% selected_patid))
    if (nrow(d) == 0) return(NULL)
    d
  })
  
  # DISPLAY INR DATA FOR SELECTED PATIENTS
  output$select_enroll_disp <- renderDataTable({
    selected_enroll()
  })
  
  # CREATE LINE PLOT OF AC PATTERN
  output$patternPlot <- renderPlotly({
    if (is.null(selected())) return()
    
    # BASIC PLOT: AC TRAJECTORIES
    p <- ggplot(selected(), aes(x=fill_dt, y=category_num)) +
      geom_vline(data = selected(), 
                 aes(xintercept = as.numeric(index_dt), linetype="Index VTE Date",
                     text=paste("Index VTE Date: ", index_dt)),
                 alpha=1, inherit.aes = FALSE) + 
      geom_vline(data = selected(), 
                 aes(xintercept = as.numeric(index_dt+90),
                     linetype="Index VTE Date + 90 Days"),
                 alpha=1, inherit.aes = FALSE) + 
      
      # geom_vline(aes(xintercept = as.numeric(index_dt), linetype="Index VTE Date",
      #                text=paste("Index VTE Date: ", index_dt)),
      #            alpha=1) + 
      # geom_vline(aes(xintercept = as.numeric(index_dt+90),
      #                linetype="Index VTE Date + 90 Days"),
      #            alpha=1) + 
      
      geom_point(data = selected(), 
                 aes(x=fill_dt, y=id, colour=factor(category)),
                 inherit.aes = FALSE) +
      # geom_point(aes(x=fill_dt, y=id, colour=factor(category))) +
      
      geom_segment(data = selected(), 
                   aes(x=fill_dt, xend=fill_sup, y=id, yend=id,
                       colour=factor(category),
                       text=paste(" Fill Date: ", fill_dt, "<br>",
                                  "Copay: ", copay, "<br>",
                                  "Days of Supply: ", days_sup, "<br>",
                                  "Index Cancer Date: ", index_cancer_dt, "<br>",
                                  "Index VTE Date: ", index_dt)), size=0.5,
                   inherit.aes = FALSE) +
      # geom_segment(aes(x=fill_dt, xend=fill_sup, y=id, yend=id,
      #                  colour=factor(category),
      #                  text=paste(" Fill Date: ", fill_dt, "<br>",
      #                             "Copay: ", copay, "<br>",
      #                             "Days of Supply: ", days_sup, "<br>",
      #                             "Index Cancer Date: ", index_cancer_dt, "<br>",
      #                             "Index VTE Date: ", index_dt)), size=0.5) +
      scale_colour_manual("", values = c("DOACS" = "#E69F00",
                                         "LMWH" = "#56B4E9",
                                         "Other" = "purple",
                                         "Warfarin" = "#009E73")) +
      scale_linetype_manual("", values=c("Index VTE Date + 90 Days"='dashed',
                                         "Index VTE Date"='solid')) +
      guides(linetype=guide_legend(keywidth = 3, keyheight = 1)) +
      scale_x_date(
        #limits=c(min(selected()$index_dt, selected_conf()$admit_dt)-20,
        # max=max(selected()$fill_sup, selected_conf()$disch_dt_combined) + 20),
                   labels=date_format("%Y-%m-%d"),
                   breaks = date_breaks("1 months")) +
      scale_y_continuous(breaks=c(0, 15,30,45),
                         labels = c("Other","Warfarin", "LMWH", "DOACS"),
                         limits=c(-5, 50)) +
      labs(x = "Fill Date", y = "Anti-coagulant Category") +
      facet_wrap(~patid, ncol=1, labeller = label_both) + 
      theme(axis.ticks.y=element_blank(), 
            axis.text.x=element_text(angle=45, hjust=1),
            plot.margin=unit(c(0.5,2,1.5,1.2),"cm"),
            panel.background = element_rect(fill = "grey92") ) +
      # shades on insurance enrollment periods
      geom_rect(data = selected_enroll(),
                aes(xmin = eligeff, ymin = -5,
                    xmax = eligend, ymax = 50),
                fill = "gray50", alpha = 0.2,
                inherit.aes = FALSE)
      
    
    # IF LAB INR IS NOT MISSING, THEN ADD POINTS TO THE BASIC PLOT
    if ( !is.null(selected_inr()) ) {
      p <- p + 
        geom_point(data = selected_inr(),
                   aes(x=inr_dt, y=inr_num, colour="INR Test",
                       text = paste(" INR Date: ", inr_dt, "<br>",
                                    "Result Number: ", rslt_nbr)),
                   shape=3, alpha = 0.8, inherit.aes = FALSE) +
        scale_colour_manual("", values = c("DOACS" = "#E69F00",
                                           "INR Test" = "red",
                                           "LMWH" = "#56B4E9",
                                           "Other" = "purple",
                                           "Warfarin" = "#009E73")) 
    }
    
    # IF CONFINEMENT IS NOT MISSING, THEN ADD LINES TO THE BASIC PLOT
    if ( !is.null(selected_conf()) ) {
      p <- p +
        geom_segment(data = selected_conf(),
                   aes(x = admit_dt, xend = admit_dt, y=-5, yend = 50,
                       text=paste(" Admit Date", id, ": ", admit_dt),
                   linetype = 'Admit Date'), color = 'palevioletred1',
                   inherit.aes = FALSE) +
        geom_text(data=selected_conf(),
                  aes(x=admit_dt, y=48,  label = paste("Admit Date", id, ": ", admit_dt)),
                  size = 2.5,
                  inherit.aes = FALSE) +
        geom_segment(data = selected_conf(),
                     aes(x = disch_dt_combined, xend = disch_dt_combined,
                         y=-5, yend = 50,
                         text=paste("Discharge Date", id, ":", disch_dt_combined),
                     linetype = "Discharge Date"), color = 'palevioletred4',
                     inherit.aes = FALSE) +
        geom_rect(data = selected_conf(),
                  aes(xmin = admit_dt, ymin = -5,
                      xmax = disch_dt_combined, ymax = 50),
                  fill = 'palevioletred3', alpha=0.2,
                  inherit.aes = FALSE) +
        geom_text(data=selected_conf(),
                  aes(x=disch_dt_combined, y=-3, 
                      label = paste("Discharge Date", id, ": ", disch_dt_combined)),
                  size = 2.5,
                  inherit.aes = FALSE) +
        scale_linetype_manual("", values=c("Index VTE Date + 90 Days"='dashed',
                                           "Index VTE Date"='solid',
                                           "Admit Date" = "dotted",
                                           "Discharge Date"="dotted"),
                              guide = 'legend') 
    }
             
    
    suppressWarnings(ggplotly(p,tooltip = c("text")) %>%
                       layout(dragmode = "select", autosize = FALSE,
                              height = n_pat()*400, width=1000)) 
  })

})
