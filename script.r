source('./r_files/flatten_HTML.r')

############### Library Declarations ###############
libraryRequireInstall("plotly");
libraryRequireInstall("ggplot2");
libraryRequireInstall("RColorBrewer");
libraryRequireInstall("dplyr");
libraryRequireInstall("tidyr");
libraryRequireInstall("glue");
libraryRequireInstall("lubridate");
####################################################

################### Actual code ####################
dataset <- Values %>% 
    mutate(AdmissionDate = as.Date(AdmissionDate),
           DischargeDate = as.Date(DischargeDate)) %>%
    mutate(LoS = difftime(DischargeDate, AdmissionDate, units="days") %>% as.numeric())
  

  if (length(unique(dataset$ClientID)) > 1) {

    # fig <- plotly::plotly_empty(type = "scatter", mode = "markers") %>%
    # plotly::config(
    #   displayModeBar = FALSE
    # ) %>%
    # plotly::layout(
    #   title = list(
    #     text = title,
    #     yref = "Please Select a Single Client",
    #     y = 0.5
    #   )
    # )

    fig <- plot_ly(type='scatter') %>%
      plotly::layout(
        title = list(
          text = title,
          yref = "Please Select a Single Client",
          y = 0.5
        )
      )

  } else {

  label_seq <- rep(seq(1, 6, 1), times= ceiling((dataset %>% nrow()) / 4))
  
  label_seq <- label_seq[1:(dataset %>% nrow())]
  
  ward_palette <- dataset %>%
    distinct(Ward)
  
  my_pal <- colorRampPalette(brewer.pal(n = nrow(ward_palette), "Set3"))
  
  # Now we add a colour to each ward
  ward_palette <- ward_palette %>% 
    arrange(Ward) %>% 
    mutate(RowNum = row_number()) %>% 
    left_join(my_pal(nrow(ward_palette)) %>% as_tibble() %>% mutate(RowNum = row_number()), by="RowNum") %>%
    rename(FillColour = value)
  
  client_stay_data_final <- dataset %>%  
    # Join to the ward colour palette
    left_join(ward_palette, by="Ward")  %>%
    # We want to alternate the side of the labels to minimize overlap
    # Add row numbers for these stays
    # Then if even, put label above, and if odd, put label below
    arrange(AdmissionDate) %>% 
    # mutate(StayNo = row_number()) %>%
    # Switch to rowwise calculations, otherwise we will get 
    # the same random number each time
    mutate(StayNo = label_seq) %>% 
    rowwise() %>% 
    mutate(LabelPos = case_when(
      # # If even in stay sequence, place label below
      # StayNo %% 2 == 0 ~ runif(1, 40, 100),
      # # If odd in stay sequence, place label above
      # TRUE ~ runif(1, 20, 100) * -1)
      
      StayNo == 1 ~ -30,
      StayNo == 2 ~ 80,
      StayNo == 3 ~ -80,
      StayNo == 4 ~ 30,
      StayNo == 5 ~ -120,
      StayNo == 6 ~ 120,
      
    ))  %>%
    # Switch back to normal calculation (non-rowwise)
    ungroup() %>% 
    # Add in a label for the length of stay
    # When we don't haave an end date, calculate it up to today
    mutate(LoSToDate = case_when(
      DischargeDate < lubridate::today() ~ glue::glue("{LoS} days"),
      TRUE ~ glue::glue("{difftime(tidyr::replace_na(DischargeDate, Sys.Date()), AdmissionDate, units='days') %>% as.numeric()} days (ONGOING)"))
    ) %>% 
    # Finalise the label to show both ward and LoS (plus indication of whether stay is still in progress)
    # mutate(WardLabelWithLoS = stringr::str_wrap(glue::glue("{Ward}: {LoSToDate}"), 8)) %>%
    mutate(WardLabelWithLoS = glue::glue("<b>{Ward}</b> {LoSToDate}") %>% 
             stringr::str_replace_all(" - ", " ") %>%
             stringr::str_replace_all(" ", "\n")
    ) %>%
    # To make current stay display correctly we need to replace the NA end date with the current date
    mutate(DischargeDate = tidyr::replace_na(DischargeDate, Sys.Date())) %>%
    # We want to place the labels in the middle of each stay - work out 
    # the midpoint between admission and discharge
    # Sate midpoint code modified from https://stat.ethz.ch/pipermail/r-help/2013-November/363276.html
    mutate(IntermediatePoint = AdmissionDate + floor((DischargeDate-AdmissionDate)/2)) #%>% 
    # left_join(most_recent_primary_diagnosis %>% select(ClientID, DiagnosisCode, Diagnosis),
    #           by="ClientID") %>% 
    # mutate(DiagnosisFull = glue::glue("{Diagnosis} ({DiagnosisCode})"))
  
  
  # Reshape the client df to a long format for plotting
  plot_df <- client_stay_data_final %>% 
    dplyr::select(ClientID, Ward, Specialty, AdmissionDate, DischargeDate) %>% 
    tidyr::gather(key="name", value = "value", AdmissionDate, DischargeDate)
  




  # Generate the base plotly figure
  # This just puts invisible points but they are important for setting up the axes
  # and giving the hover text points to anchor to
  fig <- plot_df %>%
    plot_ly(x=~value, 
            y=1, 
            alpha=0,
            text = ~paste0('</br>Ward: ', Ward, ' (', Specialty, ')',
                           # If discharge date is today we know that means they're actually still in
                           # so just display 'ongoing'
                           # Whereas if it's any other date then it must be a concluded stay
                           '</br>', name,  ': ', case_when(value >= lubridate::today() ~ "Ongoing", 
                                                           TRUE ~ as.character(value))),
            hoverinfo = 'text',
            type='scatter',
            mode="lines",
            width = NULL, 
            height = NULL
    )
  
  # Add shapes to the layout
  # These will be rectangles showing stays
  
  i <- 1
  shapes <- list()
  
  # Iterate through and generate one rectangle per stay
  while (i < nrow(client_stay_data_final) + 1) {
    # print(i)
    
    shapes[[i]] <- list(type = "rect",
                        fillcolor = client_stay_data_final[i,]$FillColour,  
                        opacity = 0.8,
                        x0 = client_stay_data_final[i,]$AdmissionDate, 
                        x1 = client_stay_data_final[i,]$DischargeDate, 
                        xref = "x",
                        y0 = 0, 
                        y1 = 1, 
                        yref = "y"
    )
    
    
    
    i <- i+1
    
  }
  
  # Add labels showing ward name and LoS in that ward
  # These will always be visible
  fig <- fig %>% 
    add_annotations(
      x = client_stay_data_final$IntermediatePoint,
      y = 1,
      text = client_stay_data_final$WardLabelWithLoS,
      xref = "x",
      yref = "y",
      showarrow = TRUE,
      arrowhead = 4,
      arrowsize = .5,
      # This controls the horizontal offset of the arrow
      ax = 0,
      # This controls the vertical distance from the top of the bar
      # to the axis
      ay = client_stay_data_final$LabelPos,
      bgcolor="#ffffff",
      opacity=0.8,
      font = list(size=10)
    )
  
  # Add finishing touches
  fig <- layout(fig, 
                shapes = shapes,
                hovermode='x unified',
                # title=glue::glue(),
                title = list(
                  text = paste0(
                    glue::glue('Client: {unique(dataset$ClientID)}')#,
                    #'<br>',
                    #'<sup>',
                    #glue::glue('Most Recent Primary Diagnosis: {client_stay_data_final %>% distinct(DiagnosisFull) %>% pull()}'),
                    #'</sup>')
                  )
                ),
                
                
                xaxis = list(
                  # Set default to just display last 5 years
                  autorange = FALSE,
                  range = c(as.character(lubridate::today() - lubridate::years(5)), 
                            as.character(lubridate::today() + lubridate::days(31))
                  ),
                  title=FALSE,
                  # Add several buttons that will jump to plot to predefined
                  # time periods
                  rangeselector = list(
                    buttons = list(
                      # list(
                      #   count = 3,
                      #   label = "3 mo",
                      #   step = "month",
                      #   stepmode = "backward"),
                      list(
                        count = 6,
                        label = "6 mo",
                        step = "month",
                        stepmode = "backward"),
                      list(
                        count = 1,
                        label = "1 yr",
                        step = "year",
                        stepmode = "backward"),
                      list(
                        count = 2,
                        label = "2 yr",
                        step = "year",
                        stepmode = "backward"),
                      list(
                        count = 3,
                        label = "3 yr",
                        step = "year",
                        stepmode = "backward"),
                      list(
                        count = 5,
                        label = "5 yr",
                        step = "year",
                        stepmode = "backward"),
                      list(
                        count = 10,
                        label = "10 yr",
                        step = "year",
                        stepmode = "backward"),
                      # list(
                      #   count = 1,
                      #   label = "YTD",
                      #   step = "year",
                      #   stepmode = "todate"),
                      list(step = "all"))),
                  
                  rangeslider = list(type = "date")),
                
                yaxis = list(
                  # Set default to just display last 2 years
                  autorange = FALSE,
                  range = c(0, 1.8),
                  # Hide axis labels as they have no meaning for the graph
                  showticklabels = FALSE
                )
                
  )

  }

  p <- fig
####################################################

############# Create and save widget ###############
internalSaveWidget(p, 'out.html');
####################################################

################ Reduce paddings ###################
ReadFullFileReplaceString('out.html', 'out.html', ',"padding":[0-9]*,', ',"padding":0,')
####################################################
