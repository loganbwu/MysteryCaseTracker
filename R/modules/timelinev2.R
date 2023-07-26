# Displays a non-interactive timeline

timelineUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Infectious periods"),
    plotOutput(ns("timeline"))
  )
}

timelineServer <- function(id, linelist.df, state) {
  # df: A single-row data frame
  moduleServer(
    id,
    function(input, output, session) {
      output$timeline = renderPlot({
        # Create ggplot timeline. This is not a ggplotly because I couldn't get it to work
        df = linelist.df() %>%
          filter(Classification == 'Confirmed') %>% 
          arrange(desc(CalculatedOnsetDate)) %>%
          mutate(PHESSID = as.factor(PHESSID) %>% fct_inorder,
                 # set Sx date as calculated date if missing
                 is.ASx = is.na(SymptomsOnsetDate),
                 SymptomsOnsetDate = coalesce(SymptomsOnsetDate, CalculatedOnsetDate),
                 # Define start and end points for each time period
                 # Symptomatic: [0, 14]
                 # Presymptomatic: [-2, 0]
                 # Acquisition: [-14, -2] # note: original script says [-14, 2]
                 Symptomatic = SymptomsOnsetDate,
                 Presymptomatic = SymptomsOnsetDate - days(2),
                 Acquisition = SymptomsOnsetDate - days(14)) %>%
          # Reformat to get 3 colums: Timeframe (Acquisition, presymptomatic, symptomatic)
          pivot_longer(cols=ends_with("start"), names_to="Period", values_to="Date_start") %>%
          mutate(Date_end = case_when(
            Period == "Symptomatic" ~ Date_start + days(14),
            Period == "Presymptomatic" ~ Date_start + days(2),
            Period == "Acquisition" ~ Date_start + days(12)
          )) %>%
          drop_na(Date_start, Date_end)
        
        TimelineData$Timeframe[which(TimelineData$Timeframe == "PreSymptomatic")] <- "Infectious period - pre-symptomatic"
        TimelineData$Timeframe[which(TimelineData$Timeframe == "Symptomatic")] <- "Infectious period - symptomatic"
        TimelineData$PointLabel <- "Diagnosis date"
        
        TimelineData$Timeframe <- ifelse(TimelineData$AsympFlag == 1,
                                         paste0(TimelineData$Timeframe, " (Asymptomatic)"),
                                         TimelineData$Timeframe)
        
        
        acquisitionCol    <- "antiquewhite2"
        presymptomaticCol <- "darkgrey"
        symptomaticCol    <- "lightblue"
        
        acquisitionColAsymp    <- "beige"
        presymptomaticColAsymp <- "gainsboro"
        symptomaticColAsymp    <- "lightcyan"
        
        TimelineData_min <- TimelineData %>%
          ungroup() %>%
          filter(diagnosis_date == min(diagnosis_date)) 
        TimelineData_min <- TimelineData_min[1,]
        TimelineData_min$LineType <- "First diagnosis date"
        
        daysBetween <- difftime(max(TimelineData$end), min(TimelineData$start), unit = "days") %>% as.numeric
        x <- ceiling(daysBetween/35)
        Plot_date_break <- paste0(x, " days")
        
        # TimelineData <- TimelineData %>% group_by(PHESSID) %>% mutate(Order = as.numeric(PHESSID), ymin = Order-0.4, ymax = Order+0.4)
        TimelineData <- TimelineData %>% group_by(PHESSID) %>% mutate(Order = as.numeric(PHESSID), ymin = Order-0.3, ymax = Order+0.3)
        Ylimits = c(0, max(TimelineData$Order)+1)
        timelineplot <- ggplot(TimelineData, aes(x=start, y = Order, col = Timeframe)) +
          # labs(color = "",
          #      fill = "",
          #      x = 'Date',
          #      y = 'Confirmed cases')+
          labs(x = 'Date',
               y = 'Confirmed cases') +
          theme_bw() + #use ggplot theme with black gridlines and white background
          geom_rect(aes(xmin = start,
                        xmax= end,
                        ymin = ymin,
                        ymax = ymax,
                        fill = Timeframe,
                        color = Timeframe),
                    color = "black",
                    lty = 1) +
          scale_fill_manual(values = c("Acquisition" = acquisitionCol,
                                       "Infectious period - pre-symptomatic" = presymptomaticCol,
                                       "Infectious period - symptomatic" = symptomaticCol,
                                       "Acquisition (Asymptomatic)" = acquisitionColAsymp,
                                       "Infectious period - pre-symptomatic (Asymptomatic)" = presymptomaticColAsymp,
                                       "Infectious period - symptomatic (Asymptomatic)" = symptomaticColAsymp)) +
          
          geom_vline(data = TimelineData_min, aes(xintercept = diagnosis_date, colour = LineType), lty = 2, lwd = 0.75) +
          geom_point(data = TimelineData, aes(x = diagnosis_date, y = Order, colour = PointLabel)) +
          scale_colour_manual(name = "stats", values = c("Diagnosis date" = "black", "First diagnosis date" = "red")) +
          
          guides(fill=guide_legend(nrow=3, byrow = TRUE,
                                   order = 1) ,
                 colour = guide_legend(override.aes = list(linetype = c("Diagnosis date" = 0,"First diagnosis date" = 2),
                                                           shape = c("Diagnosis date" = 16, "First diagnosis date" = NA)),
                                       nrow = 2,
                                       order = 2)) +
          
          scale_x_date(date_breaks = Plot_date_break,
                       date_labels =  "%d-%m") +
          
          theme(axis.text.y = element_blank(),
                axis.title = element_text(size = 14),
                axis.text.x = element_text(angle = 90, vjust = 0.25, size = 14),
                legend.title = element_blank(),
                legend.position = "bottom",
                legend.text = element_text(size = 14),
                legend.box = "horizontal",
                panel.grid.minor = element_blank()) +
          scale_y_continuous(limits = Ylimits,
                             breaks = seq(min(Ylimits), max(Ylimits)),
                             expand = c(0,0))
        g
      }, bg="transparent")
    }
  )
}