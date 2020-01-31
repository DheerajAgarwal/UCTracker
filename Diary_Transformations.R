# imports ----
library(gsheet)
library(data.table)
library(ggplot2)
library(plotly)

library(dplyr)
library(purrr)
library(gganimate)

# constants ----

gsheet_url <- "https://docs.google.com/spreadsheets/d/1Jn9EcHvzmAI-1uOTR_rSHY43Di5iNj5hGCH3AGxamTw/edit?usp=sharing"
theme_set(theme_classic())


# Get data ----
raw.data <- gsheet::gsheet2tbl(gsheet_url)

# Clean data ----

clean.data <- raw.data[!is.na(raw.data$Date) ,]
clean.data$Date <- as.Date(clean.data$Date, format = "%Y-%m-%d")
clean.data$Time <- as.ITime(clean.data$Time)
clean.data$TimeTS <- hour(clean.data$Time)+round((minute(clean.data$Time)/60),2)

clean.data$Phase <- "Night"
idx <- clean.data$TimeTS > 4 & clean.data$TimeTS <= 10
clean.data$Phase[idx] <- "Morning"
idx <- clean.data$TimeTS > 10 & clean.data$TimeTS <= 16
clean.data$Phase[idx] <- "Afternoon"
idx <- clean.data$TimeTS > 16 & clean.data$TimeTS <= 22
clean.data$Phase[idx] <- "Evening"

clean.data <- clean.data %>% mutate(TimeDiff = TimeTS - lag(TimeTS))
idx <- is.na(clean.data$TimeDiff)
clean.data$TimeDiff[idx] <- clean.data$TimeTS[idx]
idx <- clean.data$TimeDiff < 0
idx2 <- is.na(idx)
idx[idx2] <- FALSE
clean.data$TimeDiff[idx] <- 24+clean.data$TimeDiff[idx]

# obj mgmt 1----
rm(raw.data, idx, idx2, gsheet_url)


# user functions ----
clean_plotly_legend <- function(.plotly_x, .extract_str) {
  # Inpects an x$data list in a plotly object, cleans up legend values where appropriate
  if ("legendgroup" %in% names(.plotly_x)) {
    # The list includes a legend group
    
    .plotly_x$legendgroup <- stringr::str_extract(.plotly_x$legendgroup, .extract_str)
    .plotly_x$name <- stringr::str_extract(.plotly_x$name, .extract_str)
    
  }
  .plotly_x
  
  
}

# event Swimlanes----

df.phase <- data.frame(start = c(0,4,10,16,22), end = c(4,10,16,22,24), phase = c("Night", "Morning", "Afternoon", "Evening", "Night"))
df.phase$phase <- factor(df.phase$phase, levels = c("Morning","Afternoon","Evening", "Night"))

event_swimlanes <- ggplot(clean.data, 
                          aes(x=Date,
                              y=TimeTS,
                              label = Phase,
                              colour = `Feeling Outcome`)
                          )+
  labs(x = "Date", 
       y = "Time of the Day as fraction of Hour in a 24 hour Format",
       title = "Freq distribution by Date and Phase of the Day",
       colour = NULL) +
  geom_rect(data=df.phase, 
            inherit.aes = FALSE, 
            aes(xmin= min(clean.data$Date),
                xmax= max(clean.data$Date),
                ymin=start,
                ymax=end,
                fill=phase),
            alpha=0.4) +
  scale_fill_manual(values=c("Morning" = "orange", "Afternoon" = "blue", "Evening" = "pink", "Night" = "black"),
                    labels = c("Morning","Afternoon","Evening", "Night")
                    )+
  labs(fill = NULL)+
  geom_point()
  # theme_classic()

event_swimlanes <- ggplotly(event_swimlanes)

event_swimlanes$x$data <- event_swimlanes$x$data %>% 
  map(clean_plotly_legend, "[^\\(][^,]*")
event_swimlanes

# obj mgmt 2----
rm(df.phase, event_swimlanes)

# Freq Distribution----
df.dist <- clean.data %>% 
  group_by(Date) %>%
  summarise(ave.time = round(24 / length(Date),2))

freq.dist <- ggplot(clean.data,
                    aes(x = Date))+
  geom_histogram(aes(fill = `Feeling Outcome`),
                 alpha = 0.6,
                 binwidth = 0.5) +
  labs(x = "Date",
       y = "Event Freq",
       title = "Distribution by Feeling Outcome vs. Ave time between All Events",
       fill = NULL,
       colour = NULL) +
  geom_line(df.dist,
            inherit.aes = FALSE,
            mapping = aes(Date, ave.time),
            color = 'steelblue',
            size = .8,
            alpha = 0.6)+
  geom_point(df.dist,
             mapping = aes(y = ave.time),
             color = "steelblue",
             alpha = 0.6
             )+
  expand_limits(y=c(0,20))
  


freq.dist <- ggplotly(freq.dist)
freq.dist

# obj mgmt 3----
rm(df.dist, freq.dist)

