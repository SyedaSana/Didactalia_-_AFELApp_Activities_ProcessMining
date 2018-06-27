#AFEL Application Process Mining
#
#Contributors: Syeda Sana E Zainab, Rémi Venant, and Mathieu d'Aquin (@mdaquin)
#
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

##### Adding SPARQL Library
library(SPARQL)

endpoint <- 'http://localhost:3030/AFEL_Evaluation/query'

#### SPARQL Query to extract all activities of AFEL Application####

query <- 'PREFIX afl: <http://vocab.afel-project.eu/>
PREFIX extafl: <http://vocab.afel-project.eu/extension/>
PREFIX sch: <http://schema.org/>
SELECT ?activityType ?actStartTime ?actEndTime ?userid ?user
WHERE {
?activity a ?activityType .
?activity afl:user ?user .
?activity afl:eventStartDate ?actStartTime .
?activity afl:eventEndDate ?actEndTime .
?activity sch:location ?location .
?user afl:userName ?userid .
FILTER(?location ="http://afel-project.eu/")
}'

qd <- SPARQL(endpoint,query)
df <-qd$results

# load libraries for process mining ####
library(bupaR)
library(edeaR)
library(processmapR)
library(eventdataR)
library(tidyverse)
library(DiagrammeR)
library(ggplot2)
library(stringr)
library(lubridate)

df$actStartTime <- as.POSIXct(df$actStartTime, origin = "1970-01-01", tz="GMT")
df$actEndTime <- as.POSIXct(df$actEndTime, origin = "1970-01-01", tz="GMT")
events <- bupaR::activities_to_eventlog(
  head(df, n = 10000),
  case_id = 'userid',
  activity_id = 'activityType',
  resource_id = 'user',
  timestamps = c('actStartTime', 'actEndTime')
)
events <- bupaR::activities_to_eventlog(
  df,
  case_id = 'userid',
  activity_id = 'activityType',
  resource_id = 'user',
  timestamps = c('actStartTime', 'actEndTime')
)

#### Events Summary #####
events %>%
summary

Number of events:  2000
Number of cases:  69
Number of traces:  69
Number of distinct activities:  5
Average trace length:  28.98551

Start eventlog:  2018-05-02 12:20:15
End eventlog:  2018-05-18 14:32:35

activityType    userid
<http://vocab.afel-project.eu/ArtifactView>                     : 70   Length:2000
<http://vocab.afel-project.eu/extension/DisplayChange>          :998   Class :character
<http://vocab.afel-project.eu/extension/GoBack>                 :416   Mode  :character
<http://vocab.afel-project.eu/extension/RecommendedArtifactView>: 36
<http://vocab.afel-project.eu/extension/ScopeView>              :480


user      activity_instance_id       lifecycle_id
<http://vocab.afel-project.eu/User#project.afel+028>:  58   Length:2000          actEndTime  :1000
<http://vocab.afel-project.eu/User#project.afel+076>:  58   Class :character     actStartTime:1000
<http://vocab.afel-project.eu/User#project.afel+020>:  48   Mode  :character
<http://vocab.afel-project.eu/User#project.afel+032>:  46
<http://vocab.afel-project.eu/User#project.afel+066>:  46
<http://vocab.afel-project.eu/User#project.afel+027>:  44
(Other)                                             :1700
timestamp                       .order
Min.   :2018-05-02 12:20:15   Min.   :   1.0
1st Qu.:2018-05-04 04:18:03   1st Qu.: 500.8
Median :2018-05-08 08:25:23   Median :1000.5
Mean   :2018-05-09 10:16:56   Mean   :1000.5
3rd Qu.:2018-05-14 14:23:13   3rd Qu.:1500.2
Max.   :2018-05-18 14:32:35   Max.   :2000.0

#### Activities absolute and relative frequency ####
events %>%
activity_frequency(level = "activity")
# A tibble: 5 x 3
activityType                                                     absolute relative
<fct>                                                               <int>    <dbl>
1 <http://vocab.afel-project.eu/ArtifactView>                            35    0.035
2 <http://vocab.afel-project.eu/extension/DisplayChange>                499    0.499
3 <http://vocab.afel-project.eu/extension/GoBack>                       208    0.208
4 <http://vocab.afel-project.eu/extension/RecommendedArtifactView>       18    0.018
5 <http://vocab.afel-project.eu/extension/ScopeView>                    240    0.24

#### Process Map ####
  events %>%
  filter_activity_frequency(percentage = 1.0) %>% # show only most frequent activities
  filter_trace_frequency(percentage = .80) %>%    # show only the most frequent traces
  process_map()

#### Process Map w.r.t Performance####
  events %>%
   filter_activity_frequency(percentage = 1.0) %>% # show only most frequent activities
   filter_trace_frequency(percentage = .80) %>%    # show only the most frequent traces
   process_map(performance(mean, "min"),)

  # precedent matrix ####
  precedence_matrix <- events %>%
  filter_activity_frequency(percentage = 1.0) %>% # show only most frequent activities
  filter_trace_frequency(percentage = .80) %>%    # show only the most frequent traces
  precedence_matrix() %>%
  plot()

  ggsave('Output_Image/precedence_matrix.png', precedence_matrix)
  rm(precedence_matrix)

  # trace explorer
  trace_explorer <- events %>%
  trace_explorer(coverage = 0.5)

  ggsave('./02-output/04_pm-bupar_trace explorer.png', trace_explorer, width = 12)
  rm(trace_explorer)

  # idotted chart
  chart <- events %>%
  dotted_chart() %% ggsave('Output_Image/dotted_chart.png', chart, width = 12)

  # resource map ####
  events %>%
    filter_activity_frequency(percentage = .1) %>% # show only most frequent resources
    filter_trace_frequency(percentage = .8) %>%    # show only the most frequent traces
    resource_map(render = F) %>%

  # resource matrix ####
  resource_matrix <- events %>%
    filter_activity_frequency(percentage = 1.0) %>% # show only most frequent activities
    filter_trace_frequency(percentage = .80) %>%    # show only the most frequent traces
    resource_matrix() %>%
    plot()

  ggsave('Output_Image/resource matrix.png', resource_matrix)
  rm(resource_matrix)
