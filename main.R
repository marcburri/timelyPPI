rm(list = ls())
library(stringr)
library(readxl)
library(tidyverse)
library(tsbox)
library(tstools)
library(tempdisagg)
library(imputeTS)
library(forecast)
library(data.tree)
library(quantmod)
source("utils.R")


#### Push to data Github?
push_to_gh <- T

#### Update data from Excel?
update_excel <- F

# Update Data from Excel
if (update_excel) {
source("load_data.R")
}
  
# Load PPI 2020 
load("PPI_2020.RData")

# Load configuration
# If you want to load another configuration: mutate and load custom_config
load("custom_config.RData")


# Get level (depth) of series
ppis_level_nr <- as_tibble(lapply(ppis_level.2020, function(x) {
  which(as.logical(x))
}))

# Get series names
ppis_names <- as_tibble(lapply(ppis_level_names.2020, function(x) {
  x[which(!is.na(x))]
}))

# Set up tree
nmes <- colnames(ppis_level.2020)
tree <- Node$new("PPI")
sapply(c("A", "B", "C", "D", "E"), function(x) tree$AddChild(x))

for (ii in 1:length(nmes)) {
  if (ppis_level_nr[ii] != 1) {
    pn <- find_parent(colnames(ppis_level_nr[ii]), ppis_level_nr)
    FindNode(tree, pn)$AddChild(colnames(ppis_level_nr[ii]))
  }
}

ppis.2010$PPI <- PPI.2010
ppis.2015$PPI <- PPI.2015
ppis.2020$PPI <- PPI.2020
# Assign initial series and weights to tree & implement shift if needed
tree$Do(function(x) {
  x$fullName <- ppis_names[[x$name]]
  x$w.20 <- round(as.numeric(ppis_weights.all[[x$name]][1]), 4)
  x$w.15 <- round(as.numeric(ppis_weights.all[[x$name]][2]), 4)
  x$w.10 <- round(as.numeric(ppis_weights.all[[x$name]][3]), 4)
  if (x$name == "21.1"){# Pharmazeutische Grundstoffe Shift
    x$TSI.20 <- round(ts(ppis.2020[[x$name]], start = c(2020, 12), frequency = 12), 4)
    x$TSI.15 <- round(ts(ppis.2015[[x$name]], start = c(2015, 12), frequency = 12), 4)
    x$TSI.10 <- round(ts(ppis.2010[[x$name]], start = c(2010, 12), frequency = 12), 4)
    x$ORIG <- ts_chain(x$TSI.20, x$TSI.15) %>% ts_chain(x$TSI.10)
    x$FTSI <- shift_time(x$ORIG, 2, 2010)
  }
  else if (x$name == "21.2"){# Pharmazeutische Spezialitäten Shift
    x$TSI.20 <- round(ts(ppis.2020[[x$name]], start = c(2020, 12), frequency = 12), 4)
    x$TSI.15 <- round(ts(ppis.2015[[x$name]], start = c(2015, 12), frequency = 12), 4)
    x$TSI.10 <- round(ts(ppis.2010[[x$name]], start = c(2010, 12), frequency = 12), 4)
    x$ORIG <- ts_chain(x$TSI.20, x$TSI.15) %>% ts_chain(x$TSI.10)
    x$FTSI <- shift_time(x$ORIG, 1, 2010)
  } 
  else if (x$name == "20.14" | x$name == "20.5"){# Chemische Grundstoffe & Andere Shift
    x$TSI.20 <- round(ts(ppis.2020[[x$name]], start = c(2020, 12), frequency = 12), 4)
    x$TSI.15 <- round(ts(ppis.2015[[x$name]], start = c(2015, 12), frequency = 12), 4)
    x$TSI.10 <- round(ts(ppis.2010[[x$name]], start = c(2010, 12), frequency = 12), 4)
    x$ORIG <- ts_chain(x$TSI.20, x$TSI.15) %>% ts_chain(x$TSI.10)
    x$FTSI <- shift_time(x$ORIG, 2, 2010)
  }
  else {# No shift
    x$TSI.20 <- round(ts(ppis.2020[[x$name]], start = c(2020, 12), frequency = 12), 4)
    x$TSI.15 <- round(ts(ppis.2015[[x$name]], start = c(2015, 12), frequency = 12), 4)
    x$TSI.10 <- round(ts(ppis.2010[[x$name]], start = c(2010, 12), frequency = 12), 4)
    x$FTSI <- ts_chain(x$TSI.20, x$TSI.15) %>% ts_chain(x$TSI.10)
    x$ORIG <- ts_chain(x$TSI.20, x$TSI.15) %>% ts_chain(x$TSI.10)
  }
  
},
traversal = "post-order"
)

# create nodes with rest
for (ii in 6:1) {
  tree$Do(function(x) {
    if (ii == 1) {
      tree$w.20 <- round(Aggregate(x, "w.20", sum, na.rm = T), 4)
      tree$w.15 <- round(Aggregate(x, "w.15", sum, na.rm = T), 4)
      tree$w.10 <- round(Aggregate(x, "w.10", sum, na.rm = T), 4)
    }
    if (round(Aggregate(x, "w.15", sum), 4) != round(x$w.15, 4) | 
        round(Aggregate(x, "w.10", sum), 4) != round(x$w.10, 4) | 
        round(Aggregate(x, "w.20", sum), 4) != round(x$w.20, 4)) {
      
      nw15 <- round(Aggregate(x, "w.15", sum, na.rm = T), 4)
      nw10 <- round(Aggregate(x, "w.10", sum, na.rm = T), 4)
      nw20 <- round(Aggregate(x, "w.20", sum, na.rm = T), 4)
      
      # Add new child ".rest"
      x$AddChild(paste(x$name, ".rest", sep = ""))
      nnode <- FindNode(tree, paste(x$name, ".rest", sep = ""))
      nnode$fullName <- paste(x$name, ".rest", sep = "")

      nnode$w.20 <- round(x$w.20, 4) - nw20
      nnode$w.15 <- round(x$w.15, 4) - nw15
      nnode$w.10 <- round(x$w.10, 4) - nw10
      
      # If weights do not add up to 1, calculate rest series
      TSR <- getRest(x, x$children)
      nnode$TSI.10 <- TSR$TS.10
      nnode$TSI.15 <- TSR$TS.15
      nnode$TSI.20 <- TSR$TS.20
      
      # Chain rest series
      cR <- chainRest(x, TSR)
      nnode$ORIG <- cR$ORIG
      nnode$FTSI <- cR$FTSI
      
    }
  },
  traversal = "post-order",
  filterFun = function(x) {
    x$level == ii & !x$isLeaf
  }
  )
}

# Remove stair values
tree$Do(function(x) {
  message(x$name)
  x$TSI.ND.20 <- remove.double(x$TSI.20)
  x$TSI.ND.15 <- remove.double(x$TSI.15)
  x$TSI.ND.10 <- remove.double(x$TSI.10)
  x$FTSI.ND <- remove.double(x$FTSI)
},
traversal = "post-order"
)

# Load exogenous variables (Oil price, nominal exchangerate, trend)
exo_raw <- ts_fred(c("WTISPLC", "EXSZUS")) %>% ts_ts() # NBCHBIS
exo_raw <- window(exo_raw, start = c(2010,1), end=c(actual_y,actual_m))
exo <- ts_tbl(exo_raw) %>%
  ts_wide() %>%
  #mutate(neer = NBCHBIS) %>%
  mutate(exch= EXSZUS) %>%
  mutate(oil =  WTISPLC*EXSZUS) %>%
  select(c("time", "exch", "oil")) %>% #neer
  ts_long %>% ts_ts()
trend <- ts(1:((actual_y-2010-1)*12+1+actual_m),start =c(2010,1), end=c(actual_y,actual_m), frequency = 12)

# Assign series to leaf nodes
tree$Do(function(x) {
  print(paste0("Calculate timely series: ", x$name))
  if (sum(is.na(x$FTSI.ND)) >= 1) {

    # Use predefined method to interpolate 
    AC <- AssignCustom(x, config, exo, trend, actual_y, actual_m)
    x$FTS.C <- AC$ts
    x$FTS.MOD.TD <- AC$mod.td
    x$FTS.MOD.AR <- AC$mod.ar
  } 
  else {# If no missing values
    x$FTS.C <- x$FTSI.ND
    x$FTS.C <- x$FTS.C / window(x$FTSI, start = c(2020, 12), end = c(2020, 12))[[1]] * 100
  }
  },
traversal = "post-order",
filterFun = function(x) {
  x$isLeaf
}
)

# Aggregate PPI
for (ii in 6:1) {
  tree$Do(function(x) {
    print(paste0("Aggregate: ",x$name))
    
    # Aggregate original series (as Control)
    TS.AG <- AggregateOrig(x$children)
    x$FTS.AG <- TS.AG$FTS
    x$TS.AG.10 <- TS.AG$TS.10
    x$TS.AG.15 <- TS.AG$TS.15
    x$TS.AG.20 <- TS.AG$TS.20
    
    # Aggregate interpolated series 
    TS.C <- AggregateCustom(x$children)
    x$FTS.C <- TS.C$FTS
  },
  traversal = "post-order",
  filterFun = function(x) {
    x$level == ii & !x$isLeaf
  }
  )
}

# save everything
tree$fullName <- "PPI"
save(tree, file = "tree.RData")
last <- end(tree$FTS.C)
save(tree, file = paste0("old_trees/tree_", last[2], "_",last[1], ".RData"))

# Push data to github
if (push_to_gh) {
source("push.R")
}
