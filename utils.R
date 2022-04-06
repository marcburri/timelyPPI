# Get data from FRED
ts_fred <- function(..., class = "data.frame") {
  symb <- c(...)
  dta.env <- new.env()
  suppressMessages(getSymbols(symb, env = dta.env, src = "FRED"))
  z <- data.table::rbindlist(lapply(as.list(dta.env), ts_dt), idcol = "id")
  tsbox:::as_class(class)(z)
}

# Find parent Node
find_parent <- function(node.name, level_data) {
  node_level <- level_data[node.name]
  col <- which(colnames(level_data) == node.name)
  ii <- 1
  node_level_prev <- level_data[col - ii]
  while (node_level <= node_level_prev) {
    ii <- ii + 1
    node_level_prev <- level_data[col - ii]
  }
  parent_col <- col - ii
  colnames(level_data[parent_col])
}

# Shift Function
shift_time <- function(series, months, base_year) {
  if (base_year == 2010) {
    strt <- c(2010,12)
  } else if (base_year == 2015) {
    strt <- c(2015,12)
  }
  len <- length(series)
  
  series_out <- 
  round(
    ts(
      c(series[(1+months):len], rep(series[len], months)), 
           start = strt, frequency = 12), 
    4)
  series_out
}

# Remove stair values
remove.double <- function(df) {
  nrows <- length(df)
  new.df <- data.frame(matrix(ncol = 1, nrow = nrows))
  names(new.df) <- "series"
  new.df[1, 1] <- df[1]
  for (ii in 2:nrows) {
    if (df[ii - 1] == df[ii] & df[ii] != 0) {
      new.df[ii, 1] <- NA
    } else {
      new.df[ii, 1] <- df[ii]
    }
  }
  ts(new.df$series, start = start(df), frequency = 12)
}

chainRest <- function(x, TSR) {

  if (all(TSR$TS.10 == 0) & all(TSR$TS.15 == 0) & all(TSR$TS.20 == 0)) {
    message(paste0(nnode$fullName, " all = 0, this shouldn't happen!"))
  }
  else if (all(TSR$TS.10 == 0) & all(TSR$TS.15 == 0) & !all(TSR$TS.20 == 0)) {
    ORIG <- ts(c(TSR$TS.10[1:(length(TSR$TS.10)-1)],
                 TSR$TS.15[1:(length(TSR$TS.15)-1)],
                 TSR$TS.20),
               start = c(2010, 12), frequency = 12)
    FTSI <- ORIG
  }
  else if (all(TSR$TS.10 == 0) & !all(TSR$TS.15 == 0) & !all(TSR$TS.20 == 0)) {
    ORIG <- ts(c(TSR$TS.10[1:(length(TSR$TS.10)-1)],
                 ts_chain(TSR$TS.20, TSR$TS.15)),
               start = c(2010, 12), frequency = 12)
    FTSI <-  ORIG
  }
  else if (!all(TSR$TS.10 == 0) & all(TSR$TS.15 == 0) & !all(TSR$TS.20 == 0)) {
    ORIG <- ts(c(TSR$TS.10[1:(length(TSR$TS.10)-1)] ,
                 TSR$TS.15[1:(length(TSR$TS.15)-1)],
                 TSR$TS.20),
               start = c(2010, 12), frequency = 12)
    FTSI <-  ORIG
  }
  else if (all(TSR$TS.10 == 0) & !all(TSR$TS.15 == 0) & all(TSR$TS.20 == 0)) {
    ORIG <- ts(c(TSR$TS.10[1:(length(TSR$TS.10)-1)],
                 TSR$TS.15 * 100 / TSR$TS.15[length(TSR$TS.15)],
                 TSR$TS.20[1:(length(TSR$TS.20)-1)]),
               start = c(2010, 12), frequency = 12)
    FTSI <-  ORIG
  }
  else if (!all(TSR$TS.10 == 0) & all(TSR$TS.15 == 0) & all(TSR$TS.20 == 0)) {
    ORIG <- ts(c(TSR$TS.10,
                 TSR$TS.15[1:(length(TSR$TS.15)-1)],
                 TSR$TS.20[1:(length(TSR$TS.20)-1)]),
               start = c(2010, 12), frequency = 12)
    FTSI <- ORIG
  }
  else if (!all(TSR$TS.10 == 0) & !all(TSR$TS.15 == 0) & all(TSR$TS.20 == 0)) {
    message(paste0(nnode$fullName," 20 = 0"))
  }
  else if (!all(TSR$TS.10 == 0) & !all(TSR$TS.15 == 0) & !all(TSR$TS.20 == 0)) {
    ORIG <- ts_chain(ts(TSR$TS.20, start = c(2020, 12), frequency = 12),
                     ts(TSR$TS.15, start = c(2015, 12), frequency = 12),
                     ts(TSR$TS.10, start = c(2010, 12), frequency = 12)
    )
    FTSI <- ORIG
  }
  return(list(ORIG=ORIG, FTSI=FTSI))
}

# Assign Custom
AssignCustom <- function(x,config,exo, trend, actual_y, actual_m) {
  def <- filter(config, name == x$name)$def
  mod.td <- NULL
  mod.ar <-  NULL
  if (def == "Interpolieren" ||  def == "Keine NA") {
    ts.nd <- x$FTSI.ND
    # Define
    is.na.dez <- is.na(window(ts.nd, start = c(2014,12), end = c(2014,12)))
    is.na.last <- is.na(ts.nd[length(ts.nd)])
    
    if (is.na.dez) {ts.nd <- set.dez.value(ts.nd)}
    if(is.na.last) {
      pattern <- NApattern(ts.nd)
      h <- pattern[length(pattern)]
      ts.nd1 <- ts.nd[1:(length(ts.nd)-h)]
      
      ts.hf1 <- ts(
        na.interpolation(ts.nd1, option = "stine"),
        start = c(2010,12),
        frequency = 12
        )
      
      t <- end(ts.hf1)
      
      xreg <- ts_c(
        window(exo, start=c(2010,12)), "trend" = window(trend, start=c(2010,12),  
        end = c(actual_y, actual_m))
        )
      
      mod.ar <- auto.arima(ts.hf1, xreg = window(xreg, end = t))
      
      tt <- if (t[2] == 12) {c(t[1]+1, 1)} else {c(t[1],t[2]+1)}
      
      fc <- forecast(mod.ar, h = h, xreg = window(xreg, start = tt))
      
      ts.hf <- ts_bind(ts.hf1, fc$mean)
      
    } else {
    ts.hf <- na.interpolation(ts.nd, option = "stine")
    }
  }
  else if (def == "TD") {
    ts.nd <- x$FTSI.ND
    if (x$name == "21.2") {
      ts.nd <- special.treatment.pharma(ts.nd)
    }
    if (x$name == "25.1") {
      ts.nd <- special.treatment.steel(ts.nd)
    }

    pattern <- NApattern(ts.nd)
    
    if (pattern[2] == 1){freq <- 6}
    if (pattern[2] == 2){freq <- 4}
    if (pattern[2] == 5) {freq <- 2}
    if (pattern[2] == 11) {freq <- 1}
    
    # Define
    shift <- pattern[1]
    gaps <- pattern[2]
    picker <- if (shift != gaps) {(shift+2):length(ts.nd)} else {1:length(ts.nd)}
    is.na.last <- is.na(ts.nd[length(ts.nd)])
    nr.last.na <- pattern[length(pattern)]
    
    ts.lf <- ts(
      ts.nd[picker][!is.na(ts.nd[picker])],
      freq = freq)
    
    if (gaps == shift) {subs <- shift} else {subs <- gaps-shift-1}
    exo1 <- ts(exo[(12-subs):length(exo[,1]),], frequency = 12)
    trend1 <- ts(trend[(12-subs):length(trend)], frequency = 12)
    
    mod.td <- td(ts.lf~exo1+trend1, to = 12, method = "chow-lin-maxlog", conversion = "last", truncated.rho= 0.5)
    
    ts.hf <- predict(mod.td)
    
    if (is.na.last) {
      to_idx <- which(round(ts.hf,4)==round(ts.lf[length(ts.lf)],4))
      if (gaps == shift) {
        ts.hf <- ts(ts.hf[(shift+1):to_idx], 
                    start=c(2010,12), 
                    freq = 12)
      } else{
      ts.hf <- ts(c(ts.nd[1:(1+shift)], 
                ts.hf[(gaps+1):to_idx]), # -gaps +1)
                start=c(2010,12), 
                freq = 12)
    }
      
      ts.hf1 <- na.interpolation(ts.hf, option = "stine")
      
      t <- end(ts.hf1)
      
      ts.hf2 <- window(ts.hf1,  end = c(actual_y, actual_m), extend = T)
      
      
      xreg <- ts_c(window(exo, start=c(2010,12)), "trend" = window(trend, start=c(2010,12),  
                   end = c(actual_y, actual_m))) 
      
      mod.ar <- auto.arima(ts.hf1, xreg = window(xreg, end = t))
      
      h <- sum(is.na(ts.hf2))
      
      tt <- if (t[2] == 12) {c(t[1]+1, 1)} else {c(t[1],t[2]+1)}
      
      fc <- forecast(mod.ar, h = h, xreg = window(xreg, start = tt))
      
      ts.hf <- ts_bind(ts.hf1, fc$mean)
      
    } else if (gaps == shift && !is.na.last) {
      ts.hf <- ts(ts.hf[(shift+1):(length(ts.hf))], 
                  start=c(2010,12), 
                  freq = 12)
    } else {
      to_idx <- which(round(ts.hf,4)==round(ts.lf[length(ts.lf)],4))
      ts.hf <- ts(c(ts.nd[1:(1+shift)], 
                ts.hf[(gaps+1):to_idx]), 
                start=c(2010,12), 
                freq = 12)
      
      ts.hf <- na.interpolation(ts.hf, option = "stine")
    }
    
  } 
  if (!window(ts.hf, start = c(2020, 12), end = c(2020, 12))[[1]] == 0) {
  ts.hf <- ts.hf / window(ts.hf, start = c(2020, 12), end = c(2020, 12))[[1]] * 100
  }
  list(ts=ts.hf, mod.td=mod.td, mod.ar=mod.ar)
}

special.treatment.pharma <- function(ts) {
  ts.new <- c(ts[1])
  for (ii in 2:length(ts)) {
    if ((ii %% 12) %in% c(1,3, 4,6, 7,9 ,10, 0)) { #c(1,2, 4,5, 7,8, 10,11)
      ts.new <- c(ts.new, NA)
    } else {
      ts.new <- c(ts.new, ts[ii])
    }
  }
  ts(ts.new, start = c(2010,12), frequency = 12)
}

special.treatment.steel <- function(ts) {
  ts.new <- c(ts[1])
  for (ii in 2:length(ts)) {
    if ((ii %% 12) %in% c(1,3, 4,6, 7,9 ,10, 0)) {
      ts.new <- c(ts.new, NA)
    } else {
      ts.new <- c(ts.new, ts[ii])
    }
  }
  ts(ts.new, start = c(2010,12), frequency = 12)
}

set.dez.value <- function(ts) {
  val <- NA
  counter <- 1
  while (is.na(val) && counter < 12) {
    val <- ts[49-counter]
    counter <- counter+1
  }
  ts[49] <- val
  ts
}

NApattern <- function(ts){
  pattern <- NULL
  if (all(!is.na(ts[1:2]))) {
    pattern <- 0
  }
  for (ii in 2:length(ts)) {
    if (is.na(ts[ii]) && !is.na(ts[ii+1])){
      counter <- 1
      prev <- ts[ii-counter]
      while(is.na(prev)){
        counter <- counter+1
        prev <- ts[ii-counter]
      }
      pattern <- c(pattern,counter)
    }
  }
  if (is.na(ts[length(ts)])){
    counter <- 1
    prev <- ts[length(ts)-counter]
    while(is.na(prev)){
      counter <- counter+1
      prev <- ts[length(ts)-counter]
    }
    pattern <- c(pattern,counter)
  }
pattern
}

# Aggregate Levels
AggregateCustom <- function(x) {
  lgt <- length(x)
  tot.w.10 <- 0
  tot.w.15 <- 0
  tot.w.20 <- 0
  tot.ts10 <- 0
  tot.ts15 <- 0
  tot.ts20 <- 0
  for (ii in 1:lgt) {
    
    tot.w.10 <- tot.w.10 + x[[ii]]$w.10
    tot.w.15 <- tot.w.15 + x[[ii]]$w.15
    tot.w.20 <- tot.w.20 + x[[ii]]$w.20
    
    tot.ts10 <- tot.ts10 + window(x[[ii]]$FTS.C, start = c(2010, 12), end = c(2015, 12)) * x[[ii]]$w.10
    tot.ts15 <- tot.ts15 + window(x[[ii]]$FTS.C, start = c(2015, 12), end = c(2020, 12)) * x[[ii]]$w.15
    tot.ts20 <- tot.ts20 + window(x[[ii]]$FTS.C, start = c(2020, 12)) * x[[ii]]$w.20
    
  }
  TS.20 <- ts(tot.ts20 / tot.w.20, start = c(2020, 12), frequency = 12)
  TS.15 <- ts(tot.ts15 / tot.w.15, start = c(2015, 12), frequency = 12)
  TS.10 <- ts(tot.ts10 / tot.w.10, start = c(2010, 12), frequency = 12)
  FTS <- ts_chain(TS.20, TS.15) %>% ts_chain(TS.10)
  
  return(list(TS.10 = TS.10, TS.15 = TS.15, TS.20 = TS.20, FTS = FTS))
}


AggregateOrig <- function(x) {
  lgt <- length(x)
  tot.w.10 <- 0
  tot.w.15 <- 0
  tot.w.20 <- 0
  tot.ts10 <- 0
  tot.ts15 <- 0
  tot.ts20 <- 0
  for (ii in 1:lgt) {
    tot.w.10 <- tot.w.10 + x[[ii]]$w.10
    tot.w.15 <- tot.w.15 + x[[ii]]$w.15
    tot.w.20 <- tot.w.20 + x[[ii]]$w.20

    tot.ts10 <- tot.ts10 + x[[ii]]$TSI.10 * x[[ii]]$w.10
    tot.ts15 <- tot.ts15 + x[[ii]]$TSI.15 * x[[ii]]$w.15
    tot.ts20 <- tot.ts20 + x[[ii]]$TSI.20 * x[[ii]]$w.20
  }
  TS.20 <- ts(tot.ts20 / tot.w.20, start = c(2020, 12), frequency = 12)
  TS.15 <- ts(tot.ts15 / tot.w.15, start = c(2015, 12), frequency = 12)
  TS.10 <- ts(tot.ts10 / tot.w.10, start = c(2010, 12), frequency = 12)
  FTS <- ts_chain(TS.20, TS.15) %>% ts_chain(TS.10)
  return(list(TS.10 = TS.10, TS.15 = TS.15, TS.20 = TS.20, FTS = FTS))
}

getRest <- function(parent, childs) {
  lgt <- length(childs)
  tot.w10 <- 0
  tot.w15 <- 0
  tot.w20 <- 0
  numerator10 <- parent$w.10 * parent$TSI.10
  numerator15 <- parent$w.15 * parent$TSI.15
  numerator20 <- parent$w.20 * parent$TSI.20
  denominator10 <- parent$w.10
  denominator15 <- parent$w.15
  denominator20 <- parent$w.20
  for (ii in 1:lgt) {
    if (ii == 0) {
      next
    }
    if (grepl("rest", childs[[ii]]$name)) {
      next
    }

    tot.w10 <- tot.w10 + childs[[ii]]$w.10
    tot.w15 <- tot.w15 + childs[[ii]]$w.15
    tot.w20 <- tot.w20 + childs[[ii]]$w.20
    numerator10 <- numerator10 - childs[[ii]]$w.10 * childs[[ii]]$TSI.10
    numerator15 <- numerator15 - childs[[ii]]$w.15 * childs[[ii]]$TSI.15
    numerator20 <- numerator20 - childs[[ii]]$w.20 * childs[[ii]]$TSI.20
    denominator10 <- denominator10 - childs[[ii]]$w.10
    denominator15 <- denominator15 - childs[[ii]]$w.15
    denominator20 <- denominator20 - childs[[ii]]$w.20
  }

  if (round(tot.w10, 4) == round(parent$w.10, 4)) {
    rest.10 <- ts(as_tibble(rep(0, length(numerator10))), start = c(2010, 12), frequency = 12)
  } else {
    rest.10 <- numerator10 / denominator10
  }
  if (round(tot.w15, 4) == round(parent$w.15, 4)) {
    rest.15 <- ts(as_tibble(rep(0, length(numerator15))), start = c(2015, 12), frequency = 12)
  } else {
    rest.15 <- numerator15 / denominator15
  }
  if (round(tot.w20, 4) == round(parent$w.20, 4)) {
    rest.20 <- ts(as_tibble(rep(0, length(numerator20))), start = c(2020, 12), frequency = 12)
  } else {
    rest.20 <- numerator20 / denominator20
  }
  return(list(TS.10 = rest.10, TS.15 = rest.15, TS.20 = rest.20))
}

## Data Tree Function
FindNodeByFullName <- function(node, name) {
  trav <- Traverse(node, filterFun = function(x) {
    x$fullName == name
  })
  if (length(trav) == 0) {
    return(NULL)
  }
  return(trav[[1]])
}

ppi_gui <- function() {
  library(shiny)
  library(shinyjs)
  library(dygraphs)
  library(DiagrammeR)
  library(data.tree)
  library(tsbox)
  library(tempdisagg)
  
  ############ Load Data ################
  #load("PPI_2020.RData")
  load("tree.RData")
  load("custom_config.RData")
  
  
  SetGraphStyle(tree, rankdir = "TB")
  SetEdgeStyle(tree, arrowhead = "vee", color = "grey35", penwidth = 2)
  SetNodeStyle(tree, style = "filled,rounded", shape = "box", fillcolor = "grey35", 
               fontname = "helvetica", tooltip = GetDefaultTooltip)
  
  ui <-  # Define UI for application 
    shinyUI(
      bootstrapPage(
        headerPanel('PPI'),
        
        sidebarPanel(
          useShinyjs(),
          selectInput('noga', 'Noga Stufe', choices = c("isLeaf", "isNotLeaf")),
          selectInput('sektor', 'Sektor', choices = ""),
          hr(),
          selectInput('method', 'Methode', choices = c("Wählen...", "Interpolieren", "TD", "Keine NA")),
          hr(),
          h4("Documentation:"),
          h5("Find more details on https://marcburri.github.io/posts/2019/12/18/timelyPPI/")
        ),
        mainPanel(
          
          tabsetPanel(
            tabPanel("Plot Level", dygraphOutput("dygraph1")), 
            tabPanel("Plot PCY", dygraphOutput("dygraph2")), 
            tabPanel("Data", dataTableOutput("data")), 
            tabPanel("Tree", grVizOutput("tree"))
          ),
          
          h2("Model Summary"),
          verbatimTextOutput("sum")
          
          
        )
      )
    )
  
    # Define server logic 
    server <-shinyServer(function(input, output, session) {
      
      observe({
        updateSelectInput(session,
                          "noga",
                          choices = c("isLeaf", "isNotLeaf"))
      }, priority = 3)
      
      
      
      observe({
        updateSelectInput(
          session,
          "sektor",
          choices = as.character(tree$Get(function(x) x$fullName,
                                          filterFun = get(as.character(input$noga))))
        )
        
      }, priority = 2)
      
      observe({
        updateSelectInput(
          session,
          "method",
          selected = config[config$fullName==input$sektor,]$def
        )
      }, priority = 1)
      
      
      observeEvent(input$method, {
        if (input$sektor == "" || input$noga == "isNotLeaf") {}
        else {
          config[config$fullName==input$sektor,]$def <<- input$method
        }
      }, priority = -1)
      
      observeEvent(input$noga, {
        if (input$noga == "isNotLeaf") {
          disable("method")
        } else if (input$noga == "isLeaf")
          disable("method")
          #enable("method") # To be able to edit config file
      })
      
      session$onSessionEnded(function() {
        save(config, file="custom_config.RData")
      })
      
      
      FTS <- reactive({
        if (as.character(input$noga)=="isLeaf"){
          xts <- ts_c(
            "Orig" = FindNodeByFullName(tree,input$sektor)$ORIG,
            "ND" = FindNodeByFullName(tree,input$sektor)$FTSI.ND,
            "Timely" = FindNodeByFullName(tree,input$sektor)$FTS.C
          )

          lst <- list(xts=xts, ttl =  FindNodeByFullName(tree,input$sektor)$name, 
                      w15 = FindNodeByFullName(tree,input$sektor)$w.15, w10 = FindNodeByFullName(tree,input$sektor)$w.10)
          lst
        } else if(as.character(input$noga)=="isNotLeaf")  {
          xts <- ts_c(
            "Orig" = FindNodeByFullName(tree,input$sektor)$ORIG,
            "ND" = FindNodeByFullName(tree,input$sektor)$FTSI.ND,
            "Timely" = FindNodeByFullName(tree,input$sektor)$FTS.C
          )

          lst <- list(xts=xts, ttl =  FindNodeByFullName(tree,input$sektor)$name, 
                      w15 = FindNodeByFullName(tree,input$sektor)$w.15, w10 = FindNodeByFullName(tree,input$sektor)$w.10)
          lst
        }
        
      })
      
      
      FTS_PCY <- reactive({
        if (as.character(input$noga)=="isLeaf"){
          xts <- ts_c(
            "Orig" = ts_pcy(FindNodeByFullName(tree,input$sektor)$ORIG),
            "Timely" = ts_pcy(FindNodeByFullName(tree,input$sektor)$FTS.C)
          )

          lst <- list(xts=xts, ttl =  FindNodeByFullName(tree,input$sektor)$name, 
                      w15 = FindNodeByFullName(tree,input$sektor)$w.15, w10 = FindNodeByFullName(tree,input$sektor)$w.10)
          lst
        } else if(as.character(input$noga)=="isNotLeaf")  {
          xts <- ts_c(
            "Orig" = ts_pcy(FindNodeByFullName(tree,input$sektor)$ORIG),
            "Timely" = ts_pcy(FindNodeByFullName(tree,input$sektor)$FTS.C)
          )
          lst <- list(xts=xts, ttl =  FindNodeByFullName(tree,input$sektor)$name, 
                      w15 = FindNodeByFullName(tree,input$sektor)$w.15, w10 = FindNodeByFullName(tree,input$sektor)$w.10)
          lst
        }
        
      })
      
      
      MOD <- reactive({
        if (as.character(input$noga)=="isLeaf"){
          if (!is.null(FindNodeByFullName(tree,input$sektor)$FTS.MOD.TD)){
            mod.td <- summary(FindNodeByFullName(tree,input$sektor)$FTS.MOD.TD)
          } else {
            mod.td <- "No TD"
          }
          if (!is.null(FindNodeByFullName(tree,input$sektor)$FTS.MOD.AR)){
            mod.ar <- summary(FindNodeByFullName(tree,input$sektor)$FTS.MOD.AR)
          } else {
            mod.ar <- "No Forecast"
          }
        } else if(as.character(input$noga)=="isNotLeaf")  {
          mod.td <- "Only available for leaf nodes."
          mod.ar <- "Only available for leaf nodes."
        }
        lst <- list(mod.ar = mod.ar, mod.td = mod.td)
      })
      
      
      plot_tree <- reactive({
        tree$Do(function(x) SetNodeStyle(x, fillcolor = "grey", arrowhead = "vee", style = "filled,rounded", shape = "box"))
        
        xx <- FindNodeByFullName(tree,input$sektor)
        SetNodeStyle(xx, fillcolor = "Thistle") 
        if (xx$level == 4) {
          xx <- xx$parent
        } else if (xx$level == 5) {
          xx <- xx$parent$parent
        } else if (xx$level == 6) {
          xx <- xx$parent$parent$parent
        }
        xx
      })
      
      
      output$dygraph1 <- renderDygraph({
        lst <- FTS()
        dygraph(lst$xts, main = lst$ttl) %>%
          dyAxis("y", label = paste("Wgt 10: ",lst$w10, " | Wgt 15: ", lst$w15," | Wgt 20: ", lst$w20, sep = "")) %>%
          dySeries(c("Timely"), label = "Timely") %>%
          dySeries(c("ND"), label = "ND", drawPoints = T, pointSize = 3) %>%
          dyOptions(drawGrid = input$showgrid)
      })
      
      output$dygraph2 <- renderDygraph({
        lst <- FTS_PCY()
        dygraph(lst$xts, main = lst$ttl) %>%
          dyAxis("y", label = paste("Wgt 10: ",lst$w10, " | Wgt 15: ", lst$w15," | Wgt 20: ", lst$w20, sep = "")) %>%
          dySeries(c("Orig"), label = "Orig") %>%
          dySeries(c("Timely"), label = "Timely") %>%
          dyOptions(drawGrid = input$showgrid)
      })
      
      
      output$data <- renderDataTable({
        lst <- FTS()
        xts2 <- ts_ts(lst$xts)
        xts2  
      })
      
      output$tree <- renderGrViz({
        xx <- plot_tree()
        grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(xx)))
      })
      
      output$sum <- renderPrint({
        mods <- MOD()
        mods[1]
        mods[2]
      })
    })  
  
    runApp(shinyApp(ui = ui, server = server))
  
}
