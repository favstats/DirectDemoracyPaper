scraper_of_the_year <- function(test ,start, end) {
  
  sqp <- start:end
  
  yearlist <- list()
  firstyear <- vector()
  lastyear <- vector()
  nyears <- vector()
  text <- vector()
  for (jj in seq_along(sqp)) {
    paste0("\n Checking URL: #: ", sqp[jj], " of Country: ", scrape_dat$cntry[sqp[jj]], "\n") %>%
      bgRed$bold() %>%
      cat()
    if (!is.na(scrape_dat$urls[sqp[jj]])) {
      tryCatch({
        texts <- read_html(scrape_dat$urls[sqp[jj]]) %>% 
          html_nodes("#block-system-main") %>% 
          html_text()
      }, error = function(e){
        yearlist[sqp[jj]] <- NA
        nyears[sqp[jj]] <- NA
        firstyear[sqp[jj]] <- NA
        lastyear[sqp[jj]] <- NA
        text[sqp[jj]] <- NA
      })
      yearlist[[sqp[jj]]] <- str_extract_all(texts, years)
      text[sqp[jj]] <- texts
      nyears[sqp[jj]] <- length(yearlist[[sqp[jj]]][[1]])
      
      if (nyears[sqp[jj]] >= 1) {
        #firstyear[sqp[jj]] <- yearlist[[sqp[jj]]][[1]][1]
        firstyear[sqp[jj]] <- yearlist[[sqp[jj]]][[1]]
        lastyear[sqp[jj]] <- yearlist[[sqp[jj]]][[1]][nyears[sqp[jj]]]  
      } else {
        firstyear[sqp[jj]] <- NA
        lastyear[sqp[jj]] <- NA
      }
    } else {
      yearlist[sqp[jj]] <- NA
      nyears[sqp[jj]] <- NA
      firstyear[sqp[jj]] <- NA
      lastyear[sqp[jj]] <- NA
      text[sqp[jj]] <- NA
    }
    
    final_list <- list()
    rd <- tibble(yearlist, nyears, firstyear, lastyear, text)
    Sys.sleep(1)
    file_exists <- file.exists(
      paste0("data/final_list", "_", start, "_", sqp[jj] - 1, ".Rdata"))
    is_available <- (is.data.frame(rd) & !is.null(rd))
    
    final_list[[jj]] <- rd
    
    if(is_available) {
      if(file_exists){
        file.remove(paste0("data/final_list", "_", start, "_", sqp[jj] - 1, ".Rdata"))
      } 
      
      save(final_list, file = paste0("data/final_list", "_", start, "_", sqp[jj], ".Rdata"))
      
    }
    
    
  }
  return(rd)
}

# texts <- read_html("https://www.idea.int/node/284521") %>% 
#   html_nodes("#block-system-main") %>% 
#   html_text()


scraper_of_the_dats <- function(test ,start, end) {
  
  sqp <- start:end
  
  tablestyle <- list()
  final_tab <- list()
  for (jj in seq_along(sqp)) {
    paste0("\n Checking URL: #: ", test$urls[sqp[jj]], " of Country: ", test$cntry[sqp[jj]], "\n") %>%
      bgRed$bold() %>%
      cat()
    tablestyle[[sqp[jj]]] <- read_html(test$urls[sqp[jj]]) %>% 
      html_nodes("table") %>% 
      html_table(fill = TRUE) %>% 
      .[[1]] %>% 
      mutate(X1 = fix_the_string(X1)) %>% 
      mutate_cond(condition = X1 %nin% c("Bemerkungen", "Quellen"), 
                  X2 = fix_the_string(X2)) %>% 
      mutate_cond(condition = X1 %nin% c("Bemerkungen", "Quellen"), 
                  X3 = fix_the_string(X3))
    
    if (!is.null(suppressMessages(duplicates <- tablestyle[[sqp[jj]]] %>%
                                  janitor::get_dupes(X1)))) {
      tablestyle[[sqp[jj]]] %<>% 
        filter(X1 %nin% unique(duplicates$X1))
    }
    
    rownames(tablestyle[[sqp[jj]]]) <- tablestyle[[sqp[jj]]]$X1
    
    final_tab[[sqp[jj]]] <- tablestyle[[sqp[jj]]] %>% 
      dplyr::select(-X1, -X3) %>% 
      t() %>% 
      as_tibble() %>% 
      janitor::clean_names()
    
    if (nrow(final_tab[[sqp[jj]]])>1) {
      
      if (str_detect(names(final_tab[[sqp[jj]]]), "v1")) {
        final_tab[[sqp[jj]]] %<>% 
          select(-v1:-quellen) %>% 
          .[1,]
      } else {
        final_tab[[sqp[jj]]] %<>% 
#          select(-v1:-quellen) %>% 
          .[1,]
      }
    }
  }
  return(final_tab)
}



fix_the_string <- function(string) {
  string %<>%   
    iconv(from = "ASCII", to = "utf-8") %>% 
    stringr::str_remove_all("b\024\027b\024\001 ") %>% 
    stringr::str_replace_all("C\004", "ae") %>% 
    stringr::str_replace_all("C\\$", "ae") %>% 
    stringr::str_replace_all("C<", "ue") %>% 
    stringr::str_replace_all("b\006\022", "-")     
}


mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}