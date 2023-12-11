library(dplyr)
library(stringr)
library(shiny)

df <- read.csv("unified_dataset.csv")

get_year <- function(df, Name) {
  return(h4(df[df$name==Name,]$year))
}
get_mass <- function(df, Name) {
  return(h4(df[df$name==Name,]$mass..g.))
}
get_location <- function(df, Name) {
  return(h4(df[df$name==Name,]$GeoLocation))
}
get_crush <- function(df, Name) {
  return(h4(df[df$name==Name,]$crush))
}
get_speed <- function(df, Name) {
  return(h4(df[df$name==Name,]$avg_speed))
}
get_class <- function(df, Name) {
  return(h4(df[df$name==Name,]$reclass))
}
get_class_special <- function(word) {
  if(is.null(word)) {
    return(df)
  } else if(word=="") {
    return(df)
  } else {
    ans_word <- str_detect(df$recclass, fixed(word, ignore_case=TRUE))
    return(df[ans_word,])
  }
}

make_names <- function(filt_df) {
  name1 <- ""
  name2 <- ""
  name3 <- ""
  for (i in 1:nrow(filt_df)) {
    col_num <- i %% 3
    if (col_num == 1) {
      class_name <- h3(filt_df[i, ]$class)
      name_text <- HTML(str_replace_all(filt_df[i, ]$name, "\n\n", "<br/>"))
      block <- paste(class_name, name_text, br())
      name1 <- paste(name1, block)
    } else if (col_num == 2) {
      class_name <- h3(filt_df[i, ]$class)
      name_text <- HTML(str_replace_all(filt_df[i, ]$name, "\n\n", "<br/>"))
      block <- paste(class_name, name_text, br())
      name2 <- paste(name2, block)
    } else if (col_num == 0) {
      class_name <- h3(filt_df[i, ]$class)
      name_text <- HTML(str_replace_all(filt_df[i, ]$name, "\n\n", "<br/>"))
      block <- paste(class_name, name_text, br())
      name3 <- paste(name3, block)
    }
  }
  text <- fluidRow(
    column(3, HTML(name1)),
    column(4, offset = 1, HTML(name2)),
    column(4, HTML(name3))
  )
  return(text)
}
