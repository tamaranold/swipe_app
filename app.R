# Survey Template for swiable answers

# load packages
library(shiny)
library(shinyMobile)
library(shinyswipr)
library(tidyverse)

# load data
data <- read.csv2("data/items.csv",
                  sep = ",")

# output
data.output <- data %>%
  add_column(answer = NA)

# App
shinyApp(
  # App style
  ui = f7Page(
    title = data$app_title[1],
    f7SingleLayout(
      navbar = f7Navbar(
        title = data$app_title[1],
        hairline = TRUE
      ),
      # main content
      # answer
      f7Card(f7Row(
        f7Col(f7Icon("envelope", color = "white"),
              data$answer_left[1]),
        f7Align(f7Col(
          data$answer_right[1],
          f7Icon("envelope", color = "white")
        ), side = "right")
      )),
      
      # question
      shinyswiprUI(
        "my_swiper",
        f7Card(title = data$question_title[1],
               data$question_body[1])
      )
    )
  ),
  
  # process
  server = function(input, output) {
    
    # make Card swipeable
    card_swipe <- callModule(shinyswipr, "my_swiper")
    
    # print swipe direction
    observeEvent(card_swipe(), {
      print(card_swipe()) #show last swipe result.
    })
  }
)
