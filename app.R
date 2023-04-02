# Survey Template for swiable answers

# load packages
library(shiny)
library(shinyMobile)
library(shinyswipr)
library(tidyverse)

# load data
data <- read.csv2("data/items.csv",
                  sep = ",") %>%
  add_column(answer = NA)

# max questions
row_max <- nrow(data)

# App
shinyApp(
  # App style
  ui = f7Page(
    title = data$app_title[1],
    f7SingleLayout(
      navbar = f7Navbar(title = data$app_title[1],
                        hairline = TRUE),
      # main content
      # answer
      uiOutput("swipeAnswer"),
      
      # question
      shinyswiprUI("my_swiper",
                   uiOutput("swipeCard"))
      
    )
  ),
  
  # process
  server = function(input, output) {
    # make Card swipeable
    card_swipe <- callModule(shinyswipr, "my_swiper")
    
    # first question
    row_counter <- reactiveVal(1)
    
    output$swipeAnswer <- renderUI({
      f7Card(f7Row(
        f7Col(f7Icon("envelope", color = "white"),
              data$answer_left[1]),
        f7Align(f7Col(
          data$answer_right[1],
          f7Icon("envelope", color = "white")
        ), side = "right")
      ))
    })
    
    output$swipeCard <- renderUI ({
      f7Card(title = data$question_title[1],
             data$question_body[1],
             image = paste0("www/", data$question_image[1]))
    })
    
    
    # row counter and new questions
    observeEvent(card_swipe(), {
      
      if(row_counter() <= row_max){
        data$answer[row_counter()] <<- card_swipe()
      }
      
      new_row <- row_counter() + 1
      row_counter(new_row)
      
      print(row_counter())
      print(row_max)
      print(row_counter() == (row_max + 1))
      
      if (row_counter() <= row_max) {
        # change question after swipe
        output$swipeAnswer <- renderUI({
          f7Card(f7Row(
            f7Col(f7Icon("envelope", color = "white"),
                  data$answer_left[row_counter()]),
            f7Align(f7Col(
              data$answer_right[row_counter()],
              f7Icon("envelope", color = "white")
            ), side = "right")
          ))
        })
        
       output$swipeCard <- renderUI ({
          f7Card(title = data$question_title[row_counter()],
                 data$question_body[row_counter()],
                 #!!!
                 image = paste0("www/", data$question_image[row_counter()]))
          
        })
        
      } else {
        output$swipeAnswer <- renderUI ({
          f7Card()
        })
        
        output$swipeCard <- renderUI ({
          f7Card(title = "The End.",
                 "Thank you for taking part in this survey. You can now close the browser.")
        })
        
      }
      
      observeEvent(card_swipe(), {
        req(row_counter() == (row_max + 1))
        write.csv2(data, 
                               paste0("output/",
                                      format(Sys.time(), "%Y-%b-%d_%H-%M-%S_"),
                                      sample(10000:99999,1),
                                      ".csv"))
      }, once = TRUE)
        
        
    })

    
    
  }
)
