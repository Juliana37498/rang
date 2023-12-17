library(shiny)
library(readr)
library(tm)
library(zip)
library(stringdist)
library(stringi)
library(words)
library(dplyr)

ui <- fluidPage(
  tags$head(
    tags$style(type="text/css"
               , "label{ display: table-cell; text-align: center; vertical-align: middle; }
                  .form-group { display: table-row;}
                  #guess_input { width: 40px; margin-left: 10px; margin-bottom: 10px;}")
  ),
  titlePanel("Customizable Hangman Game"),
  sidebarLayout(
    sidebarPanel(
      selectInput("theme", "Select Theme", c("Default", list.files("resources", pattern = ".zip"))),
      selectInput("wordlist", "Select Wordlist", c("Default", list.files("resources", pattern = "_wordlist.txt"))),
      sliderInput('imagesize','Image Size',100,1000,step=1,value = 500),
      actionButton("start_game", "Start New Game")
    ),
    mainPanel(
      # Display hangman images and game status here
      uiOutput("hangman_image"),
      textOutput("word_to_guess"),
      textOutput("current_guess"),
      textOutput("current_word_state"),
      textOutput("game_status"),
      textInput("guess_input", "Enter a letter guess: ",width = '40px'),
      actionButton("submit_guess", "Submit Guess")
      #tags$br(),
      #tags$a('Image: Creative Commons Attribution-No Derivative Works 3.0 License',href='Creative Commons Attribution-No Derivative Works 3.0 License'),
      #tags$a('by tonkonton',href='https://www.deviantart.com/tonkonton/art/Bomb-Explosion-Gif-187668979')
    )
  )
)

server <- function(input, output, session) {
  # Define reactive values to store game-related information
  game_data <- reactiveValues(
    theme = NULL,
    wordlist = NULL,
    word_to_guess = "",
    current_guess = "",
    hangman_images = character(0),
    game_status = ""
  )

  # Function to load wordlist
  load_wordlist <- function() {
    wordlist_file <- ifelse(game_data$wordlist == "Default", "resources/default_wordlist.txt", paste("resources/", game_data$wordlist, sep = ""))
    wordlist <- readLines(wordlist_file)
    wordlist <- wordlist[sapply(wordlist, nchar) >= 5]  # Filter words with at least 5 characters
    return(sample(wordlist, 1))
  }

  # Function to load hangman images from the selected theme
  load_hangman_images <- function() {
    theme_folder <- ifelse(game_data$theme == "Default", "default", game_data$theme)
    theme_folder_path <- paste("www/images/", theme_folder, sep = "")
    images <- list.files(theme_folder_path, full.names = TRUE) %>% grep('(svg|gif|png|jpg|jpeg)$',.,ignore.case=T,value = T)
    images <- sort(images)  # Sort images lexicographically
    return(gsub('www/','',images))
  }

  # Function to update game status based on current guess
  # Function to initialize the word state with underscores
  initialize_word_state <- function(word_to_guess) {
    underscore_word <- gsub("[a-z]", "_", word_to_guess)
    return(underscore_word)
  }

  # Initialize the game when the "Start New Game" button is clicked
  observeEvent(input$start_game, {
    message('obs00')
    game_data$theme <- input$theme
    game_data$wordlist <- input$wordlist
    game_data$word_to_guess <- load_wordlist()
    game_data$hangman_images <- load_hangman_images()
    game_data$game_status <- ""
    game_data$current_word_state <- initialize_word_state(game_data$word_to_guess)
    message('end obs00')
  })

  # Handle user's guess submission
  observeEvent(input$submit_guess, {
    # Get the user's guess from the text input
    user_guess <- stri_trans_tolower(input$guess_input)

    # Ensure the guess is a single letter
    if(stri_length(user_guess)!=1 || !grepl("^[a-z]$", user_guess)){
      game_data$game_status <- "Invalid input. Try again.";
    } else {
      game_data$game_status <- "";
      if (grepl(user_guess, game_data$word_to_guess)) {
        # Correct guess: Update the current word state with the guessed letter
        word_state <- game_data$current_word_state
        word_to_guess <- game_data$word_to_guess
        positions <- stri_locate_all_regex(word_to_guess, user_guess)[[1]]
        for (ii in seq_len(NROW(positions))) {
          #word_state <- substr_replace(word_state, user_guess, pos[1], pos[2])
          stri_sub(word_state, positions[ii,1], positions[ii,2]) <- user_guess
        }
        message('obs01')
        game_data$current_word_state <- word_state

        # Check if the word has been completely guessed
        if (!grepl("_", word_state)) {
          game_data$game_status <- "You win!"
        }
      } else {
        # Incorrect guess: Decrement the number of remaining hangman images
        game_data$hangman_images <- game_data$hangman_images[-1]

        if (length(game_data$hangman_images) <= 1) {
          game_data$game_status <- "You lose!"
        }
      }
    };
    updateTextInput(inputId='guess_input',value='');
    message('end obs01')
  })

  output$word_to_guess <- renderText({
    game_data$word_to_guess
  })

  output$current_word_state <- renderText({
    game_data$current_word_state
  })
  output$current_guess <- renderText({
    game_data$current_guess
  })

  output$game_status <- renderText({
    game_data$game_status
  })

  output$hangman_image <- renderUI({
    if (length(game_data$hangman_images) > 0) {
      img_path <- game_data$hangman_images[1]
      tags$img(src = img_path, width = paste0(input$imagesize,'px'))
    } else {
      tags$p("Game over!")
    }
  })
}

shinyApp(ui, server)
