library(shiny)
library(readr)
library(tm)
library(zip)
library(stringdist)
library(stringi)
library(words)
library(dplyr)
#library(shinyjs)

letter_decorator <- "<span class='%s'>%s</span>";

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  tags$head(
    tags$style(type="text/css"
               , "
               label{ display: table-cell; text-align: center; vertical-align: middle; }
               .form-group { display: table-row;}
               #guess_input { width: 40px; margin-left: 10px; margin-bottom: 10px;}
               .selectize-input { width: 120% }
               .rang_correct { font-size: 1.5em; color: green; font-weight: extra-bold;}
               .rang_wrong { font-size: 1.5em; color: red; font-weight: extra-bold;}
               ")
  ),
  titlePanel("Customizable Hangman Game"),
  sidebarLayout(
    sidebarPanel(
      selectInput("theme", "Select Theme", c("Default", list.files("resources", pattern = ".zip"))),
      selectInput("wordlist", "Select Wordlist",
                  list.files('resources',pattern='_wordlist.txt',ignore.case = T) %>%
                    setNames(.,gsub('^[0-9]{2}_|_wordlist.txt','',.,ignore.case=T))),
      sliderInput('imagesize','Image Size',10,100,step=1,value = 80),
      actionButton("start_game", "Start New Game"),
      if(file.exists('.debug')) actionButton('debug','Debug') else ''
    ),
    mainPanel(
      # Display hangman images and game status here
      uiOutput("hangman_image"),
      textOutput("word_to_guess"),
      textOutput("current_guess"),
      textOutput("current_word_state"),
      uiOutput('letters_used'),
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
  runjs('document.getElementById("guess_input").setAttribute("maxlength", "1");')
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
    game_data$correct_letters <- ''
    game_data$wrong_letters <- ''
    message('end obs00')
  })

  # Handle user's guess submission
  observeEvent(input$submit_guess, {
    # Get the user's guess from the text input
    user_guess <- stri_trans_tolower(input$guess_input)

    # Ensure the guess is a single letter
    if(stri_length(user_guess)!=1 || !grepl("^[a-z]$", user_guess) ||
       user_guess %in% c(game_data$correct_letters,game_data$wrong_letters)){
      game_data$game_status <- "Invalid input. Try again.";
    } else {
      game_data$game_status <- "";
      if (grepl(user_guess, game_data$word_to_guess)) {
        # Correct guess: Update the current word state with the guessed letter
        game_data$correct_letters <- c(game_data$correct_letters,user_guess);
        word_state <- game_data$current_word_state
        word_to_guess <- game_data$word_to_guess
        positions <- stri_locate_all_fixed(word_to_guess, user_guess)[[1]]
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
        game_data$wrong_letters <- c(game_data$wrong_letters,user_guess);
        game_data$hangman_images <- game_data$hangman_images[-1]

        if (length(game_data$hangman_images) <= 1) {
          game_data$game_status <- "You lose!"
        }
      }
    };
    updateTextInput(inputId='guess_input',value='');
    message('end obs01')
  })

  # Use shinyjs to add an event listener for the Enter key
  # onevent("keyup", "guess_input", {
  #   if (isolate(input$keyboard_key_code) == 13) {
  #     browser()
  #     click("submit_guess")
  #   }
  # })

  output$letters_used <- renderUI(case_match(letters
                                             ,game_data$correct_letters ~ sprintf(letter_decorator,'rang_correct',letters)
                                             ,game_data$wrong_letters ~ sprintf(letter_decorator,'rang_wrong',letters)
                                             ,.default = letters) %>% HTML );

  output$word_to_guess <- renderText({
    if(file.exists('.debug')) game_data$word_to_guess
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
      tags$img(src = img_path, width = paste0(input$imagesize,'%'))
    } else {
      tags$p("Game over!")
    }
  })

  observe({req(input$debug); browser()});
}

shinyApp(ui, server)
