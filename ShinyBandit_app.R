# ------------------
#  BANDIT TASK
#  Implimented in Shiny as ShinyBandit by Nathaniel D. Phillips
# 
#  http://ndphillips.github.io, Nathaniel.D.Phillips.is@gmail.com
#
#   CODE SECTIONS
#
#   0: Load libraries
#   A: Setup Game
#     A1: Game parameters
#     A2: Data saving
#     A3: Reformat data
#     A4: Miscellaneous code
#   B: Overall layout
#   C: Reactive values
#   D: Page layouts
#   E: Game display
#     E1: Box and tickets
#     E2: Main game display
#   F: Event (button) actions
#     F1: Option selection buttons
#     F2: Page navigation buttons
#     F3: Event tracking
#   G: Save data
# ------------------



# --------------------------
# Section 0: Load libraries
# --------------------------

library(shiny)
library(rdrop2)
library(sendmailR)
library(digest)
library(yarrr)

# --------------------------
# Section A: Setup game
# --------------------------

# Section A1: GAME PARAMETERS

distributions <- round(
  cbind(rnorm(n = 1e5, mean = 0, sd = 5),
        rnorm(n = 1e5, mean = 2.5, sd = 5),
        rnorm(n = 1e5, mean = -1, sd = 8)), 0)

trials.n <- 25                          # Trials per game
practice.n <- 10                        # Trials in practice game
games.n <- 3                            # Number of games
randomize.locations <- TRUE             # Should the location of options be randomized?
randomize.outcomes <- TRUE              # Should the order of outcomes be randomized?
saveDataLocation <- "dropbox"           # Either dropbox, email, or local
outputDir <- "nphillips/Bandit_A/data"  # Directory to save data

# Section A2: DATA SAVING
if(saveDataLocation == "dropbox") {
  
  droptoken <- readRDS("droptoken.rds")        # Reads in authentication for dropbox
  
}
if(saveDataLocation == "email") {  # not working yet!
  
  # See https://goo.gl/kQLrTk for guide
  
  from <- "nathaniel.phillips@unibas.ch"
  to <- "nathaneil.phillips@unibas.ch"
  subject <- "Email Subject"
  body <- "Email body."       
  mailControl <- list(smtpServer = "serverinfo")
  
}
if(saveDataLocation == "local") {   # not working yet!
  
}

# Section A3: Reformat data
options.n <- ncol(distributions)   # Number of options
outcomes.ls <- lapply(1:games.n, FUN = function(x) {return(distributions)})
if(randomize.locations) {
  
  locations.r <- lapply(1:games.n, FUN = function(x) {sample(options.n)})
  
  for(game.i in 1:games.n) {
    
    locations.i <- locations.r[[game.i]]
    
    outcomes.ls[[game.i]] <- outcomes.ls[[game.i]][,locations.i]
  }
}
if(randomize.outcomes) {
  
  for(game.i in 1:games.n) {
    
    outcomes.ls[[game.i]] <- outcomes.ls[[game.i]][sample(nrow(distributions)),]
  }
}

# Section A4: Miscellaneous code

# Calculate study completion code
completion.code <- paste("EP-BANDIT", sample(100:999, size = 1), 
                         sample(100:999, size = 1),
                         sample(100:999, size = 1), sep = "-")

# --------------------------------
# Section B: Define overall layout
# --------------------------------

ui <- fixedPage(
  
  title = "ShinyBandit",
  uiOutput("MainAction"),
  tags$style(type = "text/css", ".recalculating {opacity: 1.0;}")   # Prevents gray screen during Sys.sleep()
  
)

server <- function(input, output, session) {
  
  output$MainAction <- renderUI( {
    PageLayouts()
  
})

# --------------------------------
# Section C: Define Reactive Values
#   These store the main values in the game
# --------------------------------

# CurrentValues stores scalers representing the latest game outcomes
CurrentValues <- reactiveValues(page = "welcome",     # Current page
                                game = 0,             # Current game
                                trial = 1,            # Current trial
                                selection = 0,        # Selection
                                outcome = 0,          # Outcome
                                points.cum = 0)       # Points earned

# GameValues stores vectors of histories
GameData <- reactiveValues(game = c(),
                           trial = c(),          
                           time = c(),
                           selection = c(),
                           outcome = c(),
                           points.cum = c())
  
  # --------------------------------
  # Section D: Page Layouts
  # --------------------------------
  
  PageLayouts <- reactive({
    
    # P1) Welcome
    if (CurrentValues$page == "welcome") {
      
      return(
        list(
          h2("Decision Making"),
          p("The purpose of this study is to understand how people learn and make decisions."),
          p("This study will take place in two phases. In Phase 1, you will play a game called „The Boxes Game.“ In the boxes game, you will learn about risky options and make decisions. In Phase 2, you will complete a few short questionnnaires about the game and how you make decisions in general."),
          p("There are no health risks or personal identifying information associated with your participation."),
          p("This study is founded by the chair of the department of Economic Psychology at the University of Basel."),
          p("Your responses will be anonymous and in a group and your individual responses will not be published."),
          p("If you consent to participating in this study, please enter a unique ID that no one else would use and click Continue."),
          textInput(inputId = "workerid", 
                    label = "Please enter a unique ID that no one else would use", 
                    value = "", 
                    placeholder = "e.g.; Cat57Door"),
          # This displays the action putton Next.
          actionButton(inputId = "gt_instructions", 
                       label = "Continue") 
        )
      )}
    
    # P2) Instructions
    if (CurrentValues$page == "instructions") {
      
      return(
        list(
          h2("The Boxes Game"),
          p(paste("On the next page you will play The Boxes Game. Your goal of The Boxes Game is to earn as many points as you can by drawing te best tickets you can from the boxes.")),
          h3("Here is how the game will look:"),
          plotOutput('GameScreenShot'),
          p(paste("As you can see, the game contains", options.n, "boxes that contain many tickets. Each ticket has a hidden point value written on it. The point values can range from -100 to +100. Tickets with negative values will decrease your point total, while those with positive values will add to it.")),
          h3("Selecting boxes"),
          p("Each box contains many tickets with different point values. However, some boxes have better point values on average than others. To do well in the game, you need to try to figure out which box(es) have the best point values."),
          p(paste("You will have", trials.n, "trials to select boxes. When you select a box, the computer will randomly draw one of its tickets and show it to you. If the outcome is positive, it will be added to your point total. If it is negative, it will be subtracted from your point total. After you see a ticket from a box, it will be put back into the box. When you have 0 trials remaining, the game will end.")),
          h3("Additional information"),
          tags$ul(
            tags$li("When you select a Box, the computer will always randomly draw a ticket from the box and will always return that ticket back to the box. In other words, the contents of each box will not change over time as a result of your selections."),
            tags$li("At the top of the screen you will always see two important pieces of information: The number of trials you have remaining in the game, and the total number of points you have earned so far in the game.")
          ),
          h3(paste(games.n, "Games")),
          p(paste("You will play a total of", games.n, "games. Each game will contain the same 3 boxes. However, the position of the boxes will be randomly shuffled! This means that the boxes might be in the same locations, or in different locations,")),
          p("Your goal is to earn as many points as possible across all games. Your final point total will be the sum of the points you earn across all games. This means that if you end a game with negative points, they will be subracted from your final point total."),
          
          actionButton(inputId = "gt_startnextgame", 
                       label = "Continue") 
        )
      )}
    
    # P3) Start next game
    if (CurrentValues$page == "startnextgame") {
      
      return(
        list(
          h2(paste("Game", CurrentValues$game, "of", games.n)),
          p("Reminder: The boxes are the same in each game. However, the location of each box (that is, which box is A, B, or C) will randomly determined at the start of each game."),
          p("Click continue to start the next game"),
          actionButton(inputId = "gt_game", 
                       label = paste("Start Game", CurrentValues$game)) 
        )
      )}
    
    # P4) Game
    if (CurrentValues$page == "game") {
      
      return(
        list(
          # Main Display
          fixedRow(
            column(12, 
                   plotOutput('GameDisplay'),
                   # Buttons
                   fixedRow(
                     column(4, actionButton("SelectA", label = "Select Box A")),
                     column(4, actionButton("SelectB", label = "Select Box B")),
                     column(4, actionButton("SelectC", label = "Select Box C"))
                   )))
        )
      )
    }
    
    # P5) Game end
    if (CurrentValues$page == "gameend") {
      
      return(list(h3("You finished the game!"),
                  p(paste("You earned", CurrentValues$points.cum, "points in game", CurrentValues$game)),
                  actionButton(inputId = "gameend", 
                               label = "Continue")))
    }
    
    # P6) All games end page
    if (CurrentValues$page == "allgameend") {
      
      return(list(h3("You finished all games!"),
                  actionButton(inputId = "gt_postsurvey", 
                               label = "Continue")))
    }
    
    # P7) Survey
    if (CurrentValues$page == "postsurvey") {
      
      return(list(
        h3("Survey (1 of 1)"),
        
        radioButtons("sex",
                     label = "What is your sex?",
                     choices = list("Male" = 1, 
                                    "Female" = 2, 
                                    "Other" = 3),
                     selected = 99),
        
        numericInput("age", 
                     label = "What is your age?",
                     value = NA),
        
        radioButtons("interesting", 
                     label = "How interesting did you find the Boxes Game?",
                     choices = c("1 - Not at all" =  1,
                                 "2" = 2,
                                 "3" = 3,
                                 "4" = 4,
                                 "5 - Very Much" = 5), 
                     selected = 99),
        
        textAreaInput(inputId = "strategy",
                      label = "What was your strategy in the Boxes Game?",
                      placeholder = "I tried to...", 
                      resize = "both",
                      value = ""),
        
        radioButtons("dontuse",
                     label = "Is there any reason why we should NOT use your data for scientific research? For example, were you not paying attention or were you intoxicated?",
                     choices = c("No. My data should be valid for scientific research" =  "0",
                                 "Yes. There is a good reason why you should NOT use my data for scientific research" = 1), 
                     selected = 99),
        
        radioButtons("playedbefore",
                     label = "Have you played a game similar to the Boxes game in the past?",
                     choices = c("No. I have not played a game similar to the Boxes game in the past" = 0,
                                 "Yes. I have played a game very similar to the Boes game in the past" = 1,
                                 "I am not sure" = 2), 
                     selected = 99),
        
        textAreaInput("comments",
                      label = "If you have any additional comments, please enter them below",
                      resize = "both", 
                      value = ""),
        
        actionButton(inputId = "gt_goodbye",
                     label = "Save Data and End Study"))
      )
    }
    
    # P8) Goodbye
    if (CurrentValues$page == "goodbye") {
      
      return(list(
        h3("Thank you for your participation!"),
        p("Here is your study completion code. Please write it down as a confirmation of your participation"),
        h2(completion.code),
        h3("What was this research about?"),
        p("The purpose of this research is to try and understand how people learn and make decisions about risky options. In this task, we're interested in how well you learn to select the best options over time. In the plot below, you can see how many points you actually earned in the game(s) over time."),
        plotOutput('EarningsPlot'),
        p("If you have any questions about this study, you may contact us at EconPsychBasel@gmail.com. If you do contact us, please include your study completion code."),
        p("Once you have recorded your code, you may close this window"),
        tags$img(src = "thankyou.jpg", width = "300px")
      ))
    }
    
  })
  
  # --------------------------------
  # Section E: Game Display
  # --------------------------------
  
  # Section E1: Box and Ticket graphical parameters
  
  #   Boxes
  box.center.x <- seq(.1, .9, length.out = options.n)
  box.center.y <- rep(.5, options.n)
  box.height <- .75
  box.width <- 1 / (options.n + 1)
  
  box.x0.v <- box.center.x - box.width / 2
  box.y0.v <- box.center.y - box.height / 2
  box.x1.v <- box.center.x + box.width / 2
  box.y1.v <- box.center.y + box.height / 2
  
  #   Tickets
  ticket.n <- 8
  ticket.width <- box.width / 15
  ticket.height <- box.height / 15
  ticket.col <- gray(.9)
  
  ticket.x.v <- as.numeric(sapply(1:options.n, FUN = function(x) {
    
    rep(seq(from = box.center.x[x] - box.width / 3,
            to = box.center.x[x] + box.width / 3,
            length.out = ticket.n), each = ticket.n)
    
  }))
  
  ticket.y.v <- as.numeric(sapply(1:options.n, FUN = function(x) {
    
    rep(seq(from = box.center.y[x] - box.height / 3,
            to = box.center.y[x] + box.height / 3,
            length.out = ticket.n), times = ticket.n)
    
  }))
  
  ticket.x0.v <- ticket.x.v - ticket.width / 2
  ticket.x1.v <- ticket.x.v + ticket.width / 2
  ticket.y0.v <- ticket.y.v - ticket.height / 2
  ticket.y1.v <- ticket.y.v + ticket.height / 2
  
  # Section E2: Main game display
  
  bandit.display <- function(outcome,                # What is the outcome on this trial?
                             selection,              # Which option is selected?
                             points,                 # How many points have been earned?
                             trials.left,            # How many trials remain?
                             pos.col = "green3",     # Color of positive outcomes
                             neg.col = "indianred1", # Color of negative outcomes
                             neu.col = "gray") {     # Color of neutral outcomes
    
    # Determine current outcome and display
    if(is.numeric(outcome)) {
      
      if(outcome > 0) {
        text.col <- pos.col
        current.outcome.disp <- paste0("+", outcome)
      }
      
      if(outcome < 0) {
        text.col <- neg.col
        current.outcome.disp <- outcome
      }
      
      if(outcome == 0) {
        text.col <- neu.col
        current.outcome.disp <- outcome
      }
    }
    
    if(is.numeric(outcome) == FALSE) {
      
      text.col <- "black"
      current.outcome.disp <- outcome
      
    }
    
    # Plotting space
    par(mar = c(1, 1, 1, 1))
    layout(matrix(c(1, 2), nrow = 2, ncol = 1, byrow = TRUE), heights = c(3, 3), widths = c(9))
    
    # Title row
    plot.new()
    text(c(.5, .85), c(.7, .7), labels = c("Points Earned", "Draws Remaining"), cex = 2, font = 3)
    text(c(.5, .85), c(.4, .4), labels = c(points, trials.left), cex = 4)
    abline(h = .2, col = gray(.5), lwd = 2)
    
    # Option display
    
    plot(1, xlim = c(0, 1), ylim = c(0, 1), type = "n", 
         xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
    
    # Options
    rect(xleft = box.x0.v, ybottom = box.y0.v,
         xright = box.x1.v, ytop = box.y1.v, 
         lwd = 2)
    
    # Tickets
    rect(xleft = ticket.x0.v,
         ybottom = ticket.y0.v,
         xright = ticket.x1.v,
         ytop = ticket.y1.v,
         col = ticket.col, lwd = .5, 
         border = gray(.3))
    
    # Option Labels
    mtext(LETTERS[1:options.n], side = 3, cex = 3, at = box.center.x)
    
    # Outcome display
    if(is.finite(selection)) {
      
      # Outcome border
      rect(xleft = box.center.x[selection] - box.width * .4,
           ybottom = box.center.y[selection] - box.height * .4,
           xright = box.center.x[selection] + box.width * .4,
           ytop = box.center.y[selection] + box.height * .4,
           col = "white"
      )
      # Outcome Display
      text(x = box.center.x[selection], y = .5, 
           current.outcome.disp, cex = 7, col = text.col)
      
    }
    
  }
  
  
  # Game Screenshot (used in instructions)
  output$GameScreenShot <- renderPlot({
    
    bandit.display(outcome = "",
                   selection = NA,
                   points = 0,
                   trials.left = trials.n)
    
  })
  
  # Actual game display
  output$GameDisplay <- renderPlot({
    
    bandit.display(outcome = CurrentValues$outcome,
                   selection = CurrentValues$selection,
                   points = CurrentValues$points.cum,
                   trials.left = trials.n - CurrentValues$trial + 1)
  })
  
  # --------------------------------
  # Section F: Event (e.g.; button) actions
  # --------------------------------
  
  
  # Section F1: Page Navigation Buttons
  observeEvent(input$gt_instructions, {CurrentValues$page <- "instructions"})
  observeEvent(input$gt_gameend, {CurrentValues$page <- "gameend"})
  observeEvent(input$gt_game, {CurrentValues$page <- "game"})
  observeEvent(input$gt_postsurvey, {CurrentValues$page <- "postsurvey"})
  observeEvent(input$gt_goodbye, {CurrentValues$page <- "goodbye"})
  observeEvent(input$gt_startnextgame, {CurrentValues$page <- "startnextgame"})
  observeEvent(input$gameend, {
    
    if(CurrentValues$game != games.n) {
      
      CurrentValues$game <- CurrentValues$game + 1
      CurrentValues$trial <- 1
      CurrentValues$selection <- 0
      CurrentValues$points.cum <- 0
      CurrentValues$page <- "startnextgame"
      
    } else {
      
      CurrentValues$page <- "allgameend"}
    
  })
  
  # Section F2: Option selection buttons
  selection.update <- function(selection.i,
                               trial.i,
                               game.i) {
    
    if(trial.i <= trials.n) {
      
      outcome.i <- outcomes.ls[[game.i]][trial.i, selection.i]
      time.i <- proc.time()[3]
      
      # Update current values
      CurrentValues$selection <<- selection.i
      CurrentValues$outcome <<- outcome.i   
      CurrentValues$points.cum <<- CurrentValues$points.cum + outcome.i
      CurrentValues$time <<- time.i
      CurrentValues$game <<- game.i
      
      
      # Update GameData
      GameData$game <<- c(GameData$game, game.i)
      GameData$trial <<- c(GameData$trial, trial.i)
      GameData$time <<- c(GameData$time, time.i)
      GameData$selection <<- c(GameData$selection, selection.i)
      GameData$outcome <<- c(GameData$outcome, outcome.i)
      GameData$points.cum <<- c(GameData$points.cum, CurrentValues$points.cum)
      
    }
    
    CurrentValues$trial <<- CurrentValues$trial + 1
  }
  
observeEvent(input$SelectA, {selection.update(1, CurrentValues$trial, CurrentValues$game)})
observeEvent(input$SelectB, {selection.update(2, CurrentValues$trial, CurrentValues$game)})
observeEvent(input$SelectC, {selection.update(3, CurrentValues$trial, CurrentValues$game)})



# Section F3: Event tracking buttons

# Reset current values at start of a game
observeEvent(input$gt_startnextgame, {
  
  # Increase game number by 1
  CurrentValues$game <- CurrentValues$game + 1
  
  # Set CurrentValues to defaults for start of next game
  CurrentValues$trial <- 1
  CurrentValues$selection <- 0
  CurrentValues$points.cum <- 0
  
  
})

# Watch for last trial in a game
observeEvent(CurrentValues$trial, {
  
  if(CurrentValues$trial == (trials.n + 1)) {
    
    CurrentValues$page <- "gameend"
    
  }
  
})

# --------------------------------
# Section G: Save data
# --------------------------------
observeEvent(input$gt_goodbye, {(
  
  # Create progress message   
  withProgress(message = "Saving data...", value = 0, {
    
    incProgress(.25)
    
    response.time.v <- c(NA, round(GameData$time[2:(trials.n)] - GameData$time[1:(trials.n - 1)], 4))
    
    # Write GameData to a dataframe
    

    
    GameData.i <- data.frame("game" = GameData$game,
                             "trial" = GameData$trial,
                             "time" = GameData$time,
                             "selection" = GameData$selection,
                             "outcome" = GameData$outcome,
                             "points.cum" = GameData$points.cum)
    
    GameDatafileName <- paste0(input$workerid, as.integer(Sys.time()), digest::digest(GameData.i), "_g.csv")
    
    # Write Survey data
    if(length(input$workerid) == 0) {workerid.i <- NA} else {workerid.i <- input$workerid}
    if(length(input$comments) == 0) {comments.i <- NA} else {comments.i <- input$comments}
    if(length(input$age) == 0) {age.i <- NA} else {age.i <- input$age}
    if(length(input$sex) == 0) {sex.i <- NA} else {sex.i <- input$sex}
    if(length(input$dontuse) == 0) {dontuse.i <- NA} else {dontuse.i <- input$dontuse}
    if(length(input$interesting) == 0) {interesting.i <- NA} else {interesting.i <- input$interesting}
    if(length(input$playedbefore) == 0) {playedbefore.i <- NA} else {playedbefore.i <- input$playedbefore}

    SurveyData.i <- data.frame("workerid" = workerid.i,
                               "age" = age.i,
                               "sex" = sex.i,
                               "comments" = comments.i,
                               "dontuse" = dontuse.i,
                               "interesting" = interesting.i,
                               "playedbefore" = playedbefore.i,
                               "option.order" = paste(locations.r, collapse = ";"),
                               "completion.code" = completion.code)
    

    SurveyDatafileName <- paste0(input$workerid,
                                 as.integer(Sys.time()),
                                 digest::digest(SurveyData.i), "_s.csv")
    
    incProgress(.5)
    
    
    if(saveDataLocation == "dropbox") {
      
      GameDatafilePath <- file.path(tempdir(), GameDatafileName)
      write.csv(GameData.i, GameDatafilePath, row.names = FALSE, quote = TRUE)
      rdrop2::drop_upload(GameDatafilePath, dest = outputDir, dtoken = droptoken)
      
      SurveyDatafilePath <- file.path(tempdir(), SurveyDatafileName)
      write.csv(SurveyData.i, SurveyDatafilePath, row.names = FALSE, quote = TRUE)
      rdrop2::drop_upload(SurveyDatafilePath, dest = outputDir, dtoken = droptoken)
      
    }
    
    # if(saveDataLocation == "email") {
    #   
    #   GameDatafilePath <- file.path(tempdir(), GameDatafileName)
    #   write.csv(GameData.i, GameDatafilePath, row.names = FALSE, quote = TRUE)
    # 
    #   SurveyDatafilePath <- file.path(tempdir(), SurveyDatafileName)
    #   write.csv(SurveyData.i, SurveyDatafilePath, row.names = FALSE, quote = TRUE)
    # 
    #   attachmentObject <- mime_part(x = "GameDatafilePath", name = GameDatafileName)
    #   attachmentObject2 <- mime_part(x = "SurveyDatafilePath", name = SurveyDatafileName)
    #   bodyWithAttachment <- list(body, attachmentObject, attachmentObject2)
    #   
    #   sendmail(from = from, to = to, subject = subject, msg = bodyWithAttachment, control = mailControl)
    #   
    # }
    
    
    # Write survey data 
    
    
    incProgress(.75)
    
    # Some interesting plots (not necessary)
    
    output$GameData.tbl <- renderTable(head(GameData.i))            
    output$EarningsPlot <- renderPlot({
      
      my.cols <- yarrr::piratepal("xmen", trans = .3)
      
      plot(1, xlim = c(0, trials.n), ylim = c(min(GameData.i$points.cum), max(GameData.i$points.cum)), 
           xlab = "Draw", ylab = "Points earned", main = "Your Earnings over time in each Game", type = "n")
      
      grid()
      
      for(game.i in 1:games.n) {
        
        lines(x = subset(GameData.i, game == game.i)$trial,
              y = subset(GameData.i, game == game.i)$points.cum,
              type = "l", col = my.cols[game.i], lwd = 3)
        
      }
      
      legend("topleft", legend = paste("Game", 1:games.n), 
             col = my.cols, lty = 1, lwd = 2, bty = "n")
      
    }) 
    
    Sys.sleep(.25)
    incProgress(1)
    
    
    
  })
  
)})
  
}

# Create app!
shinyApp(ui = ui, server = server)