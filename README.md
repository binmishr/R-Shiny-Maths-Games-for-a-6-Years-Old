# R-Shiny-Maths-Games-for-a-6-Years-Old

There are two core components within the server.R script:

    get_random_question(): randomly select numbers from a range and operations (+, -, x); calculate the true answer to the random question.
    verdict(): check user’s input against the true answer. If correct (wrong), randomly pick a GIF from the happy (angry) list. Clearly the GIF files have to be fun to keep the players engaged.


#ui.R#
library(shiny)
library(shinyjs)
shinyUI(fluidPage(
    useShinyjs(),
    tags$style(“#players_answer {font-size:30px;height:30px;}”),
    titlePanel(“Pokemon Maths Challenge!”),
    fluidRow(
        HTML(“
“),
        imageOutput(“duck_image”),
        HTML(“
“)
    ),
    fluidRow(
        sidebarLayout(
            sidebarPanel(
                numericInput(‘players_answer’, ‘Answer’, value = 0),
                actionButton(‘submit_answer’,’Submit’),
                hr(),
                actionButton(‘update’, ‘Next Question’)
            ),
            mainPanel(
                span(textOutput(“illustration”), style = “font-size: 36px”),
                imageOutput(“check_answer”),
                span(textOutput(“check_answer_text”), style = “font-size: 20px”)
            )
        )
    )
))

#server.R#
library(shiny)

shinyServer(function(input, output) {
    first_number <- sample(10:50, 1)
    second_number <- sample(3:(first_number-1),1)
    get_random_question <- reactive({
        input$update
        isolate({
            first_number <- sample(10:50, 1)
            operation <- sample(1:3, 1, prob = c(0.5,0.2,0.3))
            operation_symb <- c("-", "+", "x")
            if(operation == 1){
                #1 is subtraction
                second_number <- sample(3:(first_number-1),1)
                true_answer <- first_number - second_number
            }
            if(operation == 2){
                #2 is addition
                second_number <- sample(5:50,1)
                true_answer <- first_number + second_number
            }
            if(operation==3){
                #3 is multiplication
                second_number <- sample(1:3,1, prob = c(0.1,0.45,0.45))
                true_answer <- first_number * second_number
            }
            question_out <- list(
                “formula” = paste(first_number,
                                  operation_symb[operation],
                                  second_number,
                                  “= ?”),
                “true_answer” = true_answer
            )
            question_out
        })
    })
    output$illustration <- renderText({
        get_random_question()$formula
    })
    verdict <- reactive({
        input$submit_answer
        isolate({
            if(input$players_answer == get_random_question()$true_answer){
                happyimagelist <- c("GIF/PikachuHappy1.gif",
                                    “GIF/PikachuHappy2.gif”,
                                    “GIF/PikachuHappy3.gif”,
                                    “GIF/PikachuHappy4.gif”)
                image2show <- sample(happyimagelist,1)
            }else{
                image2show <- sample(c("GIF/TeamRocket1.gif",
                                       “GIF/TeamRocket2.gif”),1)
            }
            image2show
        })})
    verdict_text <- reactive({
        input$submit_answer
        isolate({
            if(input$players_answer == get_random_question()$true_answer){
                text2show <- "Correct!"
            }else{
                text2show <- "Please try again!"
            }
            text2show
        })})
    observeEvent(
        input$submit_answer,{
        output$check_answer <- renderImage({
            list(src=”GIF/waiting.gif”, alt = “waiting”)}, deleteFile = FALSE)
        output$check_answer_text <- renderText("hm...")
        delay(2500, 
              {output$check_answer <- renderImage({list(src=verdict(), alt="verdict")}, 
                                                 deleteFile = FALSE)
              output$check_answer_text <- renderText(verdict_text())})}
    )
    output$duck_image <- renderImage({
        list(src=”GIF/DuckQuestionMark.gif”, alt = “PsyDuck”)
    }, deleteFile = FALSE)
})

