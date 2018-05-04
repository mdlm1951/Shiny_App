#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Shiny App for simulating random sampling from different populations

library(shiny)

## Create sample populations
target_mean <- 10  ## We want all of the populations to have the target mean
target_sd <- 2  ## Use this target SD for those populations who have SD as a parameter input
## Normal Population
pop_norm <- rnorm(100000, target_mean, target_sd)
## Uniform Population
pop_unif <- runif(100000, 0, (2 * target_mean))
## Chi Squared Population
pop_chix <- rchisq(100000, target_mean)
## Exponential Population
pop_exp <- rexp(100000, 1/target_mean)
## Lognormal Populatin
location <- log(target_mean^2 / sqrt(target_mean^2 + target_sd^2))
shape <- sqrt(log(1 +(target_sd^2 / target_mean^2)))
pop_lnorm <- rlnorm(100000, location, shape)

## Calculate and store the mean of each populaton
mean_pop_norm <- mean(pop_norm)
mean_pop_unif <- mean(pop_unif)
mean_pop_chix <- mean(pop_chix)
mean_pop_exp <- mean(pop_exp)
mean_pop_lnorm <- mean(pop_lnorm)

## Calculate and store the Standard Deviation of each population
sd_pop_norm <- sd(pop_norm)
sd_pop_unif <- sd(pop_unif)
sd_pop_chix <- sd(pop_chix)
sd_pop_exp <- sd(pop_exp)
sd_pop_lnorm <- sd(pop_lnorm)

## Create a function to implement the random sampling experiment
## sample_distribution generates a sample of size = samp_size from an
## underyling population distribution represneted by the populaton[] vector
## the mean of the sample is calculated and stored in the Results[] vector
## This process is repeated a specified number of times = num_samps
## The function returns the Results[] vector which holds the means of all of the
## generated samples

sample_distribution <- function(population, samp_size, num_samps) {
  results <- numeric(num_samps)
  for (i in 1:num_samps) {
    samp <- sample(population, size = samp_size, replace = FALSE)
    samp_mean <- mean(samp)
    results[i] <- samp_mean
  }
  
  return(results)  
  
}  ## End Function

#################################################################################
# The Shiny App begins now
################################################################################

###############################################################################
## Create the Shiny User Interface
###############################################################################

ui <- navbarPage(title = tags$strong(tags$em("Exploring the Central Limit Theorem")),  
   
  tabPanel("About", 
           p("This site is designed to help students new to statistics gain an intuitive understanding
             of the action of the Central Limit Theorem.  We do this by providing an interactive simulation
             of a random sampling experiment."),
           p("The simulation is provided in the ", (tags$strong(tags$em("Sampling Distributions"))), " tab."),
           p("The student can select from a
             choice of underlying population distributions.  The student then designs a sampling experiment
             by seleting the size of the sample to be drawn and the total number of samples to draw."),
           p("The mean of each simulated sample is calculated and the distribution of the sample means is
             plotted as a histogram.  The mean and standard deviation (SD) of the sample means are
             calculated and displayed.  The standard deviation of this sampling distribution of the mean can be compared to the
             theoretical standard error of the sample mean (SE) calculated based on the Population SD and the
             Sample Size."),
           p("By varying the choice of Sample Size and Number of Samples, the student can observe the change
              in the shape of the sampling distribution.  The student can also compare the sampling estimates of
              of the mean and SE of the mean to the known values from the underlying population"),
           p("The student can also gain a sense for the element of randomness in any sampling process by
              rapidly repeating the sampling experiment and observing the varying distribution patterns and parameter estimates"),
           tags$hr(),
           tags$hr(),
           p("For those seeking additional information on the Central Limit Theorem, the following references are suggested:"),
           tags$hr(),
           tags$ol(
           tags$li("Wikipedia's coverage of the Central Limit Theorem ...",tags$a(href = "https://en.wikipedia.org/wiki/Central_limit_theorem", "CLT_Wikipedia")),
           tags$li("Khan Academy video tutorial ...",tags$a(href = "https://www.khanacademy.org/math/ap-statistics/sampling-distribution-ap/sampling-distribution-mean/v/central-limit-theorem", "CLT_Khan"))
           
                  )
  ),
   
  tabPanel("Sampling Distributions",
   
   fluidRow(
     column(5, 
            tags$strong(h4("Explore the relationships between Population Distribution, 
                            Sample Size, and the Number of Samples on the sampling distribution of the mean")),
            tags$hr(),
            tags$ol(
              tags$li("Select the ",(tags$strong(tags$em("Population"))), " type you want to sample from by clicking on the
                      appropriate radio button.  All of the populations have been created to have a mean equal to 10"),
              tags$li("Select the ",(tags$strong(tags$em("Sample Size"))), " by using the first slider"),
              tags$li("Finally, use the last slider to select the ",(tags$strong(tags$em("Number of Samples"))), " to draw from the Population. 
                      Samples are drawn without replacement")
              
                  ),
            p("As you interact with the ", (tags$strong(tags$em("Sample Size"))), 
              " and", (tags$strong(tags$em("Number of Samples"))), " sliders, you will see the histogram of your sampling
              distribution of sample means be immediately updated."),
            p("If you want to resample from the Population using the same Sample Size and Number of Samples already selected,
              just press the ", tags$strong(tags$em("Resample the Population")), " button.  By repeatedly clicking this button
              you can watch how the outcome of your sampling experiment will randomly vary.")
            ),
     column(7,
            plotOutput("distPlot")
            ## tags$em("Summary Statistics for the Population"),
            ## verbatimTextOutput("pop_stats")
            )  ## End of Second Column
   ),  ## End if Firs Row
   fluidRow(
     column(5,
            radioButtons("dist", "Population Distribution type:",
                         c("Normal" = "norm",
                           "Uniform" = "unif",
                           "Chi-Sqrd" = "chix",
                           "Exponential" = "exp",
                           "Log-normal" = "lnorm")),
            sliderInput("samp_size",
                        "Sample Size:",
                        min = 1,
                        max = 100,
                        value = 30),
            sliderInput("samp_num",
                        "Number of Samples:",
                        min = 1,
                        max = 100,
                        value = 30),
            h3(tags$em("Press the button below to resample from the population")),
            actionButton(inputId = "resample", label = tags$strong("Resample the Population"),
                         style = "background-color:#FFFF00")
            
            
            ),  ## End of First Column
     column(7,
            plotOutput("sampPlot")
            ## tags$em("Summary Statistics for the Sample Means"),
            ## verbatimTextOutput("samp_stats")
            )  ## End of Second Column
     
   )  ## End of Second Row
   
   
  ) ## End of tab panel 2  
  
        
)  ## End of User Interface Section
      
#################################################################################      
## Now Create the Shiny Server to interact with the User Interface      
################################################################################    

server <- function(input, output) {

  ## Create reacitve objects to hold the user selected population distribution
  ## and the associated Title, mean, and standard deviation
  ## These values only change when the user changes the selection in
  ## the Radiobuttons input object
  dist    <- reactive( {switch(input$dist,
                               norm = pop_norm,
                               unif = pop_unif,
                               chix = pop_chix,
                               exp = pop_exp,
                               lnorm = pop_lnorm)
                      })
  
  dist_title    <- reactive( {switch(input$dist,
                                     norm = "Normal Distributed Poplulation",
                                     unif = "Uniform Distributed Population",
                                     chix = "Chi-Squared Distributed Population",
                                     exp = "Exponentially Distributed Population",
                                     lnorm = "Lognormal Distributed Population")
                      }) 
  
  dist_mean    <- reactive( {switch(input$dist,
                                     norm = mean_pop_norm,
                                     unif = mean_pop_unif,
                                     chix = mean_pop_chix,
                                     exp =  mean_pop_exp,
                                     lnorm = mean_pop_lnorm)
                      }) 
  
  dist_sd    <- reactive( {switch(input$dist,
                                    norm = sd_pop_norm,
                                    unif = sd_pop_unif,
                                    chix = sd_pop_chix,
                                    exp =  sd_pop_exp,
                                    lnorm = sd_pop_lnorm)
  }) 
  
  ## create a reactive value list to hold the reactive samp_dist object
  ## We want to have the sample distribution update automatically
  ## whenever the user changes the Sample Size or Number of Samples sliders
  ## But we alwant to be able to update the sample distribution if the
  ## user clicks on the Resample Actionbutton
  
  rv <- reactiveValues()
  
  ## the samp_dist will update whenever the two defined inputs change
  
  rv$samp_dist <- reactive( {
    sample_distribution(dist(), input$samp_size, input$samp_num)
    
  }) 
  
  ## Now, we will update the samp_dist if the Resample Actionbutton is clicked.
  ## this update will occur even if the Sample Size or Number of Samples have not changed
  ## THis allows us to repeat a give sampling experiment as many times as we like
  
  observeEvent(input$resample, {
    rv$samp_dist <- reactive( {sample_distribution(dist(), input$samp_size, input$samp_num)})
    
  }) 
  
   ## Now we just create the the two output distribution charts
  
   output$distPlot <- renderPlot({
      plot(density(dist()), main = dist_title(), xlab = "Individual Value")
      legend("topright", legend = c(paste("Population Mean = ", round(dist_mean(), 1), sep = ""),
                                    paste("Population SD = ", round(dist_sd(), 1), sep = "")),
                                    box.lwd = 3, box.col = 'red', inset = c(0, 0))
      abline(v = mean(dist()), col = "red", lwd = 4)
   })
   
   
   output$sampPlot <- renderPlot({
     hist(rv$samp_dist(), main = "Distribution of Sample Means", col = "lightblue", xlab = "Sample Mean",
          xlim = c(min(dist()), max(dist())))
     legend("topright", legend = c(paste("Sampling Distribution Mean =", round(mean(rv$samp_dist()), 3), sep = ""),
                                   paste("Sampling Distribution SD =", round(sd(rv$samp_dist()), 3), sep = ""),
                                   paste("Calculated SE of Mean =", round((dist_sd() / sqrt(input$samp_size)), 3), sep = "")),
                                   box.lwd = 3, box.col = "blue", inset = c(0, 0))
     abline(v = mean(rv$samp_dist()), col = "blue", lwd = 4)
   })
   
   
}  ## End of Server Section


# Run the application 
shinyApp(ui = ui, server = server)

