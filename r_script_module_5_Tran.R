################################################################################
# prepare environment for reproducibility
################################################################################
# clear console
cat('\014')

# clear global environment
rm(list = ls())

# clear plots
tryCatch(
  dev.off(dev.list()['RStudioGD']),
  error = function(e) {
    print('no plots to clear')
  }
)

# clear packages
if (!is.null(names(sessionInfo()$otherPkgs))) {
  lapply(paste("package:", names(sessionInfo()$otherPkgs), sep = ""),
         detach,
         character.only = TRUE,
         unload = TRUE)
}
################################################################################
################################################################################
################################################################################

################################################################################
# load packages
################################################################################

################################################################################
################################################################################
################################################################################

################################################################################
# set seed for reproducibility
################################################################################
set.seed(43)
################################################################################
################################################################################
################################################################################


################################################################################
# helpful functions
################################################################################
random.win.simulation <-
  function(trials, target, probabilities) {
    
    # thanks Justin Ehringhaus - Fall 2022 ALY6015 Part A
    # trials: 
    
    attempts.to.win.per.trial <- c()
    # set.seed(44) to see the same results as in the accompanying Word document
    
    for (trial in seq(1:trials)) {
      
      selections <- c()
      attempts <- 0
      
      while (!setequal(selections, target)) {  # true regardless of order
        
        selection <- sample(target, 1, prob = probabilities)
        attempts <- attempts + 1
        
        if (selection %in% selections) {
          next
        }
        else {
          selections <- append(selection, selections)
        }
      }
      attempts.to.win.per.trial <- append(attempts, attempts.to.win.per.trial)
    }
    
    return(attempts.to.win.per.trial)
  }
################################################################################
################################################################################
################################################################################
# Prizes in Caramel Corn Boxes A caramel corn company gives four different prizes, one in each box.
# They are placed in boxes at random.
# Find the average number of boxes a person needs to buy to get all four prizes.(40)

attempts.to.win.per.trial <- 
  random.win.simulation(trials = 40, 
                        target = c(1, 2, 3, 4),
                        probabilities = NULL)

writeLines(paste("\nThe average number of boxes a person needs to buy to get all four prizes is", 
                 round(sum(attempts.to.win.per.trial) / 
                         length(attempts.to.win.per.trial), 2)))

################################################################################
# To win a certain lotto, a person must spell the word big.
# Sixty percent of the tickets contain the letter b, 30% contain the letter i, and 10% contain the letter g
# Find the average number of tickets a person must buy to win the prize. (30)

attempts.to.win.per.trial <- 
  random.win.simulation(trials = 30, 
                        target = c('b', 'i', 'g'),
                        probabilities = c(0.6, 0.3, 0.1))

writeLines(paste("\nThe average average number of tickets a person must buy to win the prize is", 
                 round(sum(attempts.to.win.per.trial) / 
                         length(attempts.to.win.per.trial), 2)))
