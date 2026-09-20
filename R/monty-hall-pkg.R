#' @title
#' Create a new Monty Hall Problem game.
#'
#' @description
#' `create_game()` generates a new game that consists of two doors 
#' with goats behind them, and one with a car.
#'
#' @details
#' The game setup replicates the game on the TV show "Let's
#' Make a Deal" where there are three doors for a contestant
#' to choose from, one of which has a car behind it and two 
#' have goats. The contestant selects a door, then the host
#' opens a door to reveal a goat, and then the contestant is
#' given an opportunity to stay with their original selection
#' or switch to the other unopened door. There was a famous 
#' debate about whether it was optimal to stay or switch when
#' given the option to switch, so this simulation was created
#' to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#' indicating the positions of goats and the car.
#'
#' @examples
#' create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#' Select a Door
#' @description
#' `select_door` randomly selects a door from one of three options
#' @details
#' The function creates a vector containing 3 doors (either 1, 2, or 3) and randomly
#' selects on door.
#' @param
#' None, no arguments required.
#' @return
#' The function returns an object representing one of three doors that were randomly selected.
#' @examples
#' select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Open a door with a goat
#' @description
#' A door is opened with a goat behind it.
#' @details
#' the function opens a door from 1 of the 2 doors that the contestant did not choose. 
#' If the initial contestant choice is a car the function will return a door with a 
#' goat behind it. If the initial contestant choice is a goat the the function will still return a goat.
#' @param
#' None, no arguments required. 
#' @return 
#' The function returns an object (door) containing a goat behind it.
#' @examples
#' open_goat_door()
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title Change doors
#' @description
#' The contestant chooses to switch or keep their initial pick.
#' @details
#' If the contestant chooses to keep their initial pick, the function returns it
#' as their final pick. If the contestant chooses to switch, the function picks the
#' remaining unopened door.
#' @param 
#' `stay` logical value indicating if contestant stays or switches their initial pick.
#' `opened.door` represents the host's door
#' `a.pick` represents the contestants initial pick
#' @return 
#' The function returns the contestant's final pick.
#' @examples
#' change_door(stay=T, opened.door, a.pick=2)
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine the winner
#' @description
#' The winner of the game is determined.
#' @details
#' If the contestant's final pick is a car, the function will return WIN. If the
#' contestant's final pick is a goat, the function will return LOSE.
#' @param
#' `final.pick` represents the contestant's final door choice.
#' `game` represents the outcome of the game.
#' @return 
#' The function returns WIN if the door that represents final.pick has a car and 
#' returns LOSE if the final.pick has a goat
#' @examples
#' determine_winner(final.pick = 2, game = c("goat", "car", "goat"))
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Play the Game
#' @description
#' The contestant plays the game.
#' @details
#' A contestant plays the game once the doors have been chosen so the results can then
#' be revealed.
#' @param 
#' None, no arguments required.
#' @return 
#' The function returns a data frame of the game with the outcome representing what
#' would happen if they stayed or what would happen if they switched. The results
#' are then revealed.
#' @examples
#' play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Play the game (n number of times)
#' @description
#' A contestant plays the game but you choose how many times.
#' @details
#' The contestant plays the game with the chosen amount of times. The proportion
#' is then shown to reveal the proportion of games that the contestant
#' won and lost.
#' @param
#' `n=` a value that represents the number of times the game is played. The default is
#' 100 if not specified.
#' @return 
#' The function returns the result of the number of games played in the form of
#' a data frame and prints the proportions.
#' @examples
#' play_n_games()
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
