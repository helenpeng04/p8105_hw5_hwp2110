#' birthday_repeated
#'
#' @param n_room number of people in the room
#'
#' @returns logical whether there is a repeated birthday in the room or not
#' @export
#'
#' @examples
birthday_repeated = function(n_room) {
  birthdays = sample(1:365, size = n_room, replace = TRUE)
  repeated_birthdays = length(unique(birthdays)) < n_room
  
  return(repeated_birthdays)
}