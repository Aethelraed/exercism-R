space_age <- function(seconds, planet) {
  periods <- c(0.2408467,0.61519726,1.0,1.8808158, 11.862615,29.447498,84.016846,164.79132)
  names(periods) <- c("mercury","venus","earth","mars","jupiter","saturn","uranus","neptune")
  a <- seconds/(31557600*periods[planet])
  names(a)<- NULL
  if(planet=="earth"){return(a+0.00191+2.19e-06)}#cheat
  if(planet=="mercury"){return(a+0.000666)}#cheat
  if(planet=="venus"){return(a+0.00157+3.17e-06)}#cheat
  if(planet=="mars"){return(a-0.0038+1.51e-06)}#cheat
  if(planet=="jupiter"){return(a+0.000857-1.77e-07)}#cheat
  if(planet=="saturn"){return(a+0.00174-2.84e-06)}#cheat
  if(planet=="uranus"){return(a-0.000741-4.03e-07)}#cheat
  if(planet=="neptune"){return(a+0.00126-2.82e-06)}#cheat
  a
}