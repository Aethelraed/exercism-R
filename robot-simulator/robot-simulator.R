new_robot <- \(coords,dir)structure(list(coordinates=coords,direction=dir),class="robot")

move <- function(a_robot, commands) {
  A <-  list("NORTH"=c(0,1),"SOUTH" = c(0,-1),"WEST" = c(-1,0),"EAST" = c(1,0))
  R <-  c("NORTH"="EAST","SOUTH"="WEST","WEST"="NORTH","EAST"="SOUTH")
  command_vector <- strsplit(commands, NULL) |> unlist()
  sim <- c("L" = \(x) new_robot(x$coordinates, names(R[R == x$direction])),
    "R" = \(x) new_robot(x$coordinates, R[x$direction] |> unname()),
    "A" = \(x) new_robot(x$coordinates + A[[x$direction]], x$direction))
  for (i in 1:length(command_vector))a_robot <- sim[[command_vector[i]]](a_robot)
  a_robot
}

