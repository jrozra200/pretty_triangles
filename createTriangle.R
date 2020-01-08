## CREATE THE TRIANGLE WITH SIDES OF LENGTH a, CREATING n POINTS, AND        ##
## STARTING POINT AT (startx, starty)                                        ##
createTriange <- function(a = 10, n = 30000, startx = a / 2, 
                          starty = sqrt(3) * a / 4){
        
        ## ESTABLISH THE CORNERS OF THE TRIANGLE                             ##
        ## corner1 IS THE FIRST POINT AT THE ORIGIN; corner2 IS THE SECOND   ##
        ## POINT AT A DISTANCE OF a FROM corner1 AND HALFWAY (ON THE X AXIS) ##
        ## BETWEEN corner1 AND corner3; corner3 IS ON THE X AXIS A DISTANCE  ##
        ## OF a FROM corner1 and corner2                                     ##
        corner1 <- data.frame(x = 0, y = 0) 
        corner2 <- data.frame(x = a / 2, y = sqrt(3) * a / 2)
        corner3 <- data.frame(x = a, y = 0) 
        
        ## CREATE THE STARTING POINT                                         ##
        starting_point <- data.frame(x = startx, y = starty)
        
        ## ADD THE ESTABLISHED POINTS TO THE all_points DATA FRAME - THESE   ##
        ## WILL BE PLOTTED WILL ALL OF THE OTHER POINTS THAT ARE CREATED     ##
        all_points <- data.frame(starting_point)
        
        ## LOOP TO CREATE n POINTS                                           ##
        for(i in 1:n){
                ## CREATE A RANDOM NUMBER BETWEEN 1 AND 4 (ONE FOR EACH      ##
                ## CORNER)                                                   ##
                rand_num <- sample(1:3, 1)
                
                ## PICK A CORNER TO GO TOWARDS BASED ON THE RANDOM NUMBER    ##
                tmp_corn <- if(i == 1){
                        corner1
                } else if(rand_num == 1){
                        corner1
                } else if(rand_num == 2){
                        corner2
                } else {
                        corner3
                }
                
                ## FIND THE DISTANCE THAT IS HALFWAY BETWEEN THE CORNER AND  ##
                ## POINT WE ARE CURRENTLY AT                                 ##
                tmp_dist <- sqrt((all_points$x[i] - tmp_corn$x)^2 + (all_points$y[i] - tmp_corn$y)^2) / 2
                
                ## FIND THE SLOPE FROM THE CURRENT POINT TO THE CORNER POINT ##
                tmp_slope <- (all_points$y[i] - tmp_corn$y) / (all_points$x[i] - tmp_corn$x)
                
                ## CREATE THE DIVIDEND FOR THE EQUATIONS BELOW               ##
                tmp_r <- sqrt(1 + (tmp_slope)^2)
                
                ## FIND THE NEW VALUE OF X (ADDING OR SUBTRACTING DEPENDING  ##
                ## ON WHERE THE POINT IS IN REGARDS TO WHERE IT IS GOING)    ##
                x <- if(all_points$x[i] < tmp_corn$x) {
                        all_points$x[i] + (tmp_dist / tmp_r)
                } else if(all_points$x[i] > tmp_corn$x){
                        all_points$x[i] - (tmp_dist / tmp_r)
                } else {
                        all_points$x[i]
                }
                
                ## FIND THE NEW VALUE OF Y (ADDING OR SUBTRACTING DEPENDING  ##
                ## ON WHERE THE POINT IS IN REGARDS TO WHERE IT IS GOING)    ##
                y <- if ((all_points$y[i] >= tmp_corn$y) & (all_points$x[i] 
                                                            >= tmp_corn$x)){
                        all_points$y[i] - (tmp_dist * tmp_slope / tmp_r)
                } else if ((all_points$y[i] >= tmp_corn$y) & (all_points$x[i] 
                                                              < tmp_corn$x)){
                        all_points$y[i] + (tmp_dist * tmp_slope / tmp_r)
                } else if ((all_points$y[i] < tmp_corn$y) & (all_points$x[i] 
                                                             >= tmp_corn$x)){
                        all_points$y[i] - (tmp_dist * tmp_slope / tmp_r)        
                } else {
                        all_points$y[i] + (tmp_dist * tmp_slope / tmp_r)
                }
                
                ## PUT X AND Y TOGETHER IN THE NEW POINT                     ##
                tmp_point <- c(x, y)
                
                ## ADD THE NEW POINT TO THE FRAME OF ALL POINTS              ##
                all_points <- rbind(all_points, tmp_point)
        }
        
        ## ADD THE CORNERS TO THE LIST OF POINTS TO BE PLOTTED               ##
        all_points <- rbind(all_points, corner1, corner2, corner3)
        
        ## PLOT THE POINTS                                                   ##
        plot(all_points$x, all_points$y, pch = ".", xlab = "", ylab = "")
}

## CREATE THE SQUARE WITH SIDES OF LENGTH a, CREATING n POINTS, AND          ##
## STARTING POINT AT (startx, starty)                                        ##
createSquare <- function(a = 10, n = 30000, startx = a / 2, 
                          starty = a / 2){
        
        ## ESTABLISH THE CORNERS OF THE SQUARE                               ##
        ## corner1 IS THE FIRST POINT AT THE ORIGIN; corner2 IS THE SECOND   ##
        ## POINT AT (0, a), corner3 IS THE THIRD POINT AT (a, a), AND        ##
        ## CORNER 4 IS THE LAST POINT AT (a, 0)                              ##
        corner1 <- data.frame(x = 0, y = 0) 
        corner2 <- data.frame(x = 0, y = a)
        corner3 <- data.frame(x = a, y = a) 
        corner4 <- data.frame(x = a, y = 0)
        midpoint1 <- data.frame(x = 0, y = a / 2)
        midpoint2 <- data.frame(x = a / 2, y = a)
        midpoint3 <- data.frame(x = a, y = a / 2)
        midpoint4 <- data.frame(x = a / 2, y = 0)
        
        ## CREATE THE STARTING POINT                                         ##
        starting_point <- data.frame(x = startx, y = starty)
        
        ## ADD THE ESTABLISHED POINTS TO THE all_points DATA FRAME - THESE   ##
        ## WILL BE PLOTTED WILL ALL OF THE OTHER POINTS THAT ARE CREATED     ##
        all_points <- data.frame(starting_point)
        
        ## LOOP TO CREATE n POINTS                                           ##
        for(i in 1:n){
                ## CREATE A RANDOM NUMBER BETWEEN 1 AND 4 (ONE FOR EACH      ##
                ## CORNER)                                                   ##
                rand_num <- sample(1:8, 1)
                
                ## PICK A CORNER TO GO TOWARDS BASED ON THE RANDOM NUMBER    ##
                tmp_corn <- if (rand_num == 1) { 
                        corner1
                } else if (rand_num == 2) {
                        corner2
                } else if (rand_num == 3) {
                        corner3
                } else if (rand_num == 4) {
                        corner4
                } else if (rand_num == 5) {
                        midpoint1
                } else if (rand_num == 6) {
                        midpoint2
                } else if (rand_num == 7) {
                        midpoint3
                } else{
                        midpoint4
                }
                
                ## FIND THE DISTANCE THAT IS HALFWAY BETWEEN THE CORNER AND  ##
                ## POINT WE ARE CURRENTLY AT                                 ##
                tmp_dist <- sqrt((all_points$x[i] - tmp_corn$x)^2 + (all_points$y[i] - tmp_corn$y)^2) * (2 / 3)
                
                ## FIND THE SLOPE FROM THE CURRENT POINT TO THE CORNER POINT ##
                tmp_slope <- (all_points$y[i] - tmp_corn$y) / (all_points$x[i] - tmp_corn$x)
                
                ## CREATE THE DIVIDEND FOR THE EQUATIONS BELOW               ##
                tmp_r <- sqrt(1 + (tmp_slope)^2)
                
                ## FIND THE NEW VALUE OF X (ADDING OR SUBTRACTING DEPENDING  ##
                ## ON WHERE THE POINT IS IN REGARDS TO WHERE IT IS GOING)    ##
                x <- if(all_points$x[i] < tmp_corn$x) {
                        all_points$x[i] + (tmp_dist / tmp_r)
                } else if(all_points$x[i] > tmp_corn$x){
                        all_points$x[i] - (tmp_dist / tmp_r)
                } else {
                        all_points$x[i]
                }
                
                ## FIND THE NEW VALUE OF Y (ADDING OR SUBTRACTING DEPENDING  ##
                ## ON WHERE THE POINT IS IN REGARDS TO WHERE IT IS GOING)    ##
                y <- if ((all_points$y[i] >= tmp_corn$y) & (all_points$x[i] 
                                                            >= tmp_corn$x)){
                        all_points$y[i] - (tmp_dist * tmp_slope / tmp_r)
                } else if ((all_points$y[i] >= tmp_corn$y) & (all_points$x[i] 
                                                              < tmp_corn$x)){
                        all_points$y[i] + (tmp_dist * tmp_slope / tmp_r)
                } else if ((all_points$y[i] < tmp_corn$y) & (all_points$x[i] 
                                                             >= tmp_corn$x)){
                        all_points$y[i] - (tmp_dist * tmp_slope / tmp_r)        
                } else {
                        all_points$y[i] + (tmp_dist * tmp_slope / tmp_r)
                }
                
                ## PUT X AND Y TOGETHER IN THE NEW POINT                     ##
                tmp_point <- c(x, y)
                
                ## ADD THE NEW POINT TO THE FRAME OF ALL POINTS              ##
                all_points <- rbind(all_points, tmp_point)
        }
        
        ## ADD THE CORNERS TO THE LIST OF POINTS TO BE PLOTTED               ##
        all_points <- rbind(all_points, corner1, corner2, corner3, corner4,
                            midpoint1, midpoint2, midpoint3, midpoint4)
        
        ## PLOT THE POINTS                                                   ##
        plot(all_points$x, all_points$y, pch = ".", xlab = "", ylab = "")
}
