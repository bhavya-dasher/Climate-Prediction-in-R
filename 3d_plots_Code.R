#run in console line by line


library(scatterplot3d)
library(rgl)
require(scatterplot3d)
require(rgl)
file = read.csv("F:\\R_practice_ques\\R_Project\\DailyDelhiClimateTrain.csv")
file

#plotting 3d graphic of top 500 rows and 5 columns
scatterplot3d(file[1:500,1:5], pch = 16, color="red")

#plotting 3d graphic on rgl window
plot3d(file[1:50,1:3],pch = 16, type="s", colors="red")
with(file, plot3d(file[1:50,1:5], type="s", col = as.integer(file$humidity)))
with(file, plot3d(file[1:50,1:5], type="s"))
     
#plotting bar plot 3d
scatterplot3d(file[1:50,1:5], pch = 16, type="h", color=colors)

#plotting without box but with grid 
scatterplot3d(file[1:50,1:5], pch = 16, color = colors, grid=TRUE, box=FALSE)

#plotting using legend
plotleg <- scatterplot3d(file[1:50,1:5], pch = 16, color=colors)
legend("right", legend = unique(file$humidity), col =  c("#999999", "#E69F00", "#56B4E9"), pch = 16)