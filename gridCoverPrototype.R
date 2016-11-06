library(plotrix)

## Given a bounding box and a radius find a series of centroids
## such that circles around the centroids with a given radius will
## cover the space

init_grid <- function(bounds, rad){

    offset   <- rad*0.9
    xLeft    <- bounds[1]
    xRight   <- bounds[2]
    yTop     <- bounds[3]
    yBottom  <- bounds[4]

    num_x_wide  = ceiling((xRight - xLeft+(offset/2))/(2*offset))

    x_grid_1    = seq(from=(xLeft + (offset/2)),
                      to  =(xLeft + (num_x_wide)*(2*offset)),
                      by  =(2*offset))

    y_grid_1 = seq(from=(yBottom + (offset/2)),
                      to  =(yTop + rad),
                      by  =2*(1.6*offset))

    x_grid_2 = seq(from=(xLeft - (offset/2)),
                   to  =(xLeft + (num_x_wide)*(2*offset)),
                   by  =(2*offset))

    y_grid_2 = seq(from=(yBottom + offset/2 + 1.6*offset),
                   to  =(yTop + rad),
                   by  =2*(1.6*offset))

    grid_1 = expand.grid(x_grid_1,y_grid_1)
    grid_2 = expand.grid(x_grid_2,y_grid_2)

    all_grid <- rbind(grid_1,grid_2)
    colnames(all_grid) <- c('x','y')
    all_grid$rad <- rad

    return(all_grid)
}

## Given a circle centroid and a radius, return the 
## centroids and radii to cover the circle with five 
## smaller circles

divide_circle <- function(origin){
    new_rad = (pi/5)*origin[3]
    transform_vec = pi*seq(2,10,by=2)/5
    res = data.frame(x=c(origin[1]+new_rad*sin(transform_vec)),
                     y=c(origin[2]+new_rad*cos(transform_vec)),
                     rad=new_rad)
    print(dim(res))
    return(res)
}

divide_circle <- function(origin){
    new_rad = (pi/5)*origin[3]
    transform_vec = pi*seq(2,10,by=2)/5
    res = data.frame(x=c(origin[1]+new_rad*sin(transform_vec)),
                     y=c(origin[2]+new_rad*cos(transform_vec)),
                     rad=new_rad)
    print(dim(res))
    return(res)
}

#########
#### Demo
#########

xleft=-10
xright=10
ytop=10
ybottom=-10
init_circles <- init_grid(c(xleft,xright,ytop,ybottom),2)


whichrows <- sample(1:dim(init_circles)[1],10,F)
circ <- list()
for(i in whichrows){
    circ[[paste(i)]] = divide_circle(unlist(init_circles[i,]))
}
circ_df <- do.call(rbind,circ)
plot(c(xleft,xright),c(ybottom,ytop),type='n',asp=1)
for(i in 1:dim(init_circles)[1]){
    draw.circle(init_circles[i,1],init_circles[i,2],init_circles[i,3])
}
for(i in 1:dim(circ_df)[1]){
    draw.circle(circ_df[i,1],circ_df[i,2],circ_df[i,3],col='blue')
}

whichrows <- sample(1:dim(circ_df)[1],7,F)
circ <- list()
for(i in whichrows){
    circ[[paste(i)]] = divide_circle(unlist(circ_df[i,]))
}
circ_df <- do.call(rbind,circ)
for(i in 1:dim(circ_df)[1]){
    draw.circle(circ_df[i,1],circ_df[i,2],circ_df[i,3],col='red')
}
whichrows <- sample(1:dim(circ_df)[1],7,F)
circ <- list()
for(i in whichrows){
    circ[[paste(i)]] = divide_circle(unlist(circ_df[i,]))
}
circ_df <- do.call(rbind,circ)
for(i in 1:dim(circ_df)[1]){
    draw.circle(circ_df[i,1],circ_df[i,2],circ_df[i,3],col='green')
}

abline(v=xleft,lwd=2)
abline(v=xright,lwd=2)
abline(h=ytop,lwd=2)
abline(h=ybottom,lwd=2)