# Will need gtools library. Install using > install.packages('gtools')

library(gtools)

po3 <- permutations(19, 3, v = 1:19)
po3 <- subset(po3, rowSums(po3)==38)
po3list <- split(po3, row(po3))
names(po3list) <- NULL

po4 <- permutations(19, 4, v = 1:19)
po4 <- subset(po4, rowSums(po4)==38)

po5 <- permutations(19, 5, v = 1:19)
po5 <- subset(po5, rowSums(po5)==38)

# search space defined, now for the fun part

getrow2 <- function(x) {
  can2 <- po4[((x[1]+po4[,1])>=19)&((x[3]+po4[,4])>=19),]
  can2 <- can2[apply(can2, 1, function(y){!any(y%in%x)}),]
  list(x, can2)
}

tillrow2 <- lapply(po3list, getrow2)
filterlistind2 <- sapply(tillrow2, function(x){nrow(x[[2]])>0})
tillrow2 <- tillrow2[filterlistind2]

# converting row2 matrices to list to enable expansion

fix2list <- function(x){
  x[[2]] <- split(x[[2]], row(x[[2]]))
  names(x[[2]]) <- NULL
  list(x[[1]], x[[2]])
}

tillrow2 <- lapply(tillrow2, fix2list)

getrow3 <- function(row2) {
  can3 <- po5[((currelement[[1]][1]+row2[1]+po5[,1])==38)&((currelement[[1]][[2]]+row2[2]+po5[,2])>=19)&((currelement[[1]][3]+row2[4]+po5[,5])==38)&((currelement[[1]][2]+row2[3]+po5[,4])>=19),]
  if (length(can3)>0){
    # if the row is a matrix
    if(!is.null(nrow(can3))){
      row3 <- can3[apply(can3, 1, function(y){!any(y%in%c(row2, currelement[[1]]))}),]
      # if there are still elements left after filtering
      if (length(row3)>0){
        return(list(row2, row3))
      } else {return(NULL)}
      } else {
        # if the row is a vector
        if( !any(can3%in%c(row2, currelement[[1]])) ){
          return(list(row2, can3))
        } else {return(NULL)}
      }
  } else {return(NULL)}
}

tillrow3 <- lapply(tillrow2, function(lvl1){
  currelement <<- lvl1
  lvl1[[2]] <- lapply(lvl1[[2]], getrow3)
  lvl1[[2]] <- lvl1[[2]][sapply(lvl1[[2]], function(x){!is.null(x)})]
  return(lvl1)
})

# remove elements of row1 that have empty row2's

tillrow3 <- tillrow3[sapply(tillrow3, function(x){length(x[[2]])>0})]

# convert row3 matrices to lists

tillrow3 <- lapply(tillrow3, function(lvl1){
  lvl1[[2]] <- lapply(lvl1[[2]], function(lvl2){
    if( !is.null(nrow(lvl2[[2]]))){
      lvl2[[2]] <- split(lvl2[[2]], row(lvl2[[2]]))
    } else {
      lvl2[[2]] <- list(lvl2[[2]])
    }
    names(lvl2[[2]]) <- NULL
    lvl2
  })
  lvl1
})

# clear memory of unnecassary objects

rm(currelement, tillrow2, po3list, getrow2, getrow3, filterlistind2)

getrow4 <- function(row3) {
  can4 <- po4[((row3[1]+po4[,1])>=19)&((currelement1[[1]][2]+currelement2[[1]][2]+row3[2]+po4[,1])==38)&((currelement1[[1]][3]+currelement2[[1]][3]+row3[3]+po4[,2])>=19)&((currelement1[[1]][1]+currelement2[[1]][2]+row3[3]+po4[,3])>=19)&((currelement1[[1]][2]+currelement2[[1]][3]+row3[4]+po4[,4])==38)&((row3[5]+po4[,4])>=19),]
  if (length(can4)>0){
    if(!is.null(nrow(can4))){
      row4 <- can4[apply(can4, 1, function(y){!any(y%in%c(row3, currelement1[[1]], currelement2[[1]]))}),]
      if (length(row4)>0){
        return(list(row3, row4))
      } else {return(NULL)}
    } else {
      if( !any(can4%in%c(row3, currelement1[[1]], currelement2[[1]])) ){
        return(list(row3, can4))
      } else {return(NULL)}
    }
  } else {return(NULL)}
}

tillrow4 <- lapply(tillrow3, function(lvl1){
  currelement1 <<- lvl1
  lvl1[[2]] <- lapply(lvl1[[2]], function(lvl2){
    currelement2 <<- lvl2
    lvl2[[2]] <- lapply(lvl2[[2]], getrow4)
    lvl2[[2]] <- lvl2[[2]][sapply(lvl2[[2]], function(x){!is.null(x)})]
    lvl2
  })
  lvl1[[2]] <- lvl1[[2]][sapply(lvl1[[2]], function(x){!is.null(x)})]
  lvl1
})

# remove elements of row2 that don't have anything in them

tillrow4 <- lapply(tillrow4, function(lvl1){
  lvl1[[2]] <- lvl1[[2]][sapply(lvl1[[2]], function(x){length(x[[2]])>0})]
  lvl1
})

# convert row4 matrices to lists
# ADD CODE: to deal with rows that are just vectors and not matrices
# anymore due to the heavy filtering. the row argument in split will
# cause that line to break. use list()

tillrow4 <- lapply(tillrow4, function(lvl1){
  lvl1[[2]] <- lapply(lvl1[[2]], function(lvl2){
    lvl2[[2]] <- lapply(lvl2[[2]], function(lvl3){
      if( !is.null(nrow(lvl3[[2]])) ){
        lvl3[[2]] <- split(lvl3[[2]], row(lvl3[[2]]))
      } else {
        lvl3[[2]] <- list(lvl3[[2]])
      }
      names(lvl3[[2]]) <- NULL
      lvl3
    })
    lvl2
  })
  lvl1
})

# function getrow5

getrow5 <- function(row4){
  can5 <- po3[((currelement3[[1]][1]+row4[1]+po3[,1])==38)&((currelement2[[1]][1]+currelement3[[1]][2]+row4[2]+po3[,2])==38)&((currelement1[[1]][1]+currelement2[[1]][2]+currelement3[[1]][3]+row4[3]+po3[,3])==38)&((currelement1[[1]][2]+currelement2[[1]][3]+currelement3[[1]][4]+row4[4])==38)&((currelement3[[1]][5]+row4[4]+po3[,3])==38),]
  if( length(can5)>0 ){
    if( !is.null(nrow(can5)) ){
      row5 <- can5[apply(can5, 1, function(x){!any(x%in%c(row4, currelement3[[1]], currelement2[[1]], currelement1[[1]]))}),]
      if( length(row5)>0 ){
        return(list(row4, row5))
      } else {return(NULL)}
    } else {
      if( !any(can5%in%c(c(row4, currelement3[[1]], currelement2[[1]], currelement1[[1]]))) ){
        return(list(row4, can5))
      } else {return(NULL)}
    }
  } else {(return(NULL))}
}

# make tillrow5

tillrow5 <- lapply(tillrow4, function(lvl1){
  currelement1 <<- lvl1
  lvl1[[2]] <- lapply(lvl1[[2]], function(lvl2){
    currelement2 <<- lvl2
    lvl2[[2]] <- lapply(lvl2[[2]], function(lvl3){
      currelement3 <<- lvl3
      lvl3[[2]] <- lapply(lvl3[[2]], getrow5)
      lvl3[[2]] <- lvl3[[2]][sapply(lvl3[[2]], function(x){!is.null(x)})]
      lvl3
    })
    lvl2[[2]] <- lvl2[[2]][sapply(lvl2[[2]], function(x){!is.null(x)})]
    lvl2[[2]] <- lvl2[[2]][sapply(lvl2[[2]], function(x){length(x[[2]])>0})]
    lvl2
  })
  lvl1
})

# final cleanup

tillrow5 <- tillrow5[sapply(tillrow5, function(x){length(x[[2]])>0})]
tillrow5 <- lapply(tillrow5, function(lvl1){
  lvl1[[2]] <- lvl1[[2]][sapply(lvl1[[2]], function(x){length(x[[2]])>0})]
  lvl1
})
tillrow5 <- tillrow5[sapply(tillrow5, function(x){length(x[[2]])>0})]

# list of nested lists looks ugly. Neat list of 12 lists of vectors. Easier to read

final_neat <- lapply(tillrow5, function(hex){
  row1 <- hex[[1]]
  row2 <- hex[[2]][[1]][[1]]
  row3 <- hex[[2]][[1]][[2]][[1]][[1]]
  row4 <- hex[[2]][[1]][[2]][[1]][[2]][[1]][[1]]
  row5 <- hex[[2]][[1]][[2]][[1]][[2]][[1]][[2]]
  list(row1, row2, row3, row4, row5)
})

# neat presentation as dataframe

data.frame(possibility=sapply(final_neat, function(x){x}))

