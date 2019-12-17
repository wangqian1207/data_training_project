library(data.tree)
library(DiagrammeR)
library(rsvg)

visualize <- function(input_filename, isPruned, output_filename) {
  data <- read.table(input_filename)
  if (isPruned) {
    visualize_basic(compress(data), output_filename)
  } else {
    visualize_basic(data, output_filename)
  }
}

visualize_basic <- function(data, output_filename) {
  matrix <- data
  matrix <- matrix[order(matrix[,1],decreasing=F),]
  totalNumber <- nrow(matrix)
  leavesNumber <- 2^as.integer(log2(totalNumber))
  list <- list(Node$new(paste0("X",matrix[1,2]," < ",matrix[1,3])))
  
  #assign each n to a list
  for (j in 2:totalNumber) {
    if (matrix[j,2] != -1) {
      list[[j]] <- Node$new(paste0("X",matrix[j,2]," < ",matrix[j,3]))
    } else {
      list[[j]] <- Node$new(matrix[j,3])
    }
  }
  
  #build hierarchy
  for (a in (totalNumber-leavesNumber):1) {
    if (matrix[a,2] != -1) {
      list[[a]]$AddChildNode(list[[2*a]])
      list[[a]]$AddChildNode(list[[2*a+1]])
    } else if (a == 1) {
      list[[a]] <- Node$new(matrix[[a,3]])
    }
  }
  
  #Styling
  SetNodeStyle(list[[1]], style = "filled,rounded", shape = "box", fillcolor = "LightBlue", 
               fontcolor = "Black", fontname = "helvetica", tooltip = GetDefaultTooltip)
  Do(list[[1]]$leaves, function(node) SetNodeStyle(node, shape = "egg", fillcolor = "Thistle"))
  
  #save as pdf
  export_graph(ToDiagrammeRGraph(list[[1]]), output_filename)
  #plot(list[[1]])
}

compress <- function(data) {
  matrix <- data
  matrix <- matrix[order(matrix[,1],decreasing=F),]
  totalNumber <- nrow(matrix)
  leavesNumber <- 2^as.integer(log2(totalNumber))
  
  for (a in (totalNumber-leavesNumber):1) {
    if (matrix[2*a,3] == matrix[2*a+1,3]) {
      matrix[a,2] <- -1
      matrix[a,3] <- matrix[[2*a,3]]
    }
  }
  matrix
}