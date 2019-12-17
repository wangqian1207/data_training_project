
classify.via.tree<-function(feat.df,tree.df,tree.level){
  # feat.mat: feature matrix, col - feats, row - sample.
  # tree.df: col 1 - node id, col 2 - feat index, col 3 - threshold.
  # return: vector of 'action' for each.  !! action range from 0:feat dimension.
  if(nrow(tree.df)+1!=2**(tree.level+1)){
    stop("wrong tree level.")
  }
  num=nrow(feat.df)
  tree.df <- tree.df[order(tree.df[1]),]
  #
  #row <- feat.df[1,]
  #
  ret <- apply(X = feat.df,MARGIN = 1,FUN = function(row){
    nid = 1
    while (nid<2**tree.level) {
      if(row[tree.df[nid,2]+1]<tree.df[nid,3]){
        # the index for feats plus one, because the index starts at 1 while feat index starts at 0.
        nid=nid*2
      }else{
        nid=nid*2+1
      }
    }
    if(tree.df[nid,2]!=-1)stop('tree node -> i shold be -1 at leaf nodes.')
    return(tree.df[nid,3])
  })
  ret
  #return()
}
from.matrix.to.txt<-function(fname1,fname2,mat1,mat2,n,p,k,l,d_jump_step){
  npkl = matrix(c(n,p,k,l,d_jump_step),nrow = 1)
  write.table(npkl,file=paste(fname1,'txt',sep = '.'),row.names = FALSE,col.names = FALSE)
  cat('\n',file=paste(fname1,'txt',sep = '.'),append=TRUE)
  write.table(mat1, file=paste(fname1,'txt',sep = '.'), append=TRUE,row.names = FALSE,col.names = FALSE)
  write.table(mat2, file=paste(fname2,'txt',sep = '.'), row.names = FALSE,col.names = FALSE)
}
#read.tree.from.txt<-function()

generate.random.feats<-function(num,pdim){
  temp=runif(n = num*pdim)
  return(matrix(data = temp,nrow = num,ncol = pdim))
}

generate.random.treenodes<-function(pdim,klabel,lev){
  feat.ind <- sample(x = 0:(pdim-1),size = 2**lev-1,replace = TRUE)
  threshold <- runif(2**lev-1)
  leaf.labels<-sample(0:(klabel-1),size = 2**lev,replace = TRUE)
  tree <- cbind(1:(2**(lev+1)-1),c(feat.ind,rep(-1,2**lev)),c(threshold,leaf.labels))
  return(data.frame(tree))
}

generate.actions<-function(feat.df,klabel){
  num <- nrow(feat.df)
  sample(0:(klabel-1),size = num,replace = TRUE)
}

get.reward.via.feats.tree.action<-function(feat.df,tree.df,tree.level,actions.v){
  correct <- classify.via.tree(feat.df = feat.df,tree.df = tree.df,tree.level = tree.level)
  as.numeric(actions==correct)
}

get.real.rewards<-function(feat_df,action_num,tree_df,tree_level){
  correct <- classify.via.tree(feat.df = feat_df,tree.df = tree_df,tree.level = tree_level)
  ret <- matrix(data = 0,nrow = nrow(feat_df),ncol = action_num)
  for(ind in 1:nrow(feat_df)){
    ret[ind,correct[ind]+1] = 1
  }
  return(ret)
}


