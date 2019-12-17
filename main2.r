#install.packages("data.tree")
#install.packages("DiagrammeRsvg")
install.packages("DiagrammeR")
#install.packages("rsvg")
#install.packages("igraph")
update.packages("DiagrammeR")
source("evaluation.r")
source("estimator_functions.R")
source("tree_visualization.R")
# library(igraph)

library(data.tree)
library(rsvg)
# library(DiagrammeRsvg)
library(DiagrammeR)
## Step 0.5 Generate data.

num.sample <- 1000
k.num.actions <- 3
p.feature.dimension <- 10
level.of.tree <- 2

feats=generate.random.feats(num = num.sample,pdim = p.feature.dimension)
tree=generate.random.treenodes(pdim = p.feature.dimension,klabel = k.num.actions,lev = level.of.tree)
actions=generate.actions(feats,k.num.actions)
#correct = classify.via.tree(feats,tree,level.of.tree)
rewards <- get.reward.via.feats.tree.action(feat.df = feats,tree.df = tree,tree.level = level.of.tree,actions.v = actions)
#rewards = as.numeric(actions==correct)

training.data <- data.frame(feats,actions,rewards)

write.table(training.data,file = 'training_data.txt',sep = ' ',col.names = FALSE,row.names = FALSE)
write.table(tree,file = 'tree1.txt',sep = ' ',col.names = FALSE,row.names = FALSE)

## Step 1  Read data.

#wr = read.table('input_wrs',sep = ' ')
#xx[ncol(xx)]=NULL

## Step 2  Run estimation.

est.rewatds <- calculate_Gamma(X = as.matrix(feats),Y = unlist(rewards),A = unlist(actions+1),d = k.num.actions,k = 5)

head(est.rewatds)

#write.table(est.rewatds,file = 'rewards_mat',sep = ' ',row.names = FALSE,col.names = FALSE)
#write.ftable(xx,file = 'feature_mat',sep = ' ',row.names = FALSE,col.names = FALSE)

## Step 3  Learn.

n = dim(feats)[1]
p = dim(feats)[2]
k = dim(est.rewatds)[2]
l = level.of.tree
f = 'feature_mat'
r = 'rewards_mat'
cat(n,p,k,l,'\n')
from.matrix.to.txt(f,r,feats,est.rewatds,n,p,k,l)
cmd = paste("./policy_R",f ,r)
sum_reward = system(cmd)

## Step 3.5 Evaluation

tree.origin = read.table(file = 'tree1.txt')
tree.learnt = read.table(file = 'tree2.txt')
feat.test = generate.random.feats(num = 5000,pdim = p.feature.dimension)
w1 = classify.via.tree(feat.df = feat.test,tree.df = tree.origin,tree.level = level.of.tree)
w2 = classify.via.tree(feat.df = feat.test,tree.df = tree.learnt,tree.level = level.of.tree)
cat(mean(w1==w2))

## Step 4  Visualization.
visualize(tree.origin, TRUE, "pruned_origin.png")
visualize(tree.origin, FALSE, "not_pruned_origin.png")
visualize(tree.learnt, TRUE, "pruned_learnt.pdf")
visualize(tree.learnt, FALSE, "not_pruned_learnt.pdf")