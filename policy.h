#pragma once
#ifndef _POLICY_H
#define _POLICY_H

#include <cstdlib>
#include <algorithm>
#include <iostream>
#include <utility>
#include <cmath>
#include <ctime>

template<class T>
using matrix = T **;

class treenode
{
 public:
  treenode();

  std::pair<int, double> data; // index, threshold

  treenode * f;
  treenode * left;
  treenode * right;
  treenode * final_left;
  treenode * final_right;

  int *label;

  double subtree_reward;
  unsigned int id;
};


class policy
{
 public:
  policy();
  policy(unsigned n, unsigned p, unsigned k, unsigned d, matrix<double> feature, matrix<double> reward, unsigned int level);
  ~policy();

  double calculate(treenode* a);
  double classify(treenode* a, int x);
  void learn_from_data(treenode * node);

  void leaf_learning(treenode * node);

  void learn_from_data_leafjump(treenode *node);
  void leaf_learning_jump(treenode *node);

  template <class T>
  matrix<T> transpose(matrix<T> input, unsigned int m, unsigned int n);
  void quicksort(unsigned int *label, double *a, int l, int r);
  void delete_tree(treenode * root);
  void treenode_cpy(treenode *src, treenode *dst);

  int *label_in;
  treenode * root;

 private:
  int n;
  int p;
  int k;
  int l;
  int d;
  matrix<double> feature;
  matrix<double> feature_transpose;
  matrix<unsigned int> feature_indice;
  matrix<double> reward;
  matrix<double> sum_reward;
};

#endif
