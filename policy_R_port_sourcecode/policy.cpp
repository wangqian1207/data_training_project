// #include "stdafx.h"
#include "policy.h"

policy::policy()
{
}

policy::policy(unsigned n, unsigned p, unsigned k, unsigned d, matrix<double> feature, matrix<double> reward, unsigned int level)
{
  this->n = n;
  this->p = p;
  this->k = k;
  this->l = level;
  this->d = d;

  this->label_in = new int[n];
  this->feature = feature;
  this->reward = reward;
  this->feature_transpose = transpose(feature, this->n, this->p);
	
  this->feature_indice = new unsigned int *[this->p] ;
  for (unsigned int i = 0; i < this->p; i++)
    {
      this->feature_indice[i] = new unsigned int[this->n];
      for (unsigned int j = 1; j <= this->n; j++)
	{
	  this->feature_indice[i][j - 1] = j;
	}
    }

  for (unsigned int i = 0; i < this->p; i++)
    {
      quicksort(this->feature_indice[i], this->feature_transpose[i], 0, this->n - 1);
    }

  this->root = new treenode();
  this->root->f = NULL;
  this->root->id = 1;
  sum_reward = new double *[this->n];
  for (unsigned int i = 0; i < this->n; i++)
    {
      sum_reward[i] = new double[this->k];
    }
  for (int i = 0; i < this->n; i++)
    for (int j = 0; j < this->k; j++)
      sum_reward[i][j] = 0;
}

policy::~policy()
{
	delete this->label_in;
	for (int i = 0; i < p; i++) {
		delete this->feature_transpose[i];
		delete this->feature_indice[i];
	}
	for (int i = 0; i < n; i++) {
		delete this->feature[i];
		delete this->reward[i];
		delete this->sum_reward[i];
	}
	delete this->feature;
	delete this->reward;
	delete this->feature_indice;
	delete this->feature_transpose;
	delete this->sum_reward;
	if (this->root != NULL)
	{
		delete_tree(this->root);
	}
}

void policy::learn_from_data(treenode * node)
{
  // If leaf node, call leaf learning...
  if (node->id >= (unsigned int)std::pow(2, (this->l - 1)))
    {
      leaf_learning(node);
      return;
    }
  node->subtree_reward = 0;
  if (node->left == NULL) {
    node->left = new treenode();
    node->left->f = node;
    node->left->id = node->id << 1;
  }
  if (node->right == NULL) {
    node->right = new treenode();
    node->right->f = node;
    node->right->id = (node->id << 1) + 1;
  }
  if (node->final_left == NULL) {
    node->final_left = new treenode();
    node->final_left->f = node;
    node->final_left->id = node->id << 1;
  }
  if (node->final_right == NULL) {
    node->final_right = new treenode();
    node->final_right->f = node;
    node->final_right->id = (node->id << 1) + 1;
  }
  if (node->label == NULL) node->label = new int[this->n];

  double temp = 0.0;

  // Special Initialization for root node
  if (node->id == 1)
    {
      for (unsigned int i = 0; i < this->n; i++)
	{
	  node->label[i] = 3;
	}
    }
  for (unsigned int i = 0; i < this->p; i++)
    {
      for (unsigned int j = 0; j < this->n; j++)
	{
	  if ((node->f!=NULL&&node->f->label[this->feature_indice[i][j] - 1] == node->id)||(node->f==NULL))
	    {
	      node->label[this->feature_indice[i][j] - 1] =( node->id << 1) + 1;
	    }
	  else if(node->f!=NULL)
	    {
	      node->label[this->feature_indice[i][j] - 1] = 0;
	    }
	}
		
      for (unsigned int b = 0; b < this->n; b++)
	{
	  if (node->label[this->feature_indice[i][b] - 1] == 0)
	    {
	      continue;
	    }
	  else
	    {
	      node->label[this->feature_indice[i][b] - 1] = node->id << 1;
	    }
	  if (b % this->d == 0) {
	    if (b + 1 == n || feature[feature_indice[i][b] - 1][i] != feature[feature_indice[i][b + 1] - 1][i]) {
	      if (node->id < (unsigned int)std::pow(2, this->l - 1))
		{
		  learn_from_data(node->left);
		  learn_from_data(node->right);

		}
	      temp = node->left->subtree_reward + node->right->subtree_reward;
	      if (temp > node->subtree_reward)
		{
		  if (node->id == 1)
		    int x = 1;
		  node->subtree_reward = temp;
		  node->data.first = i;
		  node->data.second = this->feature[this->feature_indice[i][b] - 1][i];
		  this->treenode_cpy(node->right, node->final_right);
		  this->treenode_cpy(node->left, node->final_left);
		}
	    }
	  }
	}
    }
  return;
}

void policy::leaf_learning(treenode * node)
{
  node->subtree_reward = 0;
  if (node->left == NULL) {
    node->left = new treenode();
    node->left->id = node->id << 1;
    node->left->f = node;
  }
  if (node->right == NULL) {
    node->right = new treenode();
    node->right->id = (node->id << 1) + 1;
    node->right->f = node;
  }
  if (node->final_left == NULL) {
    node->final_left = new treenode();
    node->final_left->data.first = -1;
    node->final_left->f = node;
  }
  if (node->final_right == NULL) {
    node->final_right = new treenode();
    node->final_right->data.first = -1;
    node->final_right->f = node;
  }

  double temp = 0;
  int label_num;

  for (unsigned int i = 0; i < this->p; i++) {

    label_num = -1;
    for (int j = 0; j < this->n; j++) {
      if (node->f==NULL||node->f->label[feature_indice[i][j] - 1] == node->id) {
	label_num++;
	this->label_in[label_num] = feature_indice[i][j] - 1;
      }
    }
    if (label_num == -1) break;

    for (int kk = 0; kk < this->k; kk++) {
      if (label_num >= 0) {
	sum_reward[0][kk] = reward[label_in[0]][kk];
      }
      else sum_reward[0][kk] = 0;
      for (int j = 1; j <= label_num; j++)
	sum_reward[j][kk] = 0;
    }
    for (int kk = 0; kk < this->k; kk++) {
      for (int j = 1; j <= label_num; j++) {
	sum_reward[j][kk] = sum_reward[j - 1][kk] + reward[label_in[j]][kk];
      }
    }
    for (int b = 0; b <= label_num; b++) {
      node->left->subtree_reward = 0;
      node->right->subtree_reward = 0;
      for (int kk = 0; kk < this->k; kk++) {
	if (sum_reward[b][kk] > node->left->subtree_reward && (b == label_num || feature[label_in[b]][i] != feature[label_in[b + 1]][i])) {
	  node->left->subtree_reward = sum_reward[b][kk];
	  node->left->data.second = kk;
	}
	if ((sum_reward[label_num][kk] - sum_reward[b][kk]) > node->right->subtree_reward && (b == label_num || feature[label_in[b]][i] != feature[label_in[b + 1]][i])) {
	  node->right->subtree_reward = sum_reward[label_num][kk] - sum_reward[b][kk];
	  node->right->data.second = kk;
	}
      }
      temp = node->left->subtree_reward + node->right->subtree_reward;
      if (temp > node->subtree_reward ) {
	node->subtree_reward = temp;
	node->data.first = i;
	node->data.second = feature[label_in[b]][i];
	this->treenode_cpy(node->right, node->final_right);
	this->treenode_cpy(node->left, node->final_left);
      }
    }
  }

  return;
}

template <class T>
matrix<T> policy::transpose(matrix<T> input, unsigned int m, unsigned int n)
{
  matrix<T> retval;

  retval = new T *[n];
  for (unsigned int i = 0; i < n; i++)
    {
      retval[i] = new T[m];
    }

  for (unsigned int i = 0; i < n; i++)
    {
      for (unsigned int j = 0; j < m; j++)
	{
	  retval[i][j] = input[j][i];
	}
    }

  return retval;
}

void policy::quicksort(unsigned int *label, double *a, int l, int r)
{
  int ll = l, rr = r;
  double mid = a[(l + r) / 2];
  double temp;
  unsigned int temp2;
  if (l==r) return;
  while (ll<=rr){
    while (a[ll]<mid) ll++;
    while (a[rr]>mid) rr--;
    if (ll<=rr){
      temp=a[ll];
      a[ll]=a[rr];
      a[rr]=temp;
      temp2 = label[ll];
      label[ll] = label[rr];
      label[rr] = temp2;
      ll++;
      rr--;
    }
  }
  if (l<ll) quicksort(label, a, l,rr);
  if (r>rr) quicksort(label, a, ll,r);
  return;
}

void policy::delete_tree(treenode * root)
{
  if (root == NULL)
    {
      return;
    }
  if (root->label != NULL) delete(root->label);
  delete_tree(root->left);
  delete_tree(root->right);
  delete_tree(root->final_left);
  delete_tree(root->final_right);
  delete root;
}

treenode::treenode()
{
  this->subtree_reward = 0.0;
  this->data.first = -1;
  this->data.second = 0.0; 
  this->id = 0;
  this->left = NULL;
  this->right = NULL;
  this->final_left = NULL;
  this->final_right = NULL;
  this->label = NULL;
}

void policy::treenode_cpy(treenode * src, treenode * dst)
{
  dst->data = src->data;
  dst->subtree_reward = src->subtree_reward;
  dst->id = src->id;
  dst->label = src->label;

  if (src->left != NULL)
    {
      if (dst->left == NULL)
	{
	  dst->left = new treenode();
	}
      treenode_cpy(src->left, dst->left);
    }
  else
    {
      dst->left = NULL;
    }
  if (src->right != NULL)
    {
      if (dst->right == NULL)
	{
	  dst->right = new treenode();
	}
      treenode_cpy(src->right, dst->right);
    }
  else
    {
      dst->right = NULL;
    }
  if (src->final_left != NULL)
    {
      if (dst->final_left == NULL)
	{
	  dst->final_left = new treenode();
	}
      treenode_cpy(src->final_left, dst->final_left);
    }
  else
    {
      dst->final_left = NULL;
    }
  if (src->final_right != NULL)
    {
      if (dst->final_right == NULL)
	{
	  dst->final_right = new treenode();
	}
      treenode_cpy(src->final_right, dst->final_right);
    }
  else
    {
      dst->final_right = NULL;
    }
}

double policy::calculate(treenode *a)
{
  double sum = 0;
  for (int i = 0; i < this->n; i++) {
    sum += classify(a, i);
  }
  return sum;
}

double policy::classify(treenode *a, int i)
{
  if (a->data.first == -1) return this->reward[i][int(a->data.second)];
  if (this->feature[i][a->data.first] < (a->data.second + 0.1)) return(classify(a->final_left, i));
  else return(classify(a->final_right, i));
}
