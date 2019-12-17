#include <Rcpp.h>
#include <queue>
#include "policy.h"
using namespace std;
// [[Rcpp::export]]
Rcpp::DataFrame caller(Rcpp::NumericMatrix feats,Rcpp::NumericMatrix reward,int n,int p,int k,int l,int d){
    double **test_feature = new double *[n];
    double **test_reward = new double *[n];

    for (unsigned int i = 0; i < n; i++)
    {
        test_feature[i] = new double[p];
        test_reward[i] = new double[k];
    }

    // Read data from feature and reward files.
    for (unsigned int i = 0; i < n; i++)
    {
        for (unsigned int j = 0; j < p; j++)
        {
            test_feature[i][j] = feats(i,j);
        }
    }

    for (unsigned int i = 0; i < n; i++)
    {
        for (unsigned int j = 0; j < k; j++)
        {
            test_reward[i][j] = feats(i,j);
        }
    }
    policy *test_policy = new policy(n, p, k, 1, test_feature, test_reward, l);
    test_policy->learn_from_data(test_policy->root);

    Rcpp::NumericVector v1(pow(2,l+1));
    Rcpp::NumericVector v2(pow(2,l+1));
    Rcpp::NumericVector v3(pow(2,l+1));
    int i = 0;
    std::queue<treenode *> tree_queue;
    tree_queue.push(test_policy->root);
    while (!tree_queue.empty())
    {
        treenode *node = tree_queue.front();
        tree_queue.pop();

        v1[i] = node->id;
        v2[i] = node->data.first;
        v3[i] = node->data.second;

        if (node->left != NULL)
        {
            tree_queue.push(node->final_left);
        }
        if (node->right != NULL)
        {
            tree_queue.push(node->final_right);
        }
        i++;
    }
    
    return Rcpp::DataFrame::create(Rcpp::Named("1")=v1,Rcpp::Named("2")=v2,Rcpp::Named("3")=v3);
}
