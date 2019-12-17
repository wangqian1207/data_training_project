#include "policy.h"
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <queue>

#define DEFAULT_FILENAME "tree2"
#define DEFAULT_EXT      ".txt"

void save_tree_levelorder(policy *policy, std::string filename);

int main(int argc, char** argv)
{
  // Arguments: feature file path, reward file path, start number, stop number
  if (argc < 3)
    {
      std::cout << "Lacking Arguments." << std::endl;
      return -1;
    }

  // Reading filename from arguments
  std::string feature_filename(argv[1]);
  std::string reward_filename(argv[2]);
  std::string ext_name(".txt");
      
  std::string feature_path = feature_filename + ext_name;
  std::string reward_path = reward_filename + ext_name;

  // Create ifstreams for opening files.
  std::ifstream feature_file;
  std::ifstream reward_file;

  std::cout << "Opening Feature and Reward file..." << std::endl;

  // Open files
  feature_file.open(feature_path);
  if (!feature_file.good())
    {
      std::cout << "Open feature file failed." << std::endl;
      return -3;
    }
  reward_file.open(reward_path);
  if (!reward_file.good())
    {
      std::cout << "Open reward file failed." << std::endl;
      return -3;
    }

  std::cout << "Open Feature and Reward file success." << std::endl;


  // Read n, p, k, l
  int n, p, k, l,dd;
  feature_file >> n;
  feature_file >> p;
  feature_file >> k;
  feature_file >> l;
  feature_file >> dd;

  // Allocate memory for test_feature and reward
  double **test_feature = NULL;
  double **test_reward = NULL;

  test_feature = new double*[n];
  test_reward = new double*[n];

  for (unsigned int i = 0; i < n; i++)
    {
      test_feature[i] = new double[p];
      test_reward[i] = new double[k];
    }

  // Read data from feature and reward files.
  double temp = 0.0;
  for (unsigned int i = 0; i < n; i++)
    {
      for (unsigned int j = 0; j < p; j++)
	{
	  feature_file >> temp;
	  test_feature[i][j] = temp;
	}
    }

  for (unsigned int i = 0; i < n; i++)
    {
      for (unsigned int j = 0; j < k; j++)
	{
	  reward_file >> temp;
	  test_reward[i][j] = temp;
	}
    }

  // Close files
  feature_file.close();
  reward_file.close();


  // Calculate and save tree to text files
  policy *test_policy = new policy(n, p, k, dd, test_feature, test_reward, l);
  test_policy->learn_from_data(test_policy->root);
  save_tree_levelorder(test_policy, std::string(DEFAULT_FILENAME) + std::string(DEFAULT_EXT));
  std::cout << "Test Reward is " << (test_policy->root)->subtree_reward << std::endl;

  return 0;
}

void save_tree_levelorder(policy *policy, std::string filename)
{
  std::ofstream out_file(filename);

  if(!out_file.is_open()){
    std::cout<<filename<<" not opened."<<std::endl;return;
  }
  std::cout << filename << " is opened." << std::endl;
  // Level-Order Traversal
  std::queue<treenode *> tree_queue;
  tree_queue.push(policy->root);
  while (!tree_queue.empty())
    {
      treenode *node = tree_queue.front();
      tree_queue.pop();

      out_file << node->id << " " << node->data.first << " " << node->data.second << std::endl; //" " << node->subtree_reward <<

      if (node->left != NULL)
        {
          tree_queue.push(node->left);
        }
      if (node->right != NULL)
        {
          tree_queue.push(node->right);
        }
    }
  out_file.close();
}
