all: policy.cpp policy_R.cpp
	g++ -O3 -std=c++11 -g policy.cpp policy_R.cpp -o ../policy_R

clean:
	$(RM) policy_R learn_result.txt

clean_data:
	$(RM) learn_result.txt
