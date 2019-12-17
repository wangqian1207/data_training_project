#include <Rcpp.h>

using namespace Rcpp;
class Demo{
private:
  double** cc;
public:
  Demo();
  ~Demo( );
  int* fun1(int*);
  double* get();
};
int* Demo::fun1(int* a){
  a[0]=10;
  return a;
}
Demo::Demo(){
  cc = new double*[2];
  cc[0]=new double[2];
  cc[0][0]=1;
  cc[0][1]=2;
}
Demo::~Demo(){
  delete cc[0];
  delete cc;
}
double* Demo::get(){
  return cc[0];
}
// [[Rcpp::export]]
double fun2(Rcpp::NumericVector a){
  Demo demo;
  int* aa = new int[a.size()];
  for (int i=0;i<a.size();i++) {
    aa[i] = a[i];
  }
  if(aa[0]==demo.fun1(aa)[0]){
    return demo.get()[0];
  }
  else return demo.get()[1];
}