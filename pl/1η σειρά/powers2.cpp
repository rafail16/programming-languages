#include <iostream>
#include <fstream>
#include <cmath>
using namespace std;
void FindAllElements(int n, int k) {
    int sum = k,c=0;
    int A[k];
    fill(A, A + k, 1);
    for (int i = k - 1; i >= 0; --i) {
        while (sum + A[i] <= n) {
            sum += A[i];
            A[i] *= 2;
        }
    }
    if (sum != n) cout << "[]"<<endl;
    else {
      int size=log2(A[k-1]);
      int hashtable[size+1];
      for (int i=0;i<size+1;i++) hashtable[i]=0;
      for (int i=0;i<k;++i) {
        if (A[i]==1) hashtable[0]++;
        else hashtable[int(log2(A[i]))]++;
      }
      for (int i=0;i<size+1;i++) {
        if (hashtable[i]!=0) c=i;
      }
      cout<<"[";
      for (int i = 0; i < c+1; ++i) {
        cout<<hashtable[i];
        if(i<c) cout<<",";
      }
      cout<<"]"<<endl;
    }
}// Driver code
int main(int argc, char** argv) {
  std::ios::sync_with_stdio(false);
  cin.tie(NULL);
  if (argc < 1) cout << "Txt file as input required\n";
  else
  {
    ifstream myfile;
    myfile.open(argv[1], std::ios_base::in);
    if (myfile.is_open())
    {
      int num;
      long  *n,*k;
      myfile >> num;
      n=new long [num],k=new long [num];
      if (n == nullptr || k==nullptr) return -1;
      for (int i=0 ; i < num ; i++) myfile >> n[i]>>k[i];
      myfile.close();
      for (int i=0;i<num;i++) FindAllElements(n[i],k[i]);
      delete[] k;
      delete[] n;
    }
    else cout << "Coulnd't open the file.\n";
  }
  return 0;
}
    