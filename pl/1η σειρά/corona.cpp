#include <iostream>
#include <queue>
#include <fstream>
#include <vector>
#include <algorithm>

using namespace std; 

class Graph { 
    int V;    // No. of vertices 
       // Pointer to an array containing adjacency lists 
public: 
    vector<int> *adj; 
    Graph(int V);   // Constructor 
    void addEdge(int v, int w);   // to add an edge to graph 
}; 
  
Graph::Graph(int V) { 
    this->V = V; 
    adj = new vector<int>[V+1]; 
} 
  
void Graph::addEdge(int v, int w) { 
    adj[v].push_back(w); // Add w to v’s list. 
    adj[w].push_back(v); // Add v to w’s list. 
}

// Merges two subvectors of arr. 
// First subarray is arr[l..m] 
// Second subarray is arr[m+1..r] 
void merge(vector<int> &arr, int l, int m, int r) { 
    int i, j, k; 
    int n1 = m - l + 1; 
    int n2 =  r - m; 
    /* create temp arrays */
    int L[n1], R[n2]; 
    /* Copy data to temp arrays L[] and R[] */
    for (i = 0; i < n1; i++) L[i] = arr[l + i]; 
    for (j = 0; j < n2; j++) R[j] = arr[m + 1+ j]; 
  
    /* Merge the temp arrays back into arr[l..r]*/
    i = 0; // Initial index of first subarray 
    j = 0; // Initial index of second subarray 
    k = l; // Initial index of merged subarray 
    while (i < n1 && j < n2) { 
        if (L[i] <= R[j]) { 
            arr[k] = L[i]; 
            i++; 
        } 
        else{ 
            arr[k] = R[j]; 
            j++; 
        } 
        k++; 
    } 
    /* Copy the remaining elements of L[], if there are any */
    while (i < n1) { 
        arr[k] = L[i]; 
        i++; 
        k++; 
    } 
    /* Copy the remaining elements of R[], if there are any */
    while (j < n2) { 
        arr[k] = R[j]; 
        j++; 
        k++; 
    } 
} 
  
/* l is for left index and r is right index of the 
   sub-array of arr to be sorted */
void mergeSort(vector<int> &arr, int l, int r) { 
    if (l < r) { 
        // Same as (l+r)/2, but avoids overflow for 
        // large l and h 
        int m = l+(r-l)/2; 
        // Sort first and second halves 
        mergeSort(arr, l, m); 
        mergeSort(arr, m+1, r); 
        merge(arr, l, m, r); 
    } 
} 

void nodesInCycle(vector<int> &result, vector<int> par, vector<bool> &inCycle){
    int u  = result[0], v = result[1];
    while(u != v && par[u] != v && par[v] != u){
        u = par[u];
        v = par[v];
        if(u != v) {
            result.push_back(v);
            inCycle[v] = true;
        }
        result.push_back(u);
        inCycle[u] = true;
    }
}
void bfs_cycle(Graph &graph, int u, int n, vector<int> &result, vector<int> &par, vector<bool> &inCycle) { 
    vector<bool> visited(n+1);
    bool flag = true;
    queue<int> q;
    int cyclenumber = 0;
    visited[u] = true;
    q.push(u);
    par[u] = 0;
    while(!q.empty() && flag){
        u = q.front();
        q.pop();
        for(int v : graph.adj[u]){
            if(!visited[v]){
                visited[v] = true;
                par[v] = u;
                q.push(v);
            }
            else if (par[u] != v){
                if (!inCycle[v] || !inCycle[u]) if (cyclenumber) flag = false;
                if (inCycle[v] && inCycle[u]) continue;
                cyclenumber++;
                result.push_back(v);
                result.push_back(u);
                inCycle[v] = true;
                inCycle[u] = true;
                nodesInCycle(result, par, inCycle);
            }
        }
    }
    if (!flag) result.clear();
    return;
}
int bfs(Graph &graph, int u, vector<bool> inCycle, vector<bool> &visited, int n){
    queue<int> q;
    int start = u;
    int counter = 0;
    q.push(u);
    while (!q.empty()){
        u = q.front();
        q.pop();
        counter++;
        for(int v : graph.adj[u]){
            if(!visited[v]){
                visited[v] = true;
                q.push(v);
            }
            else if (inCycle[v] || u == start) continue;
        }
    }
    return counter;
}


void checkCorona (Graph &graph, vector<int> cycle, vector<bool> &inCycle, vector<bool> &visited, int n) {
    vector<int> result;
    int temp = 0, sum = 0;   
    for(int i : cycle) {
        temp = bfs(graph, i, inCycle, visited, n);
        if (temp == 0) {
            cout<<"NO CORONA\n";
            return;
        }
        sum += temp;
        result.push_back(temp);       
    }
    if(sum != n){
        cout<<"NO CORONA\n";
        return;
    }
    int size = result.size();
    mergeSort(result, 0, size - 1);
    
    cout<<"CORONA "<<size<<endl;
    for (auto i = 0; i < size; i++){
        cout<<result[i];
        if(i != size-1) cout<<' ';
    }
    cout<<endl;
}
int N;;
vector<bool> inCycle(N+1);
vector<bool> visited(N+1);
// Driver Code 

int main(int argc, char** argv) { 
    std::ios::sync_with_stdio(false);
    cin.tie(NULL);
    ifstream myfile;
    myfile.open(argv[1], std::ios_base::in);
    if (myfile.is_open()){
        int T, N, M;
        myfile>>T;
        for (int i = 0; i < T; i++){
            myfile>>N>>M;
            Graph g(N);
            int u, v;
            for (int j = 0; j < M; j ++){
                myfile>>u>>v;
                g.addEdge(u, v);
            }
            vector<int> par(N+1); 
            vector<bool> inCycle(N+1);

             // call DFS to mark the cycles 
            vector<int> cycle;
            bfs_cycle(g, 1, N, cycle, par, inCycle); 
            checkCorona (g, cycle, inCycle, inCycle, N);
        }
        myfile.close();
    }
    else cout<< "Couln't open the file.\n";
    return 0;
}