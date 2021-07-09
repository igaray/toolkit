#include <algorithm>
#include <iomanip>
#include <iostream>
#include <limits>
#include <queue>
#include <vector>

using namespace std;

const int INF = numeric_limits<int>::max();

typedef struct max_flow_problem {
    int n, m, s, t;
    vector< vector<int> > residual;
    vector< vector<int> > adj;
} max_flow_problem;

max_flow_problem p;

void print_vector(vector<int> &v) {
    cout << "[ ";
    for (auto x : v) {
        cout << x << " ";
    }
    cout << "]";
}

void print_path(vector<int> &parent) {
    vector<int> path = vector<int>();
    int v;
    for (v = p.t; v != p.s; v = parent[v]) {
        path.push_back(v);
    }
    path.push_back(v);
    reverse(path.begin(), path.end());
    print_vector(path);
}

void print_max_flow_problem() {
    cout << "n: " << p.n << " m: " << p.m << endl;
    cout << "s: " << p.s << " t: " << p.t << endl;
    cout << "capacities:\n";
    for (auto x : p.residual) {
        for (auto y : x) {
            cout << setw(3) << y << " ";
        }
        cout << endl;
    }
    cout << "adjacency list:\n";
    for (int i = 0; i < p.n; i++) {
        cout << "  " << i << ": ";
        print_vector(p.adj[i]);
        cout << endl;
    }
}

void input() {
    cin >> p.n >> p.m;
    cin >> p.s >> p.t;
    p.s--;
    p.t--;
    // initialize capacities
    p.residual = vector< vector<int> >(p.n);
    for (int i = 0; i < p.n; i++) {
        p.residual[i] = vector<int>(p.n, 0);
    }
    // initialize adjacency list
    p.adj = vector< vector<int> >(p.n);
    for (int i = 0; i < p.n; i++) {
        p.adj[i] = vector<int>();
    }
    // read edges
    for (int i = 0; i < p.m; i++) {
        int u, v, c;
        cin >> u >> v >> c;
        u--;
        v--;
        p.residual[u][v] = c;
        if (find(p.adj[u].cbegin(), p.adj[u].cend(), v) == p.adj[u].end()) {
            p.adj[u].push_back(v);
        }
        if (find(p.adj[v].cbegin(), p.adj[v].cend(), u) == p.adj[v].end()) {
            p.adj[v].push_back(u);
        }
    }
}

bool bfs(vector<int> &parent) {
    fill(parent.begin(), parent.end(), -1);
    vector<bool> visited = vector<bool>(p.n, false);

    queue<int> q;
    q.push(p.s);

    while (!q.empty()) {
        int u = q.front();
        q.pop();

        for (int v : p.adj[u]) {
            if (!visited[v] && p.residual[u][v] > 0) {
                parent[v] = u;
                visited[v] = true;
                if (v == p.t) {
                    return true;
                }
                q.push(v);
            }
        }
    }
    return false;
}

int edmond_karp() {
    int max_flow = 0;
    vector<int> parent(p.n);

    while (bfs(parent)) {
        int path_flow = INF;
        for (int v = p.t; v != p.s; v = parent[v]) {
            int u = parent[v];
            path_flow = min(path_flow, p.residual[u][v]);
        }
        for (int v = p.t; v != p.s; v = parent[v]) {
            int u = parent[v];
            p.residual[u][v] -= path_flow;
            p.residual[v][u] += path_flow;
        }
        max_flow += path_flow;
    }
    return max_flow;
}

int main() {
    input();
    // print_max_flow_problem();
    cout << edmond_karp();
    return 0;
}
