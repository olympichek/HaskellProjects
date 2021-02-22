#include <iostream>
#include <vector>
#include <list>
using namespace std;

list<int> get_primes(int m, int n) {
    vector<bool> bool_arr(n-1, true);
    for(int i = 2; i*i <= n; i++) {
        for(int j = i*i; j <= n; j += i) {
            bool_arr[j-2] = false;
        }
    }
    list<int> primes;
    for(int i = max(0, m-2); i < n-1; i++) {
        if(bool_arr[i])
            primes.push_back(i+2);
    }
    return primes;
}

int main() {
    int m, n;
    cin >> m >> n;
    auto primes = get_primes(m, n);
    if(primes.size() == 0) {
        cout << "Absent\n";
    }
    else {
        for(auto &i : primes)
            cout << i << "\n";
    }
    return 0;
}