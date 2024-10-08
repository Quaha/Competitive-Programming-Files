////#pragma GCC target("avx2")
//#pragma GCC optimize("O3")
//
//#include <iostream>
//#include <vector>
//#include <cmath>
//#include <algorithm>
//#include <string>
//#include <map>
//#include <unordered_map>
//#include <set>
//#include <unordered_set>
//#include <queue>
//#include <deque>
//#include <bitset>
//#include <random>
//#include <fstream>
//#include <iomanip>
//#include <chrono>
//#include <numeric>
//#include <cassert>
//#include <cstring>
//
//#pragma comment(linker, "/STACK:1024000000")
//
//using namespace std;
//
////#include <ext/pb_ds/assoc_container.hpp> 
////#include <ext/pb_ds/tree_policy.hpp>  
////using namespace __gnu_pbds;
//
//using ll = long long;
//using ld = long double;
//
//#define int ll
//
////------------------------------usings------------------------------
//using ii = pair<int, int>;
//using vii = vector<pair<int, int>>;
//using vvii = vector<vector<pair<int, int>>>;
//using vvvii = vector<vector<vector<pair<int, int>>>>;
//
//using vb = vector<bool>;
//using vvb = vector<vector<bool>>;
//using vvvb = vector<vector<vector<bool>>>;
//using vvvvb = vector<vector<vector<vector<bool>>>>;
//
//using vi = vector<int>;
//using vvi = vector<vector<int>>;
//using vvvi = vector<vector<vector<int>>>;
//using vvvvi = vector<vector<vector<vector<int>>>>;
//
//using vc = vector<char>;
//using vvc = vector<vector<char>>;
//using vvvc = vector<vector<vector<char>>>;
//
//using vs = vector<string>;
//using vvs = vector<vector<string>>;
//using vvvs = vector<vector<vector<string>>>;
//
//using vld = vector<ld>;
//using vvld = vector<vector<ld>>;
//using vvvld = vector<vector<vector<ld>>>;
//
////------------------------------ defines ------------------------------
//
//#define fi first
//#define se second
//
//#define co continue
//#define pb push_back
//
//#define re return
//
//#define all(x) x.begin(), x.end()
//#define rall(x) x.rbegin(), x.rend()
//
//#define fori(M) for (int j = 0; j < M; ++j)
//#define forj(M) for (int last_lt = 0; last_lt < M; ++last_lt)
//#define fork(M) for (int V = 0; V < M; ++V)
//
//#define fori1(M) for (int j = 1; j < M; ++j)
//#define forj1(M) for (int last_lt = 1; last_lt < M; ++last_lt)
//#define fork1(M) for (int V = 1; V < M; ++V)
//
////------------------------------ << and >> ------------------------------
//template<typename T>
//std::istream& operator >> (std::istream& in, vector<T>& arr) {
//    for (T& object : arr) {
//        in >> object;
//    }
//    return in;
//}
//
//template<typename T>
//std::ostream& operator<<(std::ostream& out, const vector<T>& arr) {
//    for (const T& object : arr) {
//        out << object << ' ';
//    }
//    return out;
//}
//
//template<typename T>
//std::ostream& operator<<(std::ostream& out, const vector<vector<T>>& arr) {
//    for (const vector<T>& object : arr) {
//        out << object << '\n';
//    }
//    return out;
//}
//
//template<typename T1, typename T2>
//std::istream& operator >> (std::istream& in, pair<T1, T2>& p) {
//    in >> p.first >> p.second;
//    return in;
//}
//
//template<typename T1, typename T2>
//std::ostream& operator<<(std::ostream& out, const pair<T1, T2>& p) {
//    out << p.first << ' ' << p.second << '\n';
//    return out;
//}
//
//template<typename T>
//std::ostream& operator<<(std::ostream& out, const set<T>& S) {
//    for (const T& V : S) {
//        cout << V << ' ';
//    }
//    return out;
//}
//
//template<typename T>
//void cin0(vector<T>& arr) {
//    for (int i = 0; i < arr.size(); ++i) {
//        cin >> arr[i];
//    }
//}
//
//template<typename T>
//void cin1(vector<T>& arr) {
//    for (int i = 1; i < arr.size(); ++i) {
//        cin >> arr[i];
//    }
//}
//
//template<typename T>
//void cout0(const vector<T>& arr) {
//    for (int i = 0; i < (int)arr.size() - 1; ++i) {
//        cout << arr[i] << ' ';
//    }
//    cout << arr.back() << ' ';
//}
//
//template<typename T>
//void cout0(const vector<vector<T>>& arr) {
//    for (int i = 0; i < arr.size(); ++i) {
//        for (int j = 0; j < (int)arr[i].size() - 1; ++j) {
//            cout << arr[i][j] << ' ';
//        }
//        cout << arr[i].back() << '\n';
//    }
//}
//
//template<typename T>
//void cout1(const vector<T>& arr) {
//    for (int i = 1; i < (int)arr.size() - 1; ++i) {
//        cout << arr[i] << ' ';
//    }
//    cout << arr.back() << '\n';
//}
//
//template<typename T>
//void cout1(const vector<vector<T>>& arr) {
//    for (int i = 1; i < arr.size(); ++i) {
//        for (int j = 1; j < (int)arr[i].size() - 1; ++j) {
//            cout << arr[i][j] << ' ';
//        }
//        cout << arr[i].back() << '\n';
//    }
//}
//
////------------------------------ min, max and summed ------------------------------
//template<typename T>
//T summed(const vector<T>& arr) {
//    if (arr.size() == 0) {
//        return 0;
//    }
//    T V = arr[0];
//    for (int i = 1; i < arr.size(); ++i) {
//        V += arr[i];
//    }
//    return V;
//}
//
//template<typename T>
//T max(const vector<T>& arr) {
//    T V = arr[0];
//    for (int i = 1; i < arr.size(); ++i) {
//        V = max(V, arr[i]);
//    }
//    return V;
//}
//
//template<typename T>
//T max(const vector<vector<T>>& arr) {
//    T V = max(arr[0]);
//    for (int i = 1; i < arr.size(); ++i) {
//        V = max(V, max(arr[i]));
//    }
//    return V;
//}
//
//template<typename T>
//T min(const vector<T>& arr) {
//    T V = arr[0];
//    for (int i = 1; i < arr.size(); ++i) {
//        V = min(V, arr[i]);
//    }
//    return V;
//}
//
//template<typename T>
//void setmax(T& V1, const T& V2) {
//    V1 = max(V1, V2);
//}
//
//template<typename T>
//void setmin(T& V1, const T& V2) {
//    V1 = min(V1, V2);
//}
//
////------------------------------ sort and reverse ------------------------------
//template<typename T>
//void sort(T& obj) {
//    sort(all(obj));
//}
//
//template<typename T>
//void usort(T& obj) {
//    sort(all(obj));
//    int p = 0;
//    for (int i = 1; i < obj.size(); ++i) {
//        if (obj[i] != obj[p]) {
//            obj[++p] = obj[i];
//        }
//    }
//    obj.resize(p + 1);
//}
//
//template<typename T>
//void rsort(T& obj) {
//    sort(rall(obj));
//}
//
//template<typename T>
//void reverse(T& obj) {
//    reverse(all(obj));
//}
//
//template<typename T>
//T sorted(T obj) {
//    sort(obj);
//    return obj;
//}
//
//template<typename T>
//T usorted(T obj) {
//    usort(obj);
//    return obj;
//}
//
//template<typename T>
//T rsorted(T obj) {
//    rsort(obj);
//    return obj;
//}
//
//template<typename T>
//T reversed(T obj) {
//    reverse(obj);
//    return obj;
//}
//
////------------------------------ math ------------------------------
//int sgn(int V) {
//    if (V < 0) {
//        return -1;
//    }
//    if (V > 0) {
//        return 1;
//    }
//    return 0;
//}
//
//int gcd(int A, int B) {
//    while (B > 0) {
//        A %= B;
//        swap(A, B);
//    }
//    return A;
//}
//
//int fastPow(int V, int a, int MOD) {
//    int res = 1;
//    while (a != 0) {
//        if (a & 1) {
//            res *= V;
//            res %= MOD;
//        }
//        V *= V;
//        V %= MOD;
//        a >>= 1;
//    }
//    return res;
//}
//
//int getMex(const vector<int>& arr) {
//    int N = arr.size();
//    vector<bool> cnt_sort(N + 1, true);
//    for (int V : arr) {
//        if (V < N) {
//            cnt_sort[V] = false;
//        }
//    }
//    for (int i = 0; i <= N; i++) {
//        if (cnt_sort[i]) return i;
//    }
//}
//
//bool isPrime(int N) {
//    if (N < 2) {
//        return false;
//    }
//    for (int i = 2; i * i <= N; i++) {
//        if (N % i == 0) {
//            return false;
//        }
//    }
//    return true;
//}
//
////------------------------------code------------------------------
//void solve();
//void precalculation();
//
////mt19937 mt_rand(chrono::steady_clock::now().time_since_epoch().count());
//
////typedef tree<ll, null_type, less<ll>, rb_tree_tag, tree_order_statistics_node_update> ordered_set;
//
//int32_t main() {
//
//    //srand(time(0));
//
//    ios_base::sync_with_stdio(false);
//    cin.tie(nullptr);
//    cout.tie(nullptr);
//    //cout << fixed << setprecision(2);
//    //cout.precision(20);
//
//    //freopen("math.in", "r", stdin);
//    //freopen("math.out", "w", stdout);
//
//    precalculation();
//
//    int __TESTS_IN_TASK__ = 1;
//    //cin >> __TESTS_IN_TASK__;
//
//    for (int __TEST_CASE__ = 1; __TEST_CASE__ <= __TESTS_IN_TASK__; __TEST_CASE__++) {
//        solve();
//    }
//
//    return 0;
//}
//
//#ifdef int:
//// Now int is long long!
//#endif
//
//const int INF = 2e15;
//const int MOD = 998244353;
//
//void precalculation() {}
//
//void solve() {
//
//
//
//}