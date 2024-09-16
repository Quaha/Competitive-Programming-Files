#pragma once
#include <iostream>
#include <vector>
#include <cmath>
#include <algorithm>
#include <string>
#include <map>
#include <unordered_map>
#include <set>
#include <unordered_set>
#include <queue>
#include <deque>
#include <bitset>
#include <random>

using namespace std;

// --------------- Trees --------------- 

template<typename TreeType> struct FenwickTree {

	// 0-indexing

	int N;
	vector<TreeType> tree;

	FenwickTree(int size) {
		N = size;
		tree.resize(N, 0);
	}

	FenwickTree(const vector<TreeType>& arr) {
		N = arr.size();
		tree.resize(N, 0);
		for (int i = 0; i < N; i++) {
			add(arr[i], i);
		}
	}

	void add(TreeType V, int id) { // data[id] += V
		while (id < N) {
			tree[id] += V;
			id = (id | (id + 1));
		}
	}

	TreeType sum(int id) const {
		TreeType res = 0;
		while (id >= 0) {
			res += tree[id];
			id = (id & (id + 1)) - 1;
		}
		return res;
	}

	TreeType sum(int l, int r) const {
		return sum(r) - sum(l - 1);
	}
};

template<typename TreeType> struct PowerfullFenwickTree {

	// 1-indexing!

	int N;
	vector<TreeType> tree;

	const int log2 = 23;

	PowerfullFenwickTree(int size) {
		N = size;
		tree.resize(N + 1, 0);
	}

	PowerfullFenwickTree(const vector<TreeType>& arr) { // arr in 0-indexing!
		N = arr.size();
		tree.resize(N + 1, 0);
		for (int i = 0; i < N; i++) {
			add(arr[i], i + 1);
		}
	}

	// k & -k   is the first 1 in the binary of k
	void add(TreeType V, int id) {
		while (id <= N) {

			tree[id] += V;
			id += (id & -id);
		}
	}

	TreeType sum(int id) {
		TreeType res = 0;
		while (id > 0) {

			res += tree[id];
			id -= (id & -id);
		}
		return res;
	}

	TreeType sum(int l, int r) {
		return sum(r) - sum(l - 1);
	}

	int lower_bound(TreeType sum) { // return the first index such as prefs[index] >= sum
		int pos = 0;
		for (int k = log2; k >= 0; --k) {
			int next_pos = pos | (1ll << k);
			if (next_pos <= N && tree[next_pos] < sum) {
				sum -= tree[next_pos];
				pos = next_pos;
			}
		}
		return pos + 1;
	}

	int upper_bound(TreeType sum) { // return the last index such as prefs[index] >= sum
		int pos = 0;
		for (int k = log2; k >= 0; --k) {
			int next_pos = pos | (1ll << k);
			if (next_pos <= N && tree[next_pos] <= sum) {
				sum -= tree[next_pos];
				pos = next_pos;
			}
		}
		return pos + 1;
	}
};

template<typename TreeType> struct SegmentTree {

	const static inline TreeType FILLER = 0;
	TreeType func(const TreeType& A, const TreeType& B) {
		return A + B;
	}

	int size = 0;
	vector<TreeType> tree;

	void __update(const TreeType& value, int i, int __l, int __r, int __i) { // [l, r), 0-ind
		if (__r <= i || i < __l) {
			return;
		}
		if (__l == __r - 1) {
			tree[__i] = value;
			return;
		}
		int __m = (__l + __r) / 2;
		__update(value, i, __l, __m, __i * 2 + 1);
		__update(value, i, __m, __r, __i * 2 + 2);
		tree[__i] = func(tree[__i * 2 + 1], tree[__i * 2 + 2]);
	}

	TreeType __get(int l, int r, int __l, int __r, int __i) { // [l, r), 0-ind
		if (l <= __l && __r <= r) {
			return tree[__i];
		}
		if (__l >= r || __r <= l) {
			return FILLER;
		}
		int __m = (__l + __r) / 2;
		TreeType value1 = __get(l, r, __l, __m, __i * 2 + 1);
		TreeType value2 = __get(l, r, __m, __r, __i * 2 + 2);
		return func(value1, value2);
	}

	SegmentTree(const vector<TreeType>& arr) {
		size = arr.size();
		tree.resize(4 * size, FILLER);
		for (int i = 0; i < size; i++) {
			update(arr[i], i);
		}
	}
	SegmentTree(int N, const TreeType& value) {
		size = N;
		tree.resize(4 * size, value);
	}

	void update(const TreeType& value, int i) { // 0-ind
		__update(value, i, 0, size, 0);
	}

	TreeType sum(int l, int r) { // [l, r], 0-ind
		return __get(l, r + 1, 0, size, 0);
	}
};

template<typename TreeType> struct RangeSegmentTree {

	const static inline TreeType FILLER = 0;

	struct Data {
		TreeType value = FILLER;
		int id = 0;
	};

	struct Operation {
		int type = 0;
		int value = 0;
	};

	struct Node {
		Data data;
		Operation operation;
	};

	const static inline Node FILLER_NODE = { FILLER, 0 };

	int size = 0;
	vector<Node> tree;

	Data func(const Data& A, const Data& B) {
		if (A.value > B.value) {
			return A;
		}
		return B;
	}

	void __propagate(int __i, int __l, int __r) {
		if (tree[__i].operation.type == 0) {
			tree[__i].data.value += tree[__i].operation.value;
		}

		if (__r - __l > 1) {
			if (tree[__i].operation.type == 0) {
				tree[__i * 2 + 1].operation.value += tree[__i].operation.value;
				tree[__i * 2 + 2].operation.value += tree[__i].operation.value;
			}
		}

		tree[__i].operation.type = 0;
		tree[__i].operation.value = 0;
	}

	void __init(int __i, int __l, int __r) {
		tree[__i].data.id = __l;
		if (__r - __l > 1) {
			int __m = (__l + __r) / 2;
			__init(__i * 2 + 1, __l, __m);
			__init(__i * 2 + 2, __m, __r);
		}
	}

	void __update(const TreeType& value, int l, int r, int type, int __i, int __l, int __r) {
		__propagate(__i, __l, __r);
		if (__r <= l || r <= __l) {
			return;
		}
		if (l <= __l && __r <= r) {
			tree[__i].operation.value = value;
			tree[__i].operation.type = type;
			__propagate(__i, __l, __r);
			return;
		}
		int __m = (__l + __r) / 2;
		__update(value, l, r, type, __i * 2 + 1, __l, __m);
		__update(value, l, r, type, __i * 2 + 2, __m, __r);
		tree[__i].data = func(tree[__i * 2 + 1].data, tree[__i * 2 + 2].data);
	}

	Data __get(int l, int r, int __i, int __l, int __r) {
		__propagate(__i, __l, __r);
		if (__r <= l || r <= __l) {
			return { FILLER, 0 };
		}
		if (l <= __l && __r <= r) {
			return tree[__i].data;
		}
		int __m = (__l + __r) / 2;
		Data V1 = __get(l, r, __i * 2 + 1, __l, __m);
		Data V2 = __get(l, r, __i * 2 + 2, __m, __r);
		return func(V1, V2);
	}

	RangeSegmentTree(int N) {
		size = N;
		tree.resize(4 * N, FILLER_NODE);
		__init(0, 0, size);
	}

	void add(const TreeType& value, int l, int r) {
		__update(value, l, r + 1, 0, 0, 0, size);
	}

	Data sum(int l, int r) {
		return __get(l, r + 1, 0, 0, size);
	}

};

template<typename TreeType> struct SparseRangeSegmentTree {

	const static inline TreeType FILLER = 0;

	struct Data {
		TreeType value = FILLER;
	};

	Data DATA_FILLER = { FILLER };

	struct Operation {
		int type = 0; // 0 - addition, 1 - setting
		TreeType value = 0;
	};

	struct Node {
		Data data;
		Operation operation;
		int left_id = -1;
		int right_id = -1;
	};

	int size = 0;
	vector<Node> tree;
	int free_node = 1;

	void __initNode(int __i) {
		tree[__i].left_id = free_node;
		tree[__i].right_id = free_node + 1;
		free_node += 2;
	}

	bool __isInit(int __i) {
		if (tree[__i].left_id == -1) {
			return false;
		}
		return true;
	}

	Data func(const Data& A, const Data& B) {
		Data ans;
		ans.value = A.value + B.value;
		return ans;
	}

	void __propagate(int __i, int __l, int __r) {
		if (tree[__i].operation.type == 0) {
			tree[__i].data.value += tree[__i].operation.value * (__r - __l);
		}
		if (tree[__i].operation.type == 1) {
			tree[__i].data.value = tree[__i].operation.value * (__r - __l);
		}

		if (__r - __l > 1) {
			if (tree[__i].operation.type == 0) {
				tree[tree[__i].left_id].operation.value += tree[__i].operation.value;
				tree[tree[__i].right_id].operation.value += tree[__i].operation.value;
			}
			if (tree[__i].operation.type == 1) {
				tree[tree[__i].left_id].operation.value = tree[__i].operation.value;
				tree[tree[__i].right_id].operation.value = tree[__i].operation.value;
				tree[tree[__i].left_id].operation.type = 1;
				tree[tree[__i].right_id].operation.type = 1;
			}
		}

		tree[__i].operation.value = 0;
		tree[__i].operation.type = 0;

	}

	void __update(const TreeType& value, int l, int r, int type, int __i, int __l, int __r) {
		if (__r - __l > 1 && !__isInit(__i)) {
			__initNode(__i);
		}
		__propagate(__i, __l, __r);
		if (__r <= l || r <= __l) {
			return;
		}
		if (l <= __l && __r <= r) {
			tree[__i].operation.value = value;
			tree[__i].operation.type = type;
			__propagate(__i, __l, __r);
			return;
		}
		int __m = (__l + __r) / 2;
		__update(value, l, r, type, tree[__i].left_id, __l, __m);
		__update(value, l, r, type, tree[__i].right_id, __m, __r);
		tree[__i].data = func(tree[tree[__i].left_id].data, tree[tree[__i].right_id].data);
	}

	Data __get(int l, int r, int __i, int __l, int __r) {
		if (__r - __l > 1 && !__isInit(__i)) {
			__initNode(__i);
		}
		__propagate(__i, __l, __r);
		if (__r <= l || r <= __l) {
			return DATA_FILLER;
		}
		if (l <= __l && __r <= r) {
			return tree[__i].data;
		}
		int __m = (__l + __r) / 2;
		Data V1 = __get(l, r, tree[__i].left_id, __l, __m);
		Data V2 = __get(l, r, tree[__i].right_id, __m, __r);
		return func(V1, V2);
	}

	SparseRangeSegmentTree(int N) {
		size = N;
		tree.resize(10 * N);
	}

	void add(const TreeType& value, int l, int r) {
		__update(value, l, r + 1, 0, 0, 0, size);
	}

	void set(const TreeType& value, int l, int r) {
		__update(value, l, r + 1, 1, 0, 0, size);
	}

	Data sum(int l, int r) {
		return __get(l, r + 1, 0, 0, size);
	}

};

template<typename TreeType> struct MergeSortTree {

	/* This structure allows you to quickly calculate some
	function from elements from an array segment from l to r
	if it works quickly on a sorted segment from l to r. */

	int func(const vector<TreeType>& arr, const TreeType& V) {
		int l = 0;
		int r = arr.size() - 1;
		if (V < arr[0]) {
			return arr.size();
		}
		if (arr.back() <= V) {
			return 0;
		}
		while (r - l > 1) {
			int m = (l + r) / 2;
			if (arr[m] <= V) {
				l = m;
			}
			else {
				r = m;
			}
		}
		return arr.size() - r;
	}

	int size = 0;
	vector<vector<TreeType>> tree;

	void __mergeSort(vector<TreeType>& arr, int __i) {
		if (arr.size() == 1) {
			tree[__i] = arr;
			return;
		}
		int m = arr.size() / 2;

		std::vector<TreeType> lefts(m);
		for (int i = 0; i < m; i++) {
			lefts[i] = arr[i];
		}

		__mergeSort(lefts, __i * 2 + 1);

		std::vector<TreeType> right(arr.size() - m);
		for (int i = m; i < arr.size(); i++) {
			right[i - m] = arr[i];
		}

		__mergeSort(right, __i * 2 + 2);

		for (int i = 0, l = 0, r = 0; i < arr.size(); i++) {
			if (l < lefts.size() && r < right.size()) {
				if (lefts[l] < right[r]) {
					arr[i] = lefts[l++];
				}
				else {
					arr[i] = right[r++];
				}
			}
			else if (l < lefts.size()) {
				arr[i] = lefts[l++];
			}
			else if (r < right.size()) {
				arr[i] = right[r++];
			}
		}

		tree[__i] = arr;
	}

	int __get(const TreeType& V, int l, int r, int __l, int __r, int __i) { // [l, r), 0-ind
		if (l <= __l && __r <= r) {
			return func(tree[__i], V);
		}
		if (__r <= l || r <= __l) {
			return 0;
		}
		int __m = (__l + __r) / 2;
		int V1 = __get(V, l, r, __l, __m, __i * 2 + 1);
		int V2 = __get(V, l, r, __m, __r, __i * 2 + 2);
		return V1 + V2;
	}

	MergeSortTree(vector<TreeType> arr) {
		size = arr.size();
		tree.resize(4 * size);
		__mergeSort(arr, 0);
	}

	int sum(const TreeType& V, int l, int r) { // [l, r], 0-ind
		return __get(V, l, r + 1, 0, size, 0);
	}
};

template<typename TreeType> struct AVL_Tree {

	struct Node {

		TreeType key = 0;
		int height = 0;

		Node* lefts = nullptr;
		Node* right = nullptr;

	};

	Node* head = nullptr;
	unsigned int tree_size = 0;

	void __initNode(Node* current_node, TreeType key) {
		tree_size++;

		current_node->key = key;
		current_node->height = 1;

		current_node->lefts = new Node;
		current_node->right = new Node;
	}

	bool __isInit(Node* current_node) const {
		return (current_node->lefts != nullptr) && (current_node->right != nullptr);
	}

	void __destroyTree(Node* current_node) {
		if (__isInit(current_node)) {
			__destroyTree(current_node->lefts);
			__destroyTree(current_node->right);
		}
		delete current_node;
	}

	void __smallLeftRotation(Node* current_node, Node* last_node) { // Left left node rotation
		Node* left_node = current_node->lefts;

		if (last_node != nullptr) {
			if (last_node->lefts == current_node) {
				last_node->lefts = left_node;
			}
			else {
				last_node->right = left_node;
			}
		}
		else {
			head = left_node;
		}

		current_node->lefts = left_node->right;
		left_node->right = current_node;

		current_node->height = max(current_node->lefts->height, current_node->right->height) + 1;
		left_node->height = max(left_node->lefts->height, left_node->right->height) + 1;
	}
	void __smallRightRotation(Node* current_node, Node* last_node) { // Right right node rotation
		Node* right_node = current_node->right;

		if (last_node != nullptr) {
			if (last_node->lefts == current_node) {
				last_node->lefts = right_node;
			}
			else {
				last_node->right = right_node;
			}
		}
		else {
			head = right_node;
		}

		current_node->right = right_node->lefts;
		right_node->lefts = current_node;

		current_node->height = max(current_node->lefts->height, current_node->right->height) + 1;
		right_node->height = max(right_node->lefts->height, right_node->right->height) + 1;
	}
	void __bigLeftRotation(Node* current_node, Node* last_node) { // Left right node rotation
		__smallRightRotation(current_node->lefts, current_node);
		__smallLeftRotation(current_node, last_node);
	}
	void __bigRightRotation(Node* current_node, Node* last_node) { // Right left node rotation
		__smallLeftRotation(current_node->right, current_node);
		__smallRightRotation(current_node, last_node);
	}

	void __insert(TreeType key, Node* current_node, Node* last_node) {
		if (!__isInit(current_node)) {
			__initNode(current_node, key);
			return;
		}
		if (key < current_node->key) {
			__insert(key, current_node->lefts, current_node);
		}
		if (key > current_node->key) {
			__insert(key, current_node->right, current_node);
		}

		int Hl = current_node->lefts->height;
		int Hr = current_node->right->height;

		if (abs(Hl - Hr) == 0) {
			return;
		}
		if (abs(Hl - Hr) == 1) {
			current_node->height++;
			return;
		}

		if (Hl > Hr) {
			int Hll = current_node->lefts->lefts->height;
			int Hlr = current_node->lefts->right->height;
			if (Hll > Hlr) {
				__smallLeftRotation(current_node, last_node);
			}
			else {
				__bigLeftRotation(current_node, last_node);
			}
		}
		else {
			int Hrl = current_node->right->lefts->height;
			int Hrr = current_node->right->right->height;
			if (Hrl > Hrr) {
				__bigRightRotation(current_node, last_node);
			}
			else {
				__smallRightRotation(current_node, last_node);
			}
		}
	}

	void __getAllElements(TreeType* ans, int& i, Node* current_node) const {
		if (!__isInit(current_node)) {
			return;
		}

		__getAllElements(ans, i, current_node->lefts);
		ans[i++] = current_node->key;
		__getAllElements(ans, i, current_node->right);
	}

	Node* __find(TreeType key) const {
		Node* current_node = head;
		while (__isInit(current_node) && current_node->key != key) {
			if (key < current_node->key) {
				current_node = current_node->lefts;
			}
			else if (key > current_node->key) {
				current_node = current_node->right;
			}
		}
		return current_node;
	}
	Node* __lower_bound(TreeType key) const {
		Node* ans = nullptr;
		Node* current_node = head;
		while (__isInit(current_node)) {
			if (current_node->key < key) {
				current_node = current_node->right;
			}
			else {
				ans = current_node;
				current_node = current_node->lefts;
			}
		}
		return ans;
	}

	AVL_Tree() {
		head = new Node;
	}
	AVL_Tree(const AVL_Tree& T) = delete;
	~AVL_Tree() {
		__destroyTree(head);
	}

	AVL_Tree& operator=(const AVL_Tree& T) = delete;

	bool isExists(TreeType key) const {
		return __isInit(__find(key));
	}

	void insert(TreeType key) { // Adds an element to the tree, if it is already there, then nothing will happen
		__insert(key, head, nullptr);
	}

	TreeType lower_bound(TreeType key) const { // Returns V >= key or nullptr if there is no suitable value
		Node* ans = __lower_bound(key);
		if (ans == nullptr) return nullptr;
		return ans->key;
	}

	unsigned int size() const {
		return tree_size;
	}
	bool empty() const {
		return tree_size == 0;
	}

	TreeType* getAllElements() { // Returns an allocated array with all elements in sorted order
		TreeType* ans = new int[tree_size];
		int i = 0;
		__getAllElements(ans, i, head);
		return ans;
	}
};

// --------------- Graphs ---------------

struct BinaryLifting {

	/*
	This structure allows you to find the smallest common ancestor (LCA)
	of two vertices in an oriented tree in log(N). Also allows you to find any
	commutative function on the path in the tree.
	*/

	int N = 0; // Number of vertices in the graph
	int deg = 0;
	int root = 0;
	vector<int> dist; // Dists from root to others
	vector<vector<int>> jmp; // jmp[vertex][number] = the 2^number ancerstor of the vertex

	BinaryLifting(const vector<vector<int>>& g, int root = 0) {
		N = g.size();
		this->root = root;
		dist.resize(N, 0);
		__getDist(root, 0, g);

		deg = 3 + log2(N);
		jmp.resize(N, vector<int>(deg, -1));


		// jumps at 1
		for (int i = 0; i < N; i++) {
			for (int j : g[i]) {
				jmp[j][0] = i;
			}
		}
		jmp[root][0] = root;

		// other jumps
		for (int k = 1; k < deg; k++) {
			for (int i = 0; i < N; i++) {
				jmp[i][k] = jmp[jmp[i][k - 1]][k - 1];
			}
		}
	}

	void __getDist(int curr_V, int curr_dist, const vector<vector<int>>& g) {
		dist[curr_V] = curr_dist;
		for (int next_V : g[curr_V]) {
			__getDist(next_V, curr_dist + 1, g);
		}
	}

	int getLCA(int u, int v) {
		if (dist[u] < dist[v]) {
			swap(u, v);
		}
		int d = dist[u] - dist[v];
		for (int i = deg - 1; i >= 0; i--) {
			if (d >= (1 << i)) {
				d -= (1 << i);
				u = jmp[u][i];
			}
		}

		if (u == v) {
			return u;
		}

		for (int i = deg - 1; i >= 0; i--) {
			if (jmp[u][i] != jmp[v][i]) {
				u = jmp[u][i];
				v = jmp[v][i];
			}
		}
		return jmp[u][0];
	}

};

// --------------- Containers ---------------

template<typename StackType> struct Stack {

	StackType func(const StackType& A, const StackType& B) {
		return min(A, B);
	}

	vector<StackType> main_stack;
	vector<StackType> func_stack;

	void push(const StackType& value) {
		main_stack.push_back(value);
		if (func_stack.empty()) {
			func_stack.push_back(value);
		}
		else {
			func_stack.push_back(func(func_stack.back(), main_stack.back()));
		}
	}

	void pop() {
		main_stack.pop_back();
		func_stack.pop_back();
	}

	int size() {
		return main_stack.size();
	}

	bool empty() {
		return main_stack.size() == 0;
	}

	StackType back() {
		return main_stack.back();
	}

	StackType getFuncValue() {
		return func_stack.back();
	}
};

template<typename QueueType> struct Queue {

	QueueType func(const QueueType& A, const QueueType& B) {
		return std::min(A, B);
	}

	Stack<QueueType> front_part;
	Stack<QueueType> back_part;

	void push(const QueueType& value) {
		back_part.push(value);
	}

	void pop() {
		if (front_part.empty()) {
			while (!back_part.empty()) {
				front_part.push(back_part.back());
				back_part.pop();
			}
		}
		front_part.pop();
	}

	int size() {
		return front_part.size() + back_part.size();
	}

	bool empty() {
		return (front_part.size() + back_part.size() == 0);
	}

	QueueType front() {
		if (front_part.isEmpty()) {
			while (!back_part.isEmpty()) {
				front_part.push(back_part.back());
				back_part.pop();
			}
		}
		return front_part.back();
	}

	QueueType getFuncValue() {
		if (front_part.empty()) {
			return back_part.getFuncValue();
		}
		if (back_part.empty()) {
			return front_part.getFuncValue();
		}
		return func(front_part.getFuncValue(), back_part.getFuncValue());
	}
};

// --------------- Special structures ---------------

template<typename TableType> struct SparseTable {

	TableType func(TableType A, TableType B) {
		return min(A, B);
	}

	vector<int> degrees;
	vector<int> pows = { 1 };
	vector<vector<TableType>> table;

	int N, M; // N - array size, M - log2(N)

	SparseTable(const vector<TableType>& arr) {
		N = arr.size();
		degrees.resize(N + 1, 0);

		int p = 0;
		int V = 1;
		for (int i = 0; i <= N; i++) {
			if (V * 2 <= i) {
				V *= 2;
				pows.push_back(V);
				p++;
			}
			degrees[i] = p;
		}

		M = degrees.back() + 1;

		table.resize(M, vector<TableType>(N));

		for (int i = 0; i < N; i++) {
			table[0][i] = arr[i];
		}

		for (int j = 1, deg = 0; j < M; j++, deg++) {
			for (int i = 0; i + 2 * pows[deg] - 1 < N; i++) {
				table[j][i] = func(table[j - 1][i], table[j - 1][i + pows[deg]]);
			}
		}
	}

	TableType sum(int l, int r) { // [l, r], 0-ind
		int deg = degrees[r - l + 1];
		return func(table[deg][l], table[deg][r - pows[deg] + 1]);
	}
};

struct BitBor {

	/*
	This structure allows you to find among a set of numbers such that x^ value is the maximum
	*/

	struct Node {

		Node* left_node = nullptr; // 0
		Node* right_node = nullptr; // 1

		int cnt = 0;
	};

	const int number_of_bits = 32;
	Node* head = nullptr;

	BitBor(int number_of_bits) : number_of_bits(number_of_bits) {
		head = new Node;
		head->cnt = 2e15 + 14;
	}

	void __add(int x, int current_bit, Node* current_node) {
		if (current_bit == 0) {
			return;
		}
		int next_bit = current_bit - 1;

		if (((1ll << next_bit) & x) == 0) {
			if (current_node->left_node == nullptr) {
				current_node->left_node = new Node;
			}
			current_node->left_node->cnt++;
			__add(x, next_bit, current_node->left_node);
		}
		else {
			if (current_node->right_node == nullptr) {
				current_node->right_node = new Node;
			}
			current_node->right_node->cnt++;
			__add(x, next_bit, current_node->right_node);
		}
	}

	void add(int x) {
		__add(x, number_of_bits, head);
	}

	int __get(int x, int current_bit, Node* current_node) const {
		if (current_bit == 0) {
			return x;
		}
		int next_bit = current_bit - 1;

		if (((1ll << next_bit) & x) == 0) {
			if (current_node->right_node == nullptr) {
				return __get(x, next_bit, current_node->left_node);
			}
			return __get(x ^ (1ll << next_bit), next_bit, current_node->right_node);
		}
		else {
			if (current_node->left_node == nullptr) {
				return __get(x ^ (1ll << next_bit), next_bit, current_node->right_node);
			}
			return __get(x, next_bit, current_node->left_node);
		}
	}

	int sum(int x) {
		if (head->left_node == nullptr && head->right_node == nullptr) {
			return 0;
		}
		return __get(x, number_of_bits, head);
	}

	void __erase(int x, int current_bit, Node* current_node) {
		if (current_bit == 0) {
			return;
		}
		int next_bit = current_bit - 1;

		if (((1ll << next_bit) & x) == 0) {
			__erase(x, next_bit, current_node->left_node);
			current_node->left_node->cnt--;
			if (current_node->left_node->cnt == 0) {
				delete current_node->left_node;
				current_node->left_node = nullptr;
			}
		}
		else {
			__erase(x, next_bit, current_node->right_node);
			current_node->right_node->cnt--;
			if (current_node->right_node->cnt == 0) {
				delete current_node->right_node;
				current_node->right_node = nullptr;
			}
		}
	}

	void erase(int x) { // dont erase elements which wasn't added
		__erase(x, number_of_bits, head);
	}
};

struct DSU {

	int cnt;
	vector<int> parent;
	vector<int> sizes;

	DSU(int N) {
		cnt = N;
		parent.resize(N, 0);
		for (int i = 0; i < N; i++) {
			parent[i] = i;
		}
		sizes.resize(N, 1);
	}

	int getParent(int x) {
		if (x == parent[x]) return x;
		return parent[x] = getParent(parent[x]);
	}

	void merge(int x, int y) {
		x = getParent(x);
		y = getParent(y);
		if (x == y) return;
		if (sizes[x] < sizes[y]) {
			swap(x, y);
		}
		sizes[x] += sizes[y];
		parent[y] = x;
		cnt--;
	}

	bool inAdjacent(int x, int y) {
		return getParent(x) == getParent(y);
	}

	int getComponentsCnt() {
		return cnt;
	}

	int getComponentSize(int x) {
		return sizes[getParent(x)];
	}

};

// --------------- Strings and Hashes ---------------

template<typename HashType> struct AmountHash {
	/*
	This class allows you to get a hash of a multiset
	of elements from an array segment from l to r
	*/

	inline static vector<int> MODS = { (int)1e9 + 7, (int)1e9 + 9 };

	int rand() {
		//return mt_rand();
	}

	inline static vector<unordered_map<HashType, int>> mapping;

	vector<vector<int>> hash;

	AmountHash(const vector<HashType>& arr) {

		if (mapping.empty()) {
			mapping.resize(MODS.size());
		}

		for (int i = 0; i < MODS.size(); i++) {
			for (const HashType& V : arr) {
				if (!mapping[i].count(V)) {
					mapping[i][V] = rand() % MODS[i];
				}
			}
		}
		hash.resize(MODS.size());
		for (int i = 0; i < (int)MODS.size(); i++) {
			hash[i].resize(arr.size() + 1, 0);
			for (int j = (int)arr.size() - 1; j >= 0; j--) {
				hash[i][j] = (hash[i][j + 1] + mapping[i][arr[j]]) % MODS[i];
			}
		}
	}

	vector<int> getHash(int l, int r) const { // [l, r], 0-ind
		vector<int> ans(MODS.size());
		for (int i = 0; i < MODS.size(); i++) {
			ans[i] = (hash[i][l] - hash[i][r + 1] + MODS[i]) % MODS[i];
		}
		return ans;
	}
};

struct PolynomialHash {

	inline static vector<int> MODS = { (int)1e9 + 7, (int)1e9 + 9 };
	inline static const int p = 239;
	inline static vector<vector<int>> ps;

	static void init(int N) {
		ps.resize(MODS.size());
		for (int i = 0; i < MODS.size(); i++) {
			ps[i].resize(N);
			ps[i][0] = 1;
			for (int j = 1; j < N; j++) {
				ps[i][j] = (ps[i][j - 1] * p) % MODS[i];
			}
		}
	}

	vector<vector<int>> hash;

	PolynomialHash(const string& S) {
		hash.resize(MODS.size());
		for (int i = 0; i < MODS.size(); i++) {
			hash[i].resize(S.size() + 1, 0);
			for (int j = (int)S.size() - 1; j >= 0; j--) {
				hash[i][j] = (S[j] + hash[i][j + 1] * p) % MODS[i];
			}
		}
	}

	vector<int> getHash(int l, int r) const { // [l, r], 0-ind
		vector<int> ans(MODS.size());
		for (int i = 0; i < MODS.size(); i++) {
			ans[i] = (hash[i][l] - (hash[i][r + 1] * ps[i][r - l + 1]) % MODS[i] + MODS[i]) % MODS[i];
		}
		return ans;
	}
};

struct FastPolynomialHash {

	const static inline long long MODS[2] = { 999999937, 999999929 }; // should be < 1e9 !
	const static inline long long p = 239;
	static inline vector<vector<long long>> ps;

	static void init(int SZ) { // !!!!!!!!!!!!!!!!

		ps.resize(2);

		ps[0].resize(SZ + 1);
		ps[0][0] = 1;
		for (int i = 1; i <= SZ; i++) {
			ps[0][i] = ps[0][i - 1] * p;
			if (ps[0][i] >= MODS[0]) {
				ps[0][i] %= MODS[0];
			}
		}

		ps[1].resize(SZ + 1);
		ps[1][0] = 1;
		for (int i = 1; i <= SZ; i++) {
			ps[1][i] = ps[1][i - 1] * p;
			if (ps[1][i] >= MODS[1]) {
				ps[1][i] %= MODS[1];
			}
		}
	}

	static int getAllHash(const string& S) {
		long long part1 = 0;
		for (int i = (int)S.size() - 1; i >= 0; i--) {
			part1 = S[i] + part1 * p;
			if (part1 >= MODS[0]) {
				part1 %= MODS[0];
			}
		}

		long long part2 = 0;
		for (int i = (int)S.size() - 1; i >= 0; i--) {
			part2 = S[i] + part2 * p;
			if (part2 >= MODS[1]) {
				part2 %= MODS[1];
			}
		}

		return part1 * (long long)1000000000 + part2;
	}

	vector<vector<long long>> hash;


	FastPolynomialHash(const string& S) {
		hash.resize(2);
		for (int i = 0; i < 2; i++) {
			hash[i].resize(S.size() + 1, 0);
			for (int j = (int)S.size() - 1; j >= 0; j--) {
				hash[i][j] = (S[j] + hash[i][j + 1] * p) % MODS[i];
			}
		}
	}

	long long getHash(int l, int r) const { // [l, r], 0-ind
		long long ans;

		ans = (hash[0][l] - (hash[0][r + 1] * ps[0][r - l + 1]) % MODS[0] + MODS[0]) % MODS[0];
		ans *= (long long)1000000000;
		ans += (hash[1][l] - (hash[1][r + 1] * ps[1][r - l + 1]) % MODS[1] + MODS[1]) % MODS[1];

		return ans;
	}
};

vector<int> zFunction(const string& S) {
	int N = S.size();
	vector<int> zf(N, 0);
	for (int i = 1, l = 0, r = 1; i < N; ++i) {
		if (i < r) {
			zf[i] = std::min(r - i, zf[i - l]);
		}
		while (i + zf[i] < N && S[zf[i]] == S[i + zf[i]])
			++zf[i];
		if (i + zf[i] > r) {
			l = i;
			r = i + zf[i];
		}
	}
	return zf;
}

vector<int> pFunction(const string& S) {
	int N = S.size();
	vector<int> pf(N, 0);
	for (int i = 1; i < N; i++) {
		int j = pf[i - 1];
		while (j > 0 && S[i] != S[j]) {
			j = pf[j - 1];
		}
		if (S[i] == S[j]) ++j;
		pf[i] = j;
	}
	return pf;
}

vector<vector<int>> kmpAut(const string& S, char start_letter, char end_letter) {
	int N = S.size();

	vector<int> pf(N, 0); // prefix function
	for (int i = 1; i < N; i++) {
		int j = pf[i - 1];
		while (j > 0 && S[i] != S[j]) {
			j = pf[j - 1];
		}
		if (S[i] == S[j]) {
			j++;
		}
		pf[i] = j;
	}

	int alphabet_sz = end_letter - start_letter + 1;

	vector<vector<int>> aut(N + 1, vector<int>(alphabet_sz, -1)); // aut[match length][new char] = new match length

	for (int i = 0; i <= N; i++) {
		for (int j = 0; j < alphabet_sz; j++) {

			int C = start_letter + j;

			if (i < N && S[i] == C) {
				aut[i][j] = i + 1;
			}
			else if (i == 0) {
				aut[i][j] = 0;
			}
			else {
				aut[i][j] = aut[pf[i - 1]][j];
			}
		}
	}
	return aut;
}