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
	SegmentTree(int N, const TreeType &value) {
		size = N;
		tree.resize(4 * size, value);
	}

	void update(const TreeType& value, int i) { // 0-ind
		__update(value, i, 0, size, 0);
	}

	TreeType get(int l, int r) { // [l, r], 0-ind
		return __get(l, r + 1, 0, size, 0);
	}
};

template<typename TreeType> struct RangeSegmentTree {

	const static inline TreeType FILLER = 0;

	struct Node {
		TreeType value = FILLER;

		int operation = 0; // 0 - addition, 1 - setting
		TreeType worth = 0;
	};

	TreeType func(const TreeType& A, const TreeType& B) {
		return A + B;
	}

	int size = 0;
	vector<Node> tree;

	void __propagate(int __i, int __l, int __r) {

		if (tree[__i].operation == 0) {
			tree[__i].value += tree[__i].worth * (__r - __l);
		}
		if (tree[__i].operation == 1) {
			tree[__i].value = tree[__i].worth * (__r - __l);
		}

		if (__r - __l > 1) {
			if (tree[__i].operation == 0) {
				tree[__i * 2 + 1].worth += tree[__i].worth;
				tree[__i * 2 + 2].worth += tree[__i].worth;
			}
			if (tree[__i].operation == 1) {
				tree[__i * 2 + 1].operation = 1;
				tree[__i * 2 + 2].operation = 1;
				tree[__i * 2 + 1].worth = tree[__i].worth;
				tree[__i * 2 + 2].worth = tree[__i].worth;
			}
		}

		tree[__i].operation = 0;
		tree[__i].worth = 0;
	}

	void __update(const TreeType& V, int l, int r, int operation, int __i, int __l, int __r) {
		__propagate(__i, __l, __r);
		if (__r <= l || r <= __l) {
			return;
		}
		if (l <= __l && __r <= r) {
			tree[__i].operation = operation;
			tree[__i].worth = V;
			__propagate(__i, __l, __r);
			return;
		}
		int __m = (__l + __r) / 2;
		__update(V, l, r, operation, __i * 2 + 1, __l, __m);
		__update(V, l, r, operation, __i * 2 + 2, __m, __r);
		tree[__i].value = func(tree[__i * 2 + 1].value, tree[__i * 2 + 2].value);
	}

	TreeType __get(int l, int r, int __i, int __l, int __r) {
		__propagate(__i, __l, __r);
		if (__r <= l || r <= __l) {
			return FILLER;
		}
		if (l <= __l && __r <= r) {
			return tree[__i].value;
		}
		int __m = (__l + __r) / 2;

		TreeType V1 = __get(l, r, __i * 2 + 1, __l, __m);
		TreeType V2 = __get(l, r, __i * 2 + 2, __m, __r);

		return func(V1, V2);
	}

	RangeSegmentTree(const vector<TreeType>& arr) {
		size = arr.size();
		tree.resize(4 * size);
		for (int i = 0; i < size; i++) {
			set(arr[i], i, i);
		}
	}
	RangeSegmentTree(int N, const TreeType& value) {
		size = N;
		tree.resize(4 * size);
		for (int i = 0; i < size; i++) {
			set(value, i, i);
		}
	}

	void add(const TreeType& V, int l, int r) {
		__update(V, l, r + 1, 0, 0, 0, size);
	}

	void set(const TreeType& V, int l, int r) {
		__update(V, l, r + 1, 1, 0, 0, size);
	}

	TreeType get(int l, int r) {
		return __get(l, r + 1, 0, 0, size);
	}
};

template<typename TreeType> struct MergeSortTree {
private:

	/* This structure allows you to quickly calculate some
	function from elements from an array segment from l to r
	if it works quickly on a sorted segment from l to r. */

	int func(const vector<TreeType> &arr, const TreeType& V) {
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

private:
	int size = 0;
	vector<vector<TreeType>> tree;

private:
	void __mergeSort(vector<TreeType>& arr, int __i) {
		if (arr.size() == 1) {
			tree[__i] = arr;
			return;
		}
		int m = arr.size() / 2;

		std::vector<TreeType> left(m);
		for (int i = 0; i < m; i++) {
			left[i] = arr[i];
		}

		__mergeSort(left, __i * 2 + 1);

		std::vector<TreeType> right(arr.size() - m);
		for (int i = m; i < arr.size(); i++) {
			right[i - m] = arr[i];
		}

		__mergeSort(right, __i * 2 + 2);

		for (int i = 0, l = 0, r = 0; i < arr.size(); i++) {
			if (l < left.size() && r < right.size()) {
				if (left[l] < right[r]) {
					arr[i] = left[l++];
				}
				else {
					arr[i] = right[r++];
				}
			}
			else if (l < left.size()) {
				arr[i] = left[l++];
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

public:

	MergeSortTree(vector<TreeType> arr) {
		size = arr.size();
		tree.resize(4 * size);
		__mergeSort(arr, 0);
	}

	int get(const TreeType& V, int l, int r) { // [l, r], 0-ind
		return __get(V, l, r + 1, 0, size, 0);
	}
};


// --------------- Containers ---------------

template<typename StackType> struct Stack{

	StackType func(const StackType& A, const StackType& B) {
		return min(A, B);
	}

	vector<StackType> main_stack;
	vector<StackType> func_stack;

	void push(const StackType &value) {
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

	bool isEmpty() {
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

	QueueType func(const QueueType &A, const QueueType &B) {
		return std::min(A, B);
	}

	Stack<QueueType> front_part;
	Stack<QueueType> back_part;

	void push(const QueueType &value) {
		back_part.push(value);
	}

	void pop() {
		if (front_part.isEmpty()) {
			while (!back_part.isEmpty()) {
				front_part.push(back_part.back());
				back_part.pop();
			}
		}
		front_part.pop();
	}

	int size() {
		return front_part.size() + back_part.size();
	}

	bool isEmpty() {
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
		return func(front_part.func_value(), back_part.func_value());
	}
};


// --------------- Strings and Hashes ---------------

template<typename HashType> struct AmountHash {
	/* This class allows you to get a hash of a multiset 
	of elements from an array segment from l to r */

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

	vector<int> getHash(int l, int r) { // [l, r], 0-ind
		vector<int> result(MODS.size());
		for (int i = 0; i < MODS.size(); i++) {
			result[i] = (hash[i][l] - hash[i][r + 1] + MODS[i]) % MODS[i];
		}
		return result;
	}
};

void zFunction(const string &S, vector<int> &Z) {
	int N = (int)S.size();
	Z.assign(N, 0);
	for (int i = 1, l = 0, r = 1; i < N; ++i) {
		if (i < r) {
			Z[i] = std::min(r - i, Z[i - l]);
		}
		while (i + Z[i] < N && S[Z[i]] == S[i + Z[i]])
			++Z[i];
		if (i + Z[i] > r) {
			l = i, r = i + Z[i];
		}
	}
}