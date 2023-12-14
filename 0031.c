// https://leetcode.com/problems/next-permutation/

int* longestDecreasingSuffix(const int* start, const int* end);
int* firstElementGreaterThan(const int* start, const int* end, int val);
void reverse(int* start, int* end);

void nextPermutation(int* const start, size_t length) {
    int* const end = start + length;
    int* const suffix = longestDecreasingSuffix(start, end);
    reverse(suffix, end);
    if (suffix == start) return; // we're done, we don't need to do the extra swap

    int* const swapA = suffix - 1;
    int* const swapB = firstElementGreaterThan(suffix, end, *swapA);

    const int temp = *swapA;
    *swapA = *swapB;
    *swapB = temp;
}

// Returns the start of the array if the entire array is decreasing.
int* longestDecreasingSuffix(const int* start, const int* end) {
    for (const int* curr = end-1; curr > start; curr--) {
        if (curr[-1] < curr[0]) return curr;
    }
    return start;
}

// Returns NULL if there is no element of the array greater than val.
int* firstElementGreaterThan(const int* start, const int* end, int val) {
    for (int* curr = start; curr < end; curr++) {
        if (*curr > val) return curr;
    }
    return NULL;
}

void reverse(int* start, int* end) {
    const size_t numToSwap = (end - start) / 2;
    for (size_t i = 0; i < numToSwap; i++) {
        const int temp = start[i];
        start[i] = end[-i-1];
        end[-i-1] = temp;
    }
}

