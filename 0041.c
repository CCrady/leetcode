// https://leetcode.com/problems/first-missing-positive/description/

// Outline: We're going to sort the array, sort of. Pretend that the array contains exactly the
// numbers 1..n. Iterate through the array, and whenever we find an element that's out of place
// (i.e. its 1-based index is different from its value) swap it with the element that's in its
// proper place. At the end, all elements will be in their proper places, and all the missing
// elements will be noticeable because their proper place doesn't contain them.

int firstMissingPositive(int* const nums, const int numsSize) {
    // maybe faster to do this during the swaps?
    for (int i = 0; i < numsSize; i++) {
        if (nums[i] <= 0 || nums[i] > numsSize) {
            nums[i] = 0;
        }
    }

    // This kind of looks like it should take O(n^2) time, since it's a loop nested in another, but
    // in fact it only takes O(n) time. This is because the work done by the inner loop in one
    // iteration reduces the work that it needs to do in future iterations.
    for (int i = 0; i < numsSize; i++) {
        int* const curr = nums + i;
        while (*curr != 0 && *curr != (i+1)) {
            // swap the values of *curr and the element in *curr's place
            const int temp = *curr;
            *curr = nums[temp - 1];
            nums[temp - 1] = temp;
            // if we just swapped two identical values, make *curr zero
            if (*curr == temp) *curr = 0;
        }
    }

    for (int i = 0; i < numsSize; i++) {
        if (nums[i] == 0) return i+1;
    }
    return numsSize+1;
}

