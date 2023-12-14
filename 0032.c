// https://leetcode.com/problems/longest-valid-parentheses/

int max(int a, int b);

int longestValidParentheses(const char* s) {
    int maxGrpLength = 0;
    int currGrpStart = 0;
    int numOpen = 0;
    int i;
    for (i = 0; s[i] != '\0'; i++) {
        if (s[i] == '(') numOpen++;
        else numOpen--;

        if (numOpen == 0) {
            // The substring from currGrpStart to i (inclusive) is valid, so let's update
            // maxGrpLength accordingly.
            const int currGrpLength = i - currGrpStart + 1;
            maxGrpLength = max(maxGrpLength, currGrpLength);
        } else if (numOpen < 0) {
            // The current substring we've been checking is borked, so let's start fresh with a new
            // one on the next iteration.
            currGrpStart = i + 1;
            numOpen = 0;
        }
    }
    const int sLength = i; // i is now the index of the NUL terminator

    // Do the same thing but in reverse
    int currGrpEnd = sLength - 1;
    numOpen = 0;
    for (i = sLength - 1; i >= 0; i--) {
        if (s[i] == ')') numOpen++;
        else numOpen--;

        if (numOpen == 0) {
            // The substring from i to currGrpEnd (inclusive) is valid, so let's update
            // maxGrpLength accordingly.
            const int currGrpLength = currGrpEnd - i + 1;
            maxGrpLength = max(maxGrpLength, currGrpLength);
        } else if (numOpen < 0) {
            // The current substring we've been checking is borked, so let's start fresh with a new
            // one on the next iteration.
            currGrpEnd = i - 1;
            numOpen = 0;
        }
    }

    return maxGrpLength;
}

int max(int a, int b) {
    return a >= b ? a : b;
}

