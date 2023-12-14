// https://leetcode.com/problems/count-and-say/

void nextStr(const char* fromStr, char* toStr);

// This runs in O(2^n) time and space. Sub-exponential complexity is impossible, as the length of
// the look-and-say sequence grows exponentially.
char * countAndSay(int n) {
    // Conway's constant, a little less than 1.304, is the amount that the sequence grows each
    // iteration (in the limit). The value of n+1 was found by experimentation using
    // AddressSanitizer. We then add 1 to account for the NUL terminator.
    // See https://en.wikipedia.org/wiki/Look-and-say_sequence#Growth_in_length
    const float conway = 1.304;
    const float power = pow(conway, n+1);
    const size_t shortMemLength = ceil(power) + 1;
    const size_t longMemLength = ceil(power * conway) + 1;
    char* fromStr = malloc(shortMemLength * sizeof(char));
    char* toStr   = malloc(longMemLength * sizeof(char));

    // swap fromStr and toStr around so that the final iteration will be copying from the shorter
    // one to the longer one
    if (n % 2 == 1) {
        char* const temp = fromStr;
        fromStr = toStr;
        toStr = temp;
    }

    fromStr[0] = '1';
    fromStr[1] = '\0';
    for (int i = 1; i < n; i++) {
        nextStr(fromStr, toStr);
        // swap fromStr and toStr for the next iteration
        char* const temp = fromStr;
        fromStr = toStr;
        toStr = temp;
    }
    free(toStr);
    return fromStr;
}

// Find the iteration after fromStr and store it in toStr. Note that this assumes fromStr is
// NUL-terminated and toStr is large enough to store the result. toStr's contents will be
// overwritten.
void nextStr(const char* fromStr, char* toStr) {
    char* toCurr = toStr;
    for (const char* fromCurr = fromStr; *fromCurr != '\0'; fromCurr++) {
        const char digit = fromCurr[0];
        char num;
        { // The maximum number of identical digits in a row is 3, so we only have to check 3
          // cases.
            if (fromCurr[1] != digit) {
                num = '1';
                goto end;
            }
            fromCurr++;
            if (fromCurr[1] != digit) {
                num = '2';
                goto end;
            }
            fromCurr++;
            num = '3';
        } end:

        toCurr[0] = num;
        toCurr[1] = digit;
        toCurr += 2;
    }
    toCurr[0] = '\0'; // dont forget the NUL terminator!
}

