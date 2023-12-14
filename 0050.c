// https://leetcode.com/problems/powx-n/

unsigned int absval(const int n) {
    // if n is INT_MIN we can't just do (-n) because leetcode's runtime checking will complain,
    // even though it would totally work (undefined behavior? I 'ardly KNOW 'er!)
    return n >= 0 ? n : - (unsigned int) n;
}

// Simple algorithm based on the properties of exponents
double myPow(const double x, const int n) {
    double result = 1.0;
    double square = x; // We repeatedly square this

    for (unsigned int remainingExp = absval(n); remainingExp > 0; remainingExp >>= 1) {
        if (remainingExp & 0b1) {
            result *= square;
        }
        square *= square;
    }

    if (n < 0) {
        result = 1 / result;
    }

    return result;
}

