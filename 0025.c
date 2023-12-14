// https://leetcode.com/problems/reverse-nodes-in-k-group/

/**
 * Definition for singly-linked list.
 * struct ListNode {
 *     int val;
 *     struct ListNode *next;
 * };
 */

// This algorithm runs in 0(1) space; this is easily verifiable in C by noting that there is no
// recursion and no memory allocation.

typedef struct ListNode* Node;
typedef struct {
    Node newHead;
    Node oldHead;
    Node newUnreversed;
} ReverseResult;

ReverseResult reverseFirstK(Node list, size_t k);
ReverseResult reverseAll(Node list);
//bool lengthAtLeast(Node list, size_t length);

Node reverseKGroup(Node list, size_t k) {
    struct ListNode dummy = {0, list};

    Node unreversed = list;
    Node lastReversed = &dummy;
    while (unreversed) {
        ReverseResult r = reverseFirstK(unreversed, k);
        lastReversed->next = r.newHead;
        lastReversed = r.oldHead;
        unreversed = r.newUnreversed;
    }

    return dummy.next;
}

// Returns the new head of the list and the head of the as-yet unreversed portion. If the list was
// too short to reverse, returns {list, list, NULL}.
ReverseResult reverseFirstK(Node list, size_t k) {
    if (!list) return (ReverseResult) {list, list, NULL};

    const Node oldHead = list;
    // 'prev', 'curr', and 'next' are named in the original, unreversed order
    Node prev = list;
    Node curr = list->next;
    Node next = NULL;
    prev->next = NULL;
    for (size_t i = 1; i < k; i++) {
        // Oops, the list was too short and we encountered the end before we were done! We'll have
        // to un-reverse the portion of the list that we've reversed so far.
        if (!curr) {
            reverseAll(prev);
            return (ReverseResult) {oldHead, oldHead, NULL};
        }

        next = curr->next;
        curr->next = prev;
        prev = curr;
        curr = next;
    }
    const Node newHead = prev;
    const Node newUnreversed = curr;
    oldHead->next = newUnreversed;

    return (ReverseResult) {newHead, oldHead, newUnreversed};
}

// Result always has newUnreversed == NULL, since this reverses the entire list.
ReverseResult reverseAll(Node list) {
    if (!list) return (ReverseResult) {list, list, NULL};

    const Node oldHead = list;
    Node prev = list;
    Node curr = list->next;
    Node next = NULL;
    prev->next = NULL;
    while (curr) {
        next = curr->next;
        curr->next = prev;
        prev = curr;
        curr = next;
    }
    const Node newHead = prev;
    return (ReverseResult) {newHead, oldHead, NULL};
}

/*
bool lengthAtLeast(Node list, size_t length) {
    Node curr = list;
    for (size_t i = 0; i < length; i++) {
        if (!curr) return false;
        curr = curr->next;
    }
    return true;
}
*/

