// https://leetcode.com/problems/merge-k-sorted-lists/

/**
 * Definition for singly-linked list.
 * struct ListNode {
 *     int val;
 *     struct ListNode *next;
 * };
 */

// we're only going to be working with *pointers* to ListNodes, so might as well
// note that NULL represents the empty list
typedef struct ListNode* Node;

Node popLeast(Node* lists, size_t numLists);

Node mergeKLists(Node* lists, size_t numLists) {
    if (numLists == 0) return NULL;

    Node sorted = popLeast(lists, numLists);
    Node sortedEnd = sorted;
    Node curr = NULL;
    while (curr = popLeast(lists, numLists)) { // while lists contains non-empty lists...
        sortedEnd->next = curr;
        sortedEnd = curr;
    } // we've now consumed all of the lists in 'lists'

    return sorted;
}

// Given an array of Nodes, find the least one and pop it from its list, returning it. This will
// modify the array to hold the tail of the list that was popped from.
// Returns NULL if the array contains no non-null Nodes.
Node popLeast(Node* lists, size_t numLists) {
    if (numLists == 0) return NULL;

    Node leastNode = lists[0];
    size_t leastIndex = 0;
    for (size_t i = 1; i < numLists; i++) {
        Node curr = lists[i];
        // NOTE: I know that this if statement could be compressed to have just one block, but I
        //       find it more comprehensible in this format. A good compiler should optimize it to
        //       the compressed version anyway.

        // We want to ignore empty lists when finding the least Node out of our remaining input, so
        // if all the previous lists have been empty we should switch to the current list. We can
        // do this regardless of whether the current list is empty.
        if (!leastNode) {
            leastNode = curr;
            leastIndex = i;
        } else if (!curr) {
            // do nothing
        } else if (curr->val < leastNode->val) {
            leastNode = curr;
            leastIndex = i;
        }
    }
    if (!leastNode) return NULL;
    // leastNode and leastIndex are now set

    Node newHead = leastNode->next;
    lists[leastIndex] = newHead;

    // properly detach leastNode from the rest of the list, to catch bugs
    leastNode->next = NULL;
    return leastNode;
}

