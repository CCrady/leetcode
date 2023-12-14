// https://leetcode.com/problems/swap-nodes-in-pairs/

/**
 * Definition for singly-linked list.
 * struct ListNode {
 *     int val;
 *     struct ListNode *next;
 * };
 */

typedef struct ListNode* Node;

Node swapPairs(Node head) {
    // dummy node so that the algorithm works without a special case for the first pair of nodes
    struct ListNode dummy = {0, head};
    Node curr = head;
    Node prev = &dummy;
    while (curr && curr->next) {
        Node next = curr->next;
        Node nextNext = next->next;
        prev->next = next;
        curr->next = nextNext;
        next->next = curr;
        prev = curr;
        curr = nextNext;
    }
    return dummy.next;
}

