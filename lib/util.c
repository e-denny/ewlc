#include "util.h"


struct key_node *create_node(uint32_t mods, xkb_keysym_t sym,
                             struct ewlc_keyboard *kb,
                             struct wlr_event_keyboard_key *event)
{
    struct key_node *new_node =
        (struct key_node *)malloc(sizeof(struct key_node));
    printf("into: create node\n");
    if (new_node == NULL)
        ERROR("Error creating a new node.\n");

    new_node->mods = mods;
    new_node->sym = sym;
    new_node->kb = kb;
    new_node->event = event;
    new_node->next = NULL;

    return new_node;
}

struct key_node *add_to_end(struct key_node *list, uint32_t mods,
                             xkb_keysym_t sym, struct ewlc_keyboard *kb,
                             struct wlr_event_keyboard_key *event)
{
    struct key_node *cursor;
    struct key_node *new_node;
    printf("into: add_to_tail\n");

    new_node = create_node(mods, sym, kb, event);
    cursor = list;

    printf("before: create_node\n");

    /* go to the last node */
    if (cursor == NULL) {
        list = new_node;
    } else {
        while (cursor->next != NULL)
            cursor = cursor->next;
        cursor->next = new_node;
    }

    printf("after: create_node\n");
    return list;
}

struct key_node *remove_from_start(struct key_node *list)
{
    struct key_node *front;

    if (list == NULL)
        return NULL;

    front = list;
    list = list->next;

    front->next = NULL;
    /* is this the last node in the list */
    if (front == list)
        list = NULL;
    free(front);

    return list;
}
