### question
How can I search for a specific XML node based on its attribute value?

### answer
To search for a specific XML node based on its attribute value, you can implement a recursive function that traverses the XML node tree and checks each node's attributes. If a node with the specified attribute value is found, the function can return that node.

Here's how you can implement the search function:

1. Define the findNodeByAttribute Function
This function will recursively search for a node with a specific attribute value.
```c
Node* findNodeByAttribute(Node* node, const char* attrName, const char* attrValue) {
    if (node == NULL) {
        return NULL;
    }

    // Check if the current node has the specified attribute with the given value
    for (int i = 0; i < node->attrCount; i++) {
        if (strcmp(node->attributes[i]->name, attrName) == 0 &&
            strcmp(node->attributes[i]->value, attrValue) == 0) {
            return node;
        }
    }

    // Recursively search in the child nodes
    for (int i = 0; i < node->childCount; i++) {
        Node* found = findNodeByAttribute(node->children[i], attrName, attrValue);
        if (found != NULL) {
            return found;
        }
    }

    return NULL; // Node not found
}
```

2. Test the Implementation
You can now test the implementation by creating an XML node tree and searching for a node with a specific attribute value.
```c
int main() {
    Node* root = createNode("root");
    root->addAttribute(&root, "version", "1.0");

    Node* child1 = createNode("child1");
    child1->setTextContent(child1, "This is child 1");
    root->addChild(&root, child1);

    Node* child2 = createNode("child2");
    child2->addAttribute(&child2, "attr", "value");
    root->addChild(&root, child2);

    // Search for a node with attribute attr="value"
    Node* foundNode = findNodeByAttribute(root, "attr", "value");
    if (foundNode != NULL) {
        printf("Found node: %s\n", foundNode->tagName);
    } else {
        printf("Node not found\n");
    }

    root->free(root);
    return 0;
}
```


