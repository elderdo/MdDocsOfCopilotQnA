### question
How can I print the XML node tree in a formatted manner?

To print the XML node tree in a formatted manner, you can create a recursive function that prints each node and its attributes with proper indentation. This will help visualize the hierarchical structure of the XML tree.

Here's how you can implement the function to print the XML node tree:
1. Define the printNodeIndented Function
This function will recursively print each node and its attributes with indentation.
```c
void printNodeIndented(Node* node, int indentLevel) {
    if (node == NULL) {
        return;
    }

    // Print indentation
    for (int i = 0; i < indentLevel; i++) {
        printf("    ");
    }

    // Print the node's tag name
    printf("<%s", node->tagName);

    // Print the node's attributes
    for (int i = 0; i < node->attrCount; i++) {
        printf(" %s=\"%s\"", node->attributes[i]->name, node->attributes[i]->value);
    }

    if (node->childCount == 0 && node->textContent == NULL) {
        // Self-closing tag
        printf(" />\n");
    } else {
        printf(">\n");

        // Print the node's text content if it has any
        if (node->textContent) {
            for (int i = 0; i < indentLevel + 1; i++) {
                printf("    ");
            }
            printf("%s\n", node->textContent);
        }

        // Recursively print the child nodes
        for (int i = 0; i < node->childCount; i++) {
            printNodeIndented(node->children[i], indentLevel + 1);
        }

        // Print the closing tag with indentation
        for (int i = 0; i < indentLevel; i++) {
            printf("    ");
        }
        printf("</%s>\n", node->tagName);
    }
}
```

2. Modify the printNode Function
Update the printNode function to call printNodeIndented with an initial indentation level of 0.
```c
void printNode(Node* self) {
    printNodeIndented(self, 0);
}
```

Step 3: Update the Node Struct
Ensure the Node struct has the printNode function pointer set to the updated printNode function.
```c
Node* createNode(const char* tagName) {
    Node* node = (Node*)malloc(sizeof(Node));
    if (node) {
        node->type = NODE_ELEMENT;
        node->tagName = strdup(tagName);
        node->children = NULL;
        node->childCount = 0;
        node->attributes = NULL;
        node->attrCount = 0;
        node->textContent = NULL;
        node->init = NULL;
        node->free = freeNode;
        node->addChild = addChild;
        node->addAttribute = addAttribute;
        node->setTextContent = setTextContent;
        node->printNode = printNode;
        node->printAttributes = printAttributes;
    }
    return node;
}
```

4. Test the Implementation
You can now test the implementation by creating an XML node tree and printing it.
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

    root->printNode(root);

    root->free(root);
    return 0;
}
```

This will print the XML node tree in a formatted manner, showing the hierarchical structure with proper indentation.










