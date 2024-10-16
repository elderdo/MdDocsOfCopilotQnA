#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Node {
    char *tagName;
    char **attributes;
    struct Node **children;
    char *textContent;
    int attrCount;
    int childCount;
} Node;

Node* createNode(char *tagName) {
    Node *node = (Node*) malloc(sizeof(Node));
    node->tagName = strdup(tagName);
    node->attributes = NULL;
    node->children = NULL;
    node->textContent = NULL;
    node->attrCount = 0;
    node->childCount = 0;
    return node;
}

void addAttribute(Node *node, char *attrName, char *attrValue) {
    node->attributes = (char**) realloc(node->attributes, (node->attrCount + 1) * 2 * sizeof(char*));
    node->attributes[node->attrCount * 2] = strdup(attrName);
    node->attributes[node->attrCount * 2 + 1] = strdup(attrValue);
    node->attrCount++;
}

void addChild(Node *parent, Node *child) {
    parent->children = (Node**) realloc(parent->children, (parent->childCount + 1) * sizeof(Node*));
    parent->children[parent->childCount] = child;
    parent->childCount++;
}

void setTextContent(Node *node, char *textContent) {
    node->textContent = strdup(textContent);
}

Node* parseElement(FILE *file) {
    char line[256], tagName[256], attrName[256], attrValue[256], textContent[256];
    Node *currentNode = NULL;

    while (fgets(line, sizeof(line), file)) {
        if (sscanf(line, " <%s", tagName) == 1) {
            currentNode = createNode(tagName);
            while (fgets(line, sizeof(line), file) && sscanf(line, " %s = \"%[^\"]\"", attrName, attrValue) == 2) {
                addAttribute(currentNode, attrName, attrValue);
            }
            if (fgets(line, sizeof(line), file) && sscanf(line, " %[^<]", textContent) == 1) {
                setTextContent(currentNode, textContent);
            }
            return currentNode;
        }
    }
    return NULL;
}

Node* parseXML(char *filePath) {
    FILE *file = fopen(filePath, "r");
    if (!file) {
        perror("Failed to open file");
        return NULL;
    }

    Node *root = parseElement(file);
    fclose(file);
    return root;
}

typedef struct NodeArray {
    Node **nodes;
    int count;
} NodeArray;

NodeArray* createNodeArray() {
    NodeArray *array = (NodeArray*) malloc(sizeof(NodeArray));
    array->nodes = NULL;
    array->count = 0;
    return array;
}

void addToNodeArray(NodeArray *array, Node *node) {
    array->nodes = (Node**) realloc(array->nodes, (array->count + 1) * sizeof(Node*));
    array->nodes[array->count] = node;
    array->count++;
}

int nodeHasAttributeWithValue(Node *node, const char *attrName, const char *attrValue) {
    for (int i = 0; i < node->attrCount; i++) {
        if (strcmp(node->attributes[i * 2], attrName) == 0 &&
            strcmp(node->attributes[i * 2 + 1], attrValue) == 0) {
            return 1;
        }
    }
    return 0;
}

void searchNodes(Node *node, NodeArray *result, const char *tagName, const char *attrName, const char *attrValue) {
    if ((tagName == NULL || strcmp(node->tagName, tagName) == 0) &&
        (attrName == NULL || nodeHasAttributeWithValue(node, attrName, attrValue))) {
        addToNodeArray(result, node);
    }
    for (int i = 0; i < node->childCount; i++) {
        searchNodes(node->children[i], result, tagName, attrName, attrValue);
    }
}

int main() {
    Node *root = parseXML("C:\\Users\\Douglas\\Documents\\IPDM\\XML\\Neutral-R9_AssemblyDescExport.xml");
    if (root) {
        NodeArray *result = createNodeArray();
        // searchNodes(root, result, "Product", "subType", "BA6_Assembly");
        searchNodes(root, result, NULL, NULL, NULL);

        printf("Found %d nodes:\n", result->count);
        for (int i = 0; i < result->count; i++) {
            Node *node = result->nodes[i];
            printf("Tag: %s\n", node->tagName);
            for (int j = 0; j < node->attrCount; j++) {
                printf("  Attribute: %s, Value: %s\n", node->attributes[j * 2], node->attributes[j * 2 + 1]);
            }
            if (node->textContent) {
                printf("  Text Content: %s\n", node->textContent);
            }
        }
        // Free memory (not shown for brevity)
    }
    return 0;
}



