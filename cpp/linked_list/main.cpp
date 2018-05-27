#include <iostream>

// TODO
// Assignment overload
// Rule of 3
// Remove entry
// smart pointers
// &&
// virtual destructors
// iterators

template <typename T>
class Node {
public:
    T item;
    Node<T>* next;

    // rvalue&&? std::move? why or why not?
    // Node<T>(T item) : item(item), next(nullptr) { }
    Node<T>(T item, Node<T>* next=nullptr) : item(item), next(next) { }

    // bool operator==(const T& other) const { return item == other->item; }
    // bool operator!=(const T& other) const { return item != other->item; }
};

// iterator(const iterator&);
// ~iterator();
// iterator& operator=(const iterator&);
// friend void swap(iterator& lhs, iterator& rhs);
template <typename T>
class LinkedListIterator {
public:
    LinkedListIterator(Node<T>* node) : curr(node) { }

    bool operator==(const LinkedListIterator<T>& other) const {
        return curr == other.curr;
    }

    bool operator!=(const LinkedListIterator<T>& other) const {
        return curr != other.curr;
    }

    LinkedListIterator& operator++() {
        curr = curr->next;
        return *this;
    }

    LinkedListIterator operator++(int) {
        LinkedListIterator tmp(*this);
        operator++();
        return tmp;
    }

    T& operator*() const { return curr->item; }

private:
    Node<T>* curr;
};

template <typename T>
class LinkedList {
public:
    typedef LinkedListIterator<T> iterator;

    // ~LinkedList() {
    //     // iterate and destroy
    // }

    iterator begin() const { return iterator(head); }
    iterator end()   const { return iterator(nullptr); }

    T& front() const { return head->item; }
    T& back()  const { return last->item; }

    void push(T&& item) {
        auto node = new Node<T>(item);
        auto link = !head ? &head : &last->next;
        *link = node;
        last = node;
    }

    // maybe find via lambda...
    // or maybe use built in STL find_first_of/filter algorithms?
    // all you need to do is implement iterator...
    Node<T>* find_first(T&& item) const {
        auto link = head;
        while (link != nullptr &&
               link->item != item)
            link = link->next;
        return link;

        // auto link = &head;
        // while ((*link)->item != item &&
        //        (*link)->next != nullptr)
        //     link = &(*link)->next;
        // return *link;
    }

private:
    Node<T>* head = nullptr;
    Node<T>* last = nullptr;
};

int main() {
    LinkedList<int> list;
    list.push(42);
    list.push(66);
    list.push(420);

    // TODO try with something else

    // TODO use iterator...
    // boost::join, std::accumulate
    // for (auto const& it : list) {
    for (auto const it : list) {
        std::cout << it << ", ";
    }

    std::cout << std::endl;

    std::cout
        << "After 66 comes "
        << list.find_first(66)->next->item
        << std::endl;

    return 0;
}

