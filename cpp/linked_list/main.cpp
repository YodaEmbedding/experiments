#include <iostream>
#include <numeric>
#include <vector>

// TODO
// Assignment overload
// Rule of 3
// Remove entry
// smart pointers
// &&
// virtual destructors
// iterators

template <typename T> class LinkedList;
template <typename T> class LinkedListIterator;
template <typename T> class Node;

template <typename T>
class LinkedList {
public:
    typedef LinkedListIterator<T> iterator;

    ~LinkedList() {
        for (auto it = begin(); it != end(); ++it)
            delete it.curr;
    }

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

// iterator(const iterator&);
// ~iterator();
// iterator& operator=(const iterator&);
// friend void swap(iterator& lhs, iterator& rhs);
template <typename T>
class LinkedListIterator {
public:
    friend class LinkedList<T>;

    LinkedListIterator(Node<T>* node) : curr(node), next(nullptr) {
        if (node != nullptr) next = node->next;
    }

    bool operator==(const LinkedListIterator<T>& other) const {
        return curr == other.curr;
    }

    bool operator!=(const LinkedListIterator<T>& other) const {
        return curr != other.curr;
    }

    LinkedListIterator& operator++() {
        LinkedListIterator tmp(next);
        std::swap(*this, tmp);
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
    Node<T>* next;
};

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

// Consider boost::join instead
template <typename T, template<typename> typename IterType>
std::string join(
    IterType<T> begin,
    IterType<T> end,
    std::string&& delimiter) {

    auto first = *begin;
    return std::accumulate(++begin, end,
        std::to_string(first),
        [&delimiter](std::string& a, T b) {
            return a + delimiter + std::to_string(b);
        });
}

int main() {
    // TODO try with class or smart pointers (not just int)
    LinkedList<int> numbers;
    numbers.push(42);
    numbers.push(66);
    numbers.push(420);

    // for (auto const& it : numbers)
    //     std::cout << it << ", ";

    std::cout
        << join(numbers.begin(), numbers.end(), std::string(", "))
        << std::endl;

    std::cout
        << "After 66 comes "
        << numbers.find_first(66)->next->item
        << std::endl;

    return 0;
}

