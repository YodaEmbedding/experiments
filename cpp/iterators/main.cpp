#include <algorithm>
#include <iostream>
#include <vector>


int main()
{
	std::vector<int>  v = {0, 1, 1, 2, 3, 5, 8};
	std::vector<bool> b = {0, 1, 1, 0, 1, 1, 0};

	auto it = std::remove_if(v.begin(), v.end(),
		[&b, &v](auto const& x) { return !b.at(&x - v.data()); });

	for (auto i = it; i != v.end(); i++) std::cout << *i << " ";
	std::cout << std::endl;

	v.erase(it, v.end());

	for (auto& x : v) std::cout << x << " ";
	std::cout << std::endl;

	auto cpy = std::vector<int>(v.begin(), v.end());

	for (auto& x : cpy) std::cout << x << " ";
	std::cout << std::endl;

	std::cout << v.end() - std::min(v.begin() + 3, v.end());
	std::cout << std::endl;

	return 0;
}

