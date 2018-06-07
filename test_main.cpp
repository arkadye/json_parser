#include "json_parser.h"
#include <fstream>
#include <iostream>

int main()
{
	JSON::ValueType object;
	std::ifstream testjson{ "test.json" };
	try
	{
		if (testjson)
		{
			testjson >> object;
			std::cout << object;
		}
	}
	catch (const std::exception& any)
	{
		std::cout << "Exception caught: " << any.what() << '\n';
	}
	std::string dummy;
	std::cout << "Press 'enter' to quit."; // Sorry command line folks.
	std::getline(std::cin, dummy);
    return 0;
}
