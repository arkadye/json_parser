# json_parser
Read and write JSON

To use:

JSON::ValueType is the basic type, holding any JSON type.

You can read into a ValueType with operator>>, and write one one using operator<<.

Base functions are:

- type(): returns an enum with the type.
- to_string(): returns a std::string with the JSON
- to_stream(std::ostream&): writes the value as JSON to the given stream.

NullType is convertable to a nullptr.

NumberType is convertable to a double.

StringType is convertable to a std::string.

ArrayType acts like a std::vector<ValueType>.

ObjectType acts like a std::vector<std::pair<StringType,ValueType>> but also a bit like a std::unordered_multimap<StringType,ValueType>.
