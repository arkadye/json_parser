#pragma once

#include <vector>
#include <string>
#include <memory>
#include <sstream>
#include <algorithm>
#include <iterator>
#include <exception>
#include <iomanip>
#include <cassert>

namespace JSON
{
	// Exceptions

	class RangeException : public std::out_of_range
	{
		static std::string make_message(std::size_t index, std::size_t max)
		{
			std::ostringstream msg;
			msg << "Tried to access array out of bounds: index " << index << ", maximum " << max;
			return msg.str();
		}
	public:
		const std::size_t m_index;
		const std::size_t m_max;
		RangeException(std::size_t index, std::size_t max)
			: std::out_of_range{ make_message(index,max) }, m_index{ index }, m_max{ max }
		{}
	};

	class KeyException : public std::out_of_range
	{
	public:
		std::string m_key;
		KeyException(std::string key) :
			std::out_of_range{ std::string("Could not find key: ") + key }, m_key{ std::move(key) }{}
	};

	class InvalidCopy : public std::exception
	{
	public:
		InvalidCopy() : std::exception("Tried to copy to the wrong type.") {}
	};

	class ParseError : public std::exception
	{
	public:
		ParseError(std::string msg) : std::exception{ (std::string("Parse error: ") + std::move(msg)).c_str() } {}
	};

	// Types of object available.
	enum class Type
	{
		number,
		string,
		array,
		object,
		boolean,
		null
	};

	class BaseType;

	namespace
	{
		// PtrType for internal use
		using PtrType = std::unique_ptr<BaseType>;
	}

	// Free function polymorphic copy.
	PtrType copy(const BaseType& rhs);

	// Free-function polymorphic move.
	PtrType move(BaseType&& rhs) noexcept;

	// Base class to PtrType different JSON types polymorphically.
	class BaseType
	{
	private:
		// Polymorphic copy to a particlar type (if possible).
		friend PtrType copy(const BaseType&);
		friend PtrType move(BaseType&& rhs) noexcept;
		template <class T>
		PtrType copy_internal() const &
		{
			static_assert(std::is_base_of<BaseType, T>(), "Can only copy as a type built on BaseType.");
			const T* derived = dynamic_cast<const T*>(this);
			if (derived != nullptr)
			{
				return std::make_unique<T>(*derived);
			}
			else
			{
				throw InvalidCopy();
			}
		}

		// Sort of the same, but moves instead.
		template <class T>
		PtrType move_internal() && noexcept
		{
			static_assert(std::is_base_of<BaseType, T>(), "Can only move as a type built on BaseType.");
			T* derived = dynamic_cast<T*>(this);
			if (derived != nullptr)
			{
				return std::make_unique<T>(std::move(*derived));
			}
			return nullptr;
		}
	public:
		// Query the derived type; useful for tagged dispatch.
		virtual Type type() const = 0;

		// Write the type to a stream
		virtual void to_stream(std::ostream& output) const = 0;

		// Convert the type to a string.
		virtual std::string to_string() const
		{
			std::ostringstream oss;
			to_stream(oss);
			return oss.str();
		}
	};

	// JSON null.
	class NullType : public BaseType
	{
	public:
		Type type() const noexcept override final { return Type::null; }
		void to_stream(std::ostream& output) const override final { output << to_string(); }
		std::string to_string() const override final { return "null"; }
		nullptr_t get() const noexcept { return nullptr; }
		operator nullptr_t() const noexcept { return get(); }
	};

	class ValueType
	{
		PtrType data;
	public:
		ValueType() : ValueType{ NullType{} } {}
		ValueType(const BaseType& object) : data{ copy(object) } {}
		ValueType(BaseType&& object) noexcept : data{ JSON::move(std::move(object)) } {}
		ValueType(const ValueType& other) : ValueType{ *other } {}
		ValueType(ValueType&&) = default;
		ValueType& operator=(const ValueType& other) { data = JSON::copy(*other); return *this; }
		ValueType& operator=(ValueType&&) = default;

		const BaseType& operator->() const noexcept { return *data.get(); }
		BaseType& operator->() noexcept { return *data.get(); }
		const BaseType& operator*() const noexcept { return operator->(); }
		BaseType& operator*() noexcept { return operator->(); }

		template <class JSONType>
		const JSONType& getAs() const
		{
			static_assert(std::is_base_of<BaseType, T>(), "Can only get a JSON::ValueType as a child of BaseType.");
			const BaseType* pBase = object.get();
			const JSONType* pResult = dynamic_cast<const JSONType*>(pBase);
			if (pResult != nullptr)
			{
				return *pResult;
			}
			else
			{
				throw InvalidCopy{};
			}
		}

		template <class JSONType>
		JSONType& getAs() { return const_cast<JSONType&>(const_cast<const ValueType*>(this)->getAs<JSONType>()); }

		Type getType() const { return data->type(); }
	};
}

std::ostream& operator<<(std::ostream& outstream, const JSON::BaseType& value)
{
	value.to_stream(outstream);
	return outstream;
}

std::ostream& operator<<(std::ostream& outstream, const JSON::ValueType& value)
{
	outstream << *value;
	return outstream;
}

namespace JSON
{

	// JSON number - uses a double internally.
	class NumberType : public BaseType
	{
		double value;
	public:
		// Construct, copy and covert.
		NumberType(double init) noexcept : value{ init }{}
		NumberType() noexcept : NumberType{ 0.0 } {}
		NumberType(const NumberType&) = default;
		NumberType& operator=(const NumberType&) = default;
		NumberType& operator=(double rhs) noexcept { value = rhs; return *this; }
		operator double() const noexcept { return get(); }

		// Base overrides.
		Type type() const noexcept override final { return Type::number; }
		std::string to_string() const override final { return std::to_string(value); }
		void to_stream(std::ostream& in) const override final { in << value; }

		// Interact with the value.
		double get() const noexcept { return value; }
		double& get() noexcept { return value; }
		void set(double number) noexcept { value = number; }
	};

	// JSON boolean.
	class BooleanType : public BaseType
	{
		bool value;
	public:
		// Construct, copy and convert.
		BooleanType(bool init) noexcept : value{ init } {}
		BooleanType() noexcept : BooleanType{ false } {}
		BooleanType(const BooleanType&) = default;
		BooleanType& operator=(const BooleanType&) = default;
		BooleanType& operator=(bool rhs) noexcept { value = rhs; return *this; }
		operator bool() const noexcept { return get(); }

		// Base overrides.
		Type type() const noexcept override final { return Type::boolean; }
		std::string to_string() const override final { return value ? "true" : "false"; }
		void to_stream(std::ostream& output) const override final { output << to_string(); }

		// Interact with the value.
		bool get() const noexcept { return value; }
		bool& get() noexcept { return value; }
		void set(bool boolean) noexcept { value = boolean; }
	};

	class StringType : public BaseType
	{
		std::string value;
	public:
		// Constructor, copy and convert
		StringType(std::string init) noexcept : value{ std::move(init) } {}
		StringType() noexcept {}
		StringType(const StringType&) = default;
		StringType(StringType&&) = default;
		StringType& operator=(const StringType&) = default;
		StringType& operator=(StringType&&) = default;
		operator std::string() const { return get(); }

		// Base overrides.
		Type type() const noexcept override final { return Type::string; }

		// Returns the string with quote marks.
		void to_stream(std::ostream& output) const override final
		{
			output << '"' << value << '"';
		}

		// Set and inspect.
		const std::string& get() const noexcept { return value; }
		std::string& get() noexcept { return value; }
		void set(std::string newstring) noexcept { value = std::move(newstring); }
	};

	namespace
	{
		// Shared base type for ArrayType and ObjectType, which otherwise share a lot of code.
		// Interface is based on std::vector.
		template <class UnderlyingType>
		class AggregateType : public BaseType
		{
			using DataType = std::vector<UnderlyingType>;

			// Build from iterators
			template <class ItType>
			static DataType makeData(ItType start, ItType finish)
			{
				DataType result;
				result.reserve(std::distance(start, finish));
				std::copy(start, finish, std::back_inserter(result));
				return result;
			}

			DataType data;

		public:
			using value_type = UnderlyingType;
			using size_type = std::size_t;
			using difference_type = std::ptrdiff_t;
			using reference = value_type&;
			using const_reference = const value_type&;
			using rvalue_reference = value_type&&;
			using pointer = value_type*;
			using const_pointer = const value_type*;
			using iterator = typename DataType::iterator;
			using const_iterator = typename DataType::const_iterator;
			using reverse_iterator = typename DataType::reverse_iterator;
			using const_reverse_iterator = typename DataType::const_reverse_iterator;

			// Default constructor - empty
			AggregateType() {}

			// Construct from iterators
			template <class ItType>
			AggregateType(ItType start, ItType finish) : data{ makeData(start,finish) } {}

			// Copy and move constructors and operators
			AggregateType(const AggregateType& other) : AggregateType{ other.begin(),other.end() } {}
			AggregateType& operator=(const AggregateType& other) { data = makeData(other.begin(), other.end()); return *this; }
			AggregateType(AggregateType&&) = default;
			AggregateType& operator=(AggregateType&&) = default;

			// Access
			reference operator[](size_type pos)
			{
				assert(pos < size());
				return data[pos];
			}
			const_reference operator[](size_type pos) const { return const_cast<AggregateType*>(this)->operator[](pos); }
			reference at(size_type pos)
			{
				if (pos < size())
				{
					return operator[](pos);
				}
				else
				{
					throw RangeException{ pos,size() };
				}
			}
			const_reference at(size_type pos) const { return const_cast<const_reference>(const_cast<AggregateType*>(this)->at(pos)); }
			reference front() { return operator[](0); }
			const_reference front() const { return operator[](0); }
			reference back() { return operator[](size() - 1); }
			const_reference back() const { return operator[](size() - 1); }

			iterator begin() noexcept { return data.begin(); }
			const_iterator begin() const noexcept { return cbegin(); }
			iterator end() noexcept { return data.end(); }
			const_iterator end() const noexcept { return cend(); }
			const_iterator cbegin() const noexcept { return data.cbegin(); }
			const_iterator cend() const noexcept { return data.cend(); }
			reverse_iterator rbegin() noexcept { return data.rbegin(); }
			const_reverse_iterator rbegin() const noexcept { return crbegin(); }
			reverse_iterator rend() noexcept { return data.rend(); }
			const_reverse_iterator rend() const noexcept { return crend(); }
			const_reverse_iterator crbegin() const noexcept { return data.crbegin(); }
			const_reverse_iterator crend() const noexcept { return data.crend(); }

			// Modifiers
			void push_back(const_reference datum) { data.push_back(datum); }
			void push_back(rvalue_reference datum) { data.push_back(std::forward<value_type>(datum)); }
			void pop_back() { data.pop_back(); }
			void clear() noexcept { data.clear(); }
			void swap(reference other) noexcept { data.swap(other.data); }
			iterator insert(const_iterator pos, const_reference value)
			{
				data.insert(pos, value);
				return pos;
			}

			iterator insert(const_iterator pos, value_type&& value)
			{
				data.insert(pos, std::move(value));
			}

			template <class ItType>
			iterator insert(const_iterator pos, ItType start, ItType finish)
			{
				return std::copy(start, finish, std::inserter(data,pos));
			}

			iterator erase(const_iterator pos) { return data.erase(pos); }
			iterator erase(const_iterator start, const_iterator finish) { return data.erase(start, finish); }

			// Capacity functions
			size_type size() const noexcept { return data.size(); }
			size_type capactiy() const noexcept { return data.capacity(); }
			bool empty() const noexcept { return data.empty(); }
			size_type max_size() const noexcept { return data.max_size(); }
			void reserve(size_type val) { data.reserve(val); }
			void shrink_to_fit() { data.shrink_to_fit(); }
		
		private:
				virtual void ostreamInternal(std::ostream& output, const_reference datum) const = 0;

		protected:
			void printDataToStream(std::ostream& output, char startChar, char endChar) const
			{
				if (data.empty())
				{
					output << startChar << endChar;
					return;
				}
				output << startChar << '\n';
				bool needsDelim = false;
				for (const auto& datum : data)
				{
					if (needsDelim)
					{
						output << ",\n";
					}
					ostreamInternal(output, datum);
					needsDelim = true;
				}
				output << '\n' << endChar;
				return;
			}
		};
	}

	class ArrayType : public AggregateType<ValueType>
	{
	private:
		void ostreamInternal(std::ostream& output, const_reference element) const override final
		{
			output << element;
		}
	public:
		void to_stream(std::ostream& output) const override final
		{
			printDataToStream(output, '[', ']');
		}

		Type type() const noexcept override final { return Type::array; }
	};

	class ObjectType : public AggregateType<std::pair<StringType, ValueType>>
	{
	private:
		using Base = AggregateType<value_type>;
		void ostreamInternal(std::ostream& output, const_reference element) const override final
		{
			output << element.first << " : " << element.second;
		}
	public:
		Type type() const override final { return Type::object; }
		void to_stream(std::ostream& output) const override final
		{
			printDataToStream(output, '{', '}');
		}

		// Add map-like semantics to Object.
		using key_type = StringType;

		iterator find(const key_type& key)
		{
			return std::find_if(begin(), end(), [key](const_reference elem) {return elem.first.get() == key.get(); });
		}

		const_iterator find(const key_type& key) const
		{
			return const_iterator{ const_cast<ObjectType*>(this)->find(key) };
		}

		using Base::push_back;
		void push_back(StringType key, ValueType value) { push_back(std::make_pair(std::move(key), std::move(value))); }
		
		using Base::operator[];
		reference operator[](const key_type& key)
		{
			iterator result = find(key);
			if (result != end())
			{
				return *result;
			}
			else
			{
				push_back(key, NullType{});
				return back();
			}
		}

		reference operator[](key_type&& key)
		{
			iterator result = find(key);
			if (result != end())
			{
				return *result;
			}
			else
			{
				push_back(std::move(key), NullType{});
				return back();
			}
		}

		using Base::at;
		reference at(const key_type& key)
		{
			iterator result = find(key);
			if (result != end())
			{
				return *result;
			}
			else
			{
				throw KeyException{ key.get() };
			}
		}
		const_reference at(const key_type& key) const
		{
			return const_cast<ObjectType*>(this)->at(key);
		}

		size_type count(const key_type& key) const
		{
			return std::count_if(begin(), end(), [key](const value_type& elem) {return key.get() == elem.first.get(); });
		}

		using Base::insert;
		iterator insert(const_iterator pos, key_type&& key, ValueType&& value)
		{
			return Base::insert(pos, value_type{ std::move(key), std::move(value) });
		}
		iterator insert(const_iterator pos, const key_type& key, const ValueType& value)
		{
			return Base::insert(pos, value_type{ key_type{key}, ValueType{value} });
		}
	};

	namespace
	{
		void validateInput(const std::string& expects, std::istream& is)
		{
			std::string got;
			got.reserve(expects.size());
			for (char c : expects)
			{
				got.push_back(is.get());
			}
			if (got != expects)
			{
				std::ostringstream msg;
				msg << "Expects \"" << expects << "\" but got \"" << got << '"';
				throw ParseError{ msg.str() };
			}
		}

		ValueType parseAny(std::istream& is);

		void clearLeadingWhitespace(std::istream& is)
		{
			is >> std::ws;
		}

		template <typename ListType, typename ReadFunc>
		ListType parseListType(std::istream& is, char start, char finish, char delim, ReadFunc readFunc)
		{
			ListType result;
			const char firstChar = is.get();
			assert(firstChar == start); // Clear leading token.
			bool firstRead = true;
			while (is)
			{
				clearLeadingWhitespace(is);
				const char next = firstRead ? delim : is.get();
				firstRead = false;
				if (next == delim)
				{
					readFunc(is,result);
				}
				else if (next == finish)
				{
					return result;
				}
				else
				{
					std::ostringstream msg;
					msg << "Unexpected token while parsing array or object: '" << next << "' (" << static_cast<int>(next) << ')';
					throw ParseError(msg.str());
				}
			}
			std::ostringstream msg;
			msg << "Array or object did not have a closing '" << finish << "' token.";
			throw ParseError(msg.str());
		}

		// All these functions assume zero whitespace at the start of the parse.
		// First char is '['
		ArrayType parseArray(std::istream& is)
		{
			return parseListType<ArrayType>(is, '[', ']',',',
				[](std::istream& is, ArrayType& arr)
			{
				arr.push_back(*(parseAny(is)));
			});
		}

		// First char is 't' or 'f'
		BooleanType parseBool(std::istream& is)
		{
			switch (is.peek())
			{
			case 'f':
				validateInput("false", is);
				return BooleanType{ false };
			case 't':
				validateInput("true", is);
				return BooleanType{ true };
			default:
				assert(false);
			}
			return BooleanType{};
		}

		// First char is 'n'
		NullType parseNull(std::istream& is)
		{
			validateInput("null", is);
			return NullType{};
		}

		// First char is a digit.
		NumberType parseNumber(std::istream& is)
		{
			double val{};
			is >> val;
			return NumberType{ val };
		}

		// First char is '{'
		ObjectType parseObject(std::istream& is)
		{
			return parseListType<ObjectType>(is,'{','}',',',
				[](std::istream& is, ObjectType& obj)
			{
				clearLeadingWhitespace(is);
				const char first = is.get();
				if (first != '"')
				{
					std::ostringstream msg;
					msg << "Object field name must start with '\"'. Instead started with '" << first << '\'';
					throw ParseError{ msg.str() };
				}
				std::string fieldName;
				std::getline(is, fieldName, '"');
				if (std::any_of(begin(fieldName), end(fieldName), ::isspace))
				{
					std::ostringstream msg;
					msg << "Field named \"" << fieldName << "\" constains whitespace.";
					throw ParseError(msg.str());
				}
				char delim{ 0 };
				is >> delim;
				if (delim != ':')
				{
					std::ostringstream msg;
					msg << "Expected field name value to be separated by ':'. Actually got '" << delim << "' for field \"" << fieldName << '"';
					throw ParseError(msg.str());
				}

				obj.push_back(std::move(fieldName), parseAny(is));
			});
		}

		// First char is '"'
		StringType parseString(std::istream& is)
		{
			is.get();
			std::string str;
			std::getline(is, str, '"');
			return StringType{ str };
		}

		ValueType parseAny(std::istream& is)
		{
			clearLeadingWhitespace(is);
			const char first = is.peek();
			switch (first)
			{
			case 'n':
				return parseNull(is);
			case 't':
			case 'f':
				return parseBool(is);
			case '{':
				return parseObject(is);
			case '[':
				return parseArray(is);
			case '"':
				return parseString(is);
			default:
				if (isdigit(first))
				{
					return parseNumber(is);
				}
				else
				{
					std::ostringstream msg;
					msg << "Cannot read JSON starting with '" << first << '\'';
					std::string line;
					std::getline(is, line);
					msg << ". Line is: " << line;
					throw ParseError{ msg.str() };
				}
			}
		}
	}

	PtrType copy(const BaseType& rhs)
	{
		switch (rhs.type())
		{
		case Type::array:
			return rhs.copy_internal<ArrayType>();
		case Type::boolean:
			return rhs.copy_internal<BooleanType>();
		case Type::null:
			return rhs.copy_internal<NullType>();
		case Type::number:
			return rhs.copy_internal<NumberType>();
		case Type::object:
			return rhs.copy_internal<ObjectType>();
		case Type::string:
			return rhs.copy_internal<StringType>();
		default:
			assert(false);
			throw InvalidCopy();
		}
	}

	// Free-function polymorphic move.
	PtrType move(BaseType&& rhs) noexcept
	{
		switch (rhs.type())
		{
		case Type::array:
			return std::move(rhs).move_internal<ArrayType>();
		case Type::boolean:
			return std::move(rhs).move_internal<BooleanType>();
		case Type::null:
			return std::move(rhs).move_internal<NullType>();
		case Type::number:
			return std::move(rhs).move_internal<NumberType>();
		case Type::object:
			return std::move(rhs).move_internal<ObjectType>();
		case Type::string:
			return std::move(rhs).move_internal<StringType>();
		default:
			assert(false);
			return PtrType{ nullptr };
		}
	}
}

std::istream& operator>>(std::istream& input, JSON::ValueType& result)
{
	result = JSON::parseAny(input);
	return input;
}
