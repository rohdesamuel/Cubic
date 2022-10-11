#include "pch.h"
#include "CppUnitTest.h"
#include "../Cubic/parser.h"

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

namespace Test
{
	TEST_CLASS(Test)
	{
	public:
		
		TEST_METHOD(TestMethod1)
		{
			Parser_ parser;
			parser_init(&parser);
			parser_clear(&parser);
		}
	};
}
