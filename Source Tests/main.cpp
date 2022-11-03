#include <assert.h>
#include <filesystem>

#include "process_utils.h"
#include "utest.h"

UTEST_STATE();

std::filesystem::path exedir = {};

struct MyTestFixture {
  std::string current_dir;
};

UTEST_F_SETUP(MyTestFixture) {  
  auto current_dir = std::filesystem::current_path();
}

UTEST_F_TEARDOWN(MyTestFixture) {}

UTEST_F(MyTestFixture, hello_world) {
  auto output = run_file(exedir, "hello_world.cub");
  ASSERT_TRUE(output.has_value());
  ASSERT_EQ(0, output->exit_code);

  const char* expected = "Hello, World!";
  const char* actual = output->std_out.c_str();
  ASSERT_STRNEQ(expected, actual, strlen(expected));
}

UTEST_F(MyTestFixture, conditionals) {
  auto output = run_file(exedir, "conditionals.cub");
  ASSERT_TRUE(output.has_value());
  ASSERT_EQ(0, output->exit_code);
}

UTEST_F(MyTestFixture, operators) {
  auto output = run_file(exedir, "operators.cub");
  ASSERT_TRUE(output.has_value());
  ASSERT_EQ(0, output->exit_code);
}

UTEST_F(MyTestFixture, global_variables) {
  auto output = run_file(exedir, "global_variables.cub");
  ASSERT_TRUE(output.has_value());
  ASSERT_EQ(0, output->exit_code);
}

UTEST_F(MyTestFixture, local_variables) {
  auto output = run_file(exedir, "local_variables.cub");
  ASSERT_TRUE(output.has_value());
  ASSERT_EQ(0, output->exit_code);
}

UTEST_F(MyTestFixture, multiline_comments) {
  auto output = run_file(exedir, "multiline_comments.cub");
  ASSERT_TRUE(output.has_value());
  ASSERT_EQ(0, output->exit_code);
}

UTEST_F(MyTestFixture, scope) {
  auto output = run_file(exedir, "scope.cub");
  ASSERT_TRUE(output.has_value());
  ASSERT_EQ(0, output->exit_code);
}

UTEST_F(MyTestFixture, simple_type_inference) {
  auto output = run_file(exedir, "simple_type_inference.cub");
  ASSERT_TRUE(output.has_value());
  ASSERT_EQ(0, output->exit_code);
}

UTEST_F(MyTestFixture, strings) {
  auto output = run_file(exedir, "strings.cub");
  ASSERT_TRUE(output.has_value());
  ASSERT_EQ(0, output->exit_code);
}

UTEST_F(MyTestFixture, arrays) {
  auto output = run_file(exedir, "arrays.cub");
  ASSERT_TRUE(output.has_value());
  ASSERT_EQ(0, output->exit_code);
}

UTEST_F(MyTestFixture, while_loops) {
  auto output = run_file(exedir, "while_loops.cub");
  ASSERT_TRUE(output.has_value());
  ASSERT_EQ(0, output->exit_code);
}


int main(int argc, char** argv) {
  exedir = std::filesystem::path(argv[0]).parent_path();
  return utest_main(argc, argv);
}