#include "vm.h"

#include "chunk.h"
#include "debug.h"
#include "lexer.h"
#include "scanner.h"


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
* class Bar c: int, d: int end; class Foo a: int, b: Bar end; val foo := Foo{1, Bar{2, 3}}; val a := foo.b.d; print(a)
* 
const char* code = R"

[pval | mut | ref | ptr] name : type = expression

TYPE_EXPRESSION = mut? [ref | ptr] 

struct Vector3:
  x: float
  y: float
  z: float

struct TaggedUnion:
  x: float | int

struct AnonymousFields:
  float

struct Name:
  x: string

struct Id:
  int

a : Entity[Id, Name, Vector3] = (0, 'A', (1, 2, 3))

a += Vector3
a += Name

b : Vector3 = (1, 2, 3)

# Destructuring the vector
x, y, z := b

fn *Vector3(a: Vector3, b: Vector3) -> Vector3:
  return Vector3(x=a.x + b.x, y=a.y + b.y, z=a.z + b.z)

fn operator+ (a: Vector3, b: Vector3) -> Vector3:
  return Vector3(x=a.x + b.x, y=a.y + b.y, z=a.z + b.z)

fn foo(size: int32) -> var char*:
  return char[size]

fn foo(x: *int):
  x = 0

fn foo(x: **int) -> int:
  x = 0

fn polymorphic(x):
  x = 0

fn polymorphic(x) -> :
  return x




fn constrained_polymorphism[T is (Movable, Copyable)]
    (x: T):
  x = 0

fn constrained_polymorphism[T is (Movable, Copyable)] (x: T):
  x = 0

fn add(a: int, b: int) -> () -> void:
  fn() -> int: a + b

a := () -> int: 3

fn add(a: int, b: int):
  fn() -> float: a + b

fn sum(a, b):
  fn(): a + b

# Compute the x'th fibonacci number.
fn fib(x: int) -> int:
  if x < 3:
    return 1
  else:
    return fib(x - 1) + fib(x - 2)

# This expression will compute the 40th number.
fib(40)

// Static array.
d: float[2] = [2.f, 2.f]


a : float[] = [1.f, 2.f]
(b, c) := a

// Multi-dimensional static and dynamic arrays.

// std::array<std::array<float, 3>, 2> d;
d: float[3][2] = [[1.f, 0.f, 0.f], [0.f, 1.f, 0.f]]

// std::vector<float>
d: list[float] = [ 1.f, 2.f ]

// std::vector<std::vector<float>> d;
d: list[list[float]] = [ [1.f], [2.f, 3.f] ]

// std::vector<std::array<float, 2>> d;
d: list[float[2]] =  [[1.f, 0.f], [0.f, 1.f], [0.f, 0.f]]

// std::array<std::vector<float>, 2> d;
d: list[float][2] =  [[1.f], [0.f, 1.f]]

// std::array<std::unordered_set<int>, 2> d;
d: set[int][2] = [ {1.f, 2.f}, {3.f, 4.f, 5.f} ]
d: set[set[int]] = { {1.f, 2.f}, {3.f, 4.f, 5.f} }

// Set of ints.
e: set[int] = { 1, 2, 3}

// Set of boxes.
e: set[] = { 1, "a", 5.f }
e: list[] = [1, "b", 10.f]

f: dict[int: str]  = { 1: "a", 2: "b" }

g: dict[:] = { 1: "a", "b": 2.f }

h: float[2][2]

";
*/

#define MAX_LINE_LENGTH 4096
static void repl(VM vm) {
  char* line = malloc(MAX_LINE_LENGTH);
  assert(line);

  for (;;) {
    printf("> ");

    if (!fgets(line, MAX_LINE_LENGTH, stdin)) {
      printf("\n");
      break;
    }
    vm_interpret(vm, line);
  }

  free(line);
}

static char* read_file(const char* path) {
  FILE* file = NULL;
  errno_t err = fopen_s(&file, path, "rb");
  if (err != 0) {
    fprintf(stderr, "Could not open file \"%s\".\n", path);
    exit(74);
  }

  fseek(file, 0L, SEEK_END);
  size_t file_size = ftell(file);
  rewind(file);

  char* buffer = (char*)malloc(file_size + 1);
  if (!buffer) {
    fprintf(stderr, "Not enough memory to read \"%s\".\n", path);
    exit(74);
  }

  size_t bytes_read = fread(buffer, sizeof(char), file_size, file);
  if (bytes_read < file_size) {
    fprintf(stderr, "Could not read file \"%s\".\n", path);
    exit(74);
  }

  buffer[bytes_read] = '\0';

  fclose(file);
  return buffer;
}

static void run_file(VM vm, const char* path) {
  char* source = read_file(path);
  if (!source) {
    return;
  }

  InterpretResult result = vm_interpret(vm, source);
  free(source);

  if (result == INTERPRET_COMPILE_ERROR) exit(65);
  if (result == INTERPRET_RUNTIME_ERROR) exit(70);
  if (result == INTERPRET_ASSERTION_FAILED) exit(75);
}

int main(int argc, const char* argv[]) {
  VM_ vm;
  vm_init(&vm);
  
  //const char* filename = "D:\\Users\\rohde\\source\\repos\\Cubic\\x64\\Debug\\arrays.cub";
  //run_file(&vm, filename);

  if (argc == 1) {
    repl(&vm);
  } else if (argc == 2) {
    run_file(&vm, argv[1]);
  } else {
    fprintf(stderr, "Usage: cubic [path]\n");
    exit(64);
  }

  vm_free(&vm);

  return 0;
}