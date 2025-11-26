#include "scanner.h"


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vector>

#define MAX_LINE_LENGTH 4096
static void repl() {
  char* line = (char*)malloc(MAX_LINE_LENGTH);
  assert(line);

  for (;;) {
    printf("> ");

    if (!fgets(line, MAX_LINE_LENGTH, stdin)) {
      printf("\n");
      break;
    }
    Scanner scanner(line);
    Token tk = {};
    do {
      tk = scanner.Scan();
      printf("(%.*s)\n", tk.tk.length(), tk.tk.data());
    } while (tk.type != TK_EOF);
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

int main(int argc, const char* argv[]) {
  std::vector<std::string_view> args;
  for (int i = 0; i < argc; ++i) {
    args.emplace_back(argv[i]);
  }

  //const char* filename = "D:\\Users\\rohde\\source\\repos\\Cubic\\x64\\Debug\\generic_functions.cub";
  //run_file(&vm, filename);

  if (argc == 1) {
    repl();
  } else if (argc == 2) {
    if (args.back().starts_with("-")) {
      fprintf(stderr, "Usage: cubic [-s] [path]\n");
      exit(1);
    }

    bool output_scan = false;

    for (int i = 1; i < args.size(); ++i) {
      if (args[i] == "-s") {
        output_scan = true;
      }
    }
    // run_file(&vm, argv[1]);
  } else {
    fprintf(stderr, "Usage: cubic [path]\n");
    exit(64);
  }

  return 0;
}