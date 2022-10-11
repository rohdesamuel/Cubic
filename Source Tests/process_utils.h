#ifndef PROCESS_UTILS__H

#include <filesystem>
#include <optional>
#include <string>

struct CommandOutput {
  std::string std_out = "";
  unsigned exit_code = -1;
};

std::optional<CommandOutput> run_file(const std::filesystem::path& source_dir, const std::string& cub_file);

#endif // PROCESS_UTILS__H
