#include "process_utils.h"

#include <Windows.h>

#include <assert.h>
#include <direct.h>
#include <iostream>
#include <filesystem>
#include <optional>


std::string ReadFromPipe(HANDLE stdout_rd)

// Read output from the child process's pipe for STDOUT
// and write to the parent process's pipe for STDOUT. 
// Stop when there is no more data. 
{
  DWORD dwRead;
  CHAR chBuf[4096] = {'\0'};
  BOOL bSuccess = FALSE;
  HANDLE hParentStdOut = GetStdHandle(STD_OUTPUT_HANDLE);

  DWORD bytesAvail = 0;
  if (!PeekNamedPipe(stdout_rd, NULL, 0, NULL, &bytesAvail, NULL)) {
    std::cout << "Could not peek stdout for child process" << std::endl;
    return "";
  }

  if (bytesAvail) {
    ReadFile(stdout_rd, chBuf, sizeof(chBuf), &dwRead, NULL);
  }

  return std::string(chBuf);
}

std::optional<CommandOutput> run_cubic(const wchar_t* filename) {
  SECURITY_ATTRIBUTES saAttr;
  STARTUPINFO si;
  PROCESS_INFORMATION pi;

  ZeroMemory(&saAttr, sizeof(saAttr));
  saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
  saAttr.bInheritHandle = TRUE;
  saAttr.lpSecurityDescriptor = NULL;

  // Create a pipe for the child process's STDOUT. 
  HANDLE g_hChildStd_OUT_Rd = NULL;
  HANDLE g_hChildStd_OUT_Wr = NULL;
  assert(CreatePipe(&g_hChildStd_OUT_Rd, &g_hChildStd_OUT_Wr, &saAttr, 0));
  assert(SetHandleInformation(g_hChildStd_OUT_Rd, HANDLE_FLAG_INHERIT, 0));

  ZeroMemory(&si, sizeof(si));
  si.cb = sizeof(si);
  si.hStdError = g_hChildStd_OUT_Wr;
  si.hStdOutput = g_hChildStd_OUT_Wr;
  si.dwFlags |= STARTF_USESTDHANDLES;
  ZeroMemory(&pi, sizeof(pi));


  // Start the child process. 
  if (!CreateProcess(NULL,   // No module name (use command line)
    (wchar_t*)filename,        // Command line
    NULL,           // Process handle not inheritable
    NULL,           // Thread handle not inheritable
    TRUE,          // Set handle inheritance to TRUE
    0,              // No creation flags
    NULL,           // Use parent's environment block
    NULL,           // Use parent's starting directory 
    &si,            // Pointer to STARTUPINFO structure
    &pi)           // Pointer to PROCESS_INFORMATION structure
    ) {
    std::cerr << "Could not run 'cubic': " << GetLastError() << std::endl;
    return std::nullopt;
  }

  WaitForSingleObject(pi.hProcess, 5000); // Change to 'INFINITE' wait if req'd

  std::string output = ReadFromPipe(g_hChildStd_OUT_Rd);

  DWORD exit_code;
  if (FALSE == GetExitCodeProcess(pi.hProcess, &exit_code)) {
    std::cerr << "Could not get exit code: " << GetLastError() << std::endl;
  } else if (STILL_ACTIVE == exit_code) {
    std::cout << "Process is still running. Terminating." << std::endl;
    if (FALSE == TerminateProcess(pi.hProcess, 4)) {
      std::cerr << "Could not kill cubic: " << GetLastError() << std::endl;
      for (int i = 0; i < 5; ++i) {
        std::cerr << "PANIC" << std::endl;
      }
      exit(4);
    }
  }

  // Close process and thread handles. 
  CloseHandle(pi.hProcess);
  CloseHandle(pi.hThread);

  // Close handles to the stdout pipe no longer needed by the child process.
  // If they are not explicitly closed, there is no way to recognize that the
  // child process has ended.
  CloseHandle(g_hChildStd_OUT_Wr);
  CloseHandle(g_hChildStd_OUT_Rd);

  return CommandOutput{
    .std_out = output,
    .exit_code = exit_code
  };
}

std::optional<CommandOutput> run_file(const std::filesystem::path& source_dir, const std::string& cub_file) {
  auto cubic = (source_dir / "cubic.exe").wstring();
  std::wstring cmd = cubic + L" " + (source_dir / cub_file).wstring();
  const wchar_t* cmd_string = cmd.c_str();

  std::cout << "=========================" << std::endl;
  std::cout << "Running cub file \"" << cub_file << "\"..." << std::endl;
  auto ret = run_cubic(cmd_string);
  std::cout << "Exited with code " << ret.value().exit_code << std::endl << std::endl;
  std::cout << "stdout >>>>>>>>>>>>>>>>>>" << std::endl;
  std::cout << ret.value().std_out << std::endl;
  std::cout << "<<<<<<<<<<<<<<<<<<<<<<<<<" << std::endl;
  std::cout << "=========================" << std::endl;
  return ret;
}