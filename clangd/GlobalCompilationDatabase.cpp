//===--- GlobalCompilationDatabase.cpp --------------------------*- C++-*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===---------------------------------------------------------------------===//

#include "GlobalCompilationDatabase.h"
#include "Logger.h"
#include "index/ClangdIndex.h"

#include "clang/Tooling/CompilationDatabase.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"

namespace clang {
namespace clangd {

tooling::CompileCommand
GlobalCompilationDatabase::getFallbackCommand(PathRef File) const {
  return tooling::CompileCommand(llvm::sys::path::parent_path(File),
                                 llvm::sys::path::filename(File),
                                 {"clang", File.str()},
                                 /*Output=*/"");
}

DirectoryBasedGlobalCompilationDatabase::
    DirectoryBasedGlobalCompilationDatabase(
        clangd::Logger &Logger, llvm::Optional<Path> CompileCommandsDir)
    : Logger(Logger), CompileCommandsDir(std::move(CompileCommandsDir)) {}


std::vector<tooling::CompileCommand> DirectoryBasedGlobalCompilationDatabase::getCompileCommandsUsingIndex(std::unique_ptr<ClangdIndexFile> IndexFile) const {

  std::vector<tooling::CompileCommand> Commands;

  class FirstDependentSourceFileVisitor : public ClangdIndexFile::DependentVisitor {
    const DirectoryBasedGlobalCompilationDatabase &GCDB;
    std::vector<tooling::CompileCommand> &Commands;
  public:
    FirstDependentSourceFileVisitor(const DirectoryBasedGlobalCompilationDatabase &GCDB,
        std::vector<tooling::CompileCommand> &Commands) :
          GCDB(GCDB), Commands(Commands) {
    }

    virtual ClangdIndexFile::NodeVisitResult VisitDependent(ClangdIndexFile& IndexFile) {
      auto IncludedBy = IndexFile.getFirstIncludedBy();
      if (!IncludedBy) {
        auto CDB = GCDB.getCompilationDatabase(IndexFile.getPath());
        if (CDB) {
          Commands = CDB->getCompileCommands(IndexFile.getPath());
          return ClangdIndexFile::NodeVisitResult::ABORT;
        }
      }
      return ClangdIndexFile::NodeVisitResult::CONTINUE;
    }
  };

  FirstDependentSourceFileVisitor V(*this, Commands);
  IndexFile->visitDependentFiles(V);
  return Commands;
}

llvm::Optional<tooling::CompileCommand>
DirectoryBasedGlobalCompilationDatabase::getCompileCommand(PathRef File) const {
  if (auto CDB = getCompilationDatabase(File)) {
    auto Candidates = CDB->getCompileCommands(File);
    if (!Candidates.empty()) {
      addExtraFlags(File, Candidates.front());
      return std::move(Candidates.front());
    }

    //TODO: index mutex needs to be locked!
    if (auto LockedIndex = Index.lock()) {
      auto IndexFile = LockedIndex->getFile(File.str());
      if (IndexFile) {
        auto Commands = getCompileCommandsUsingIndex(std::move(IndexFile));
        if (!Commands.empty()) {
          return std::move(Commands.front());
        }
      }
    }
  }
  return llvm::None;
}

tooling::CompileCommand
DirectoryBasedGlobalCompilationDatabase::getFallbackCommand(
    PathRef File) const {
  auto C = GlobalCompilationDatabase::getFallbackCommand(File);
  addExtraFlags(File, C);
  return C;
}

void DirectoryBasedGlobalCompilationDatabase::setExtraFlagsForFile(
    PathRef File, std::vector<std::string> ExtraFlags) {
  std::lock_guard<std::mutex> Lock(Mutex);
  ExtraFlagsForFile[File] = std::move(ExtraFlags);
}

void DirectoryBasedGlobalCompilationDatabase::addExtraFlags(
    PathRef File, tooling::CompileCommand &C) const {
  std::lock_guard<std::mutex> Lock(Mutex);

  auto It = ExtraFlagsForFile.find(File);
  if (It == ExtraFlagsForFile.end())
    return;

  auto &Args = C.CommandLine;
  assert(Args.size() >= 2 && "Expected at least [compiler, source file]");
  // The last argument of CommandLine is the name of the input file.
  // Add ExtraFlags before it.
  Args.insert(Args.end() - 1, It->second.begin(), It->second.end());
}

tooling::CompilationDatabase *
DirectoryBasedGlobalCompilationDatabase::tryLoadDatabaseFromPath(
    PathRef File) const {

  namespace path = llvm::sys::path;
  auto CachedIt = CompilationDatabases.find(File);

  assert((path::is_absolute(File, path::Style::posix) ||
          path::is_absolute(File, path::Style::windows)) &&
         "path must be absolute");

  if (CachedIt != CompilationDatabases.end())
    return CachedIt->second.get();
  std::string Error = "";
  auto CDB = tooling::CompilationDatabase::loadFromDirectory(File, Error);
  if (CDB) {
    auto Result = CDB.get();
    CompilationDatabases.insert(std::make_pair(File, std::move(CDB)));
    return Result;
  }

  return nullptr;
}

tooling::CompilationDatabase *
DirectoryBasedGlobalCompilationDatabase::getCompilationDatabase(
    PathRef File) const {
  std::lock_guard<std::mutex> Lock(Mutex);

  namespace path = llvm::sys::path;
  if (CompileCommandsDir.hasValue()) {
    tooling::CompilationDatabase *ReturnValue =
        tryLoadDatabaseFromPath(CompileCommandsDir.getValue());
    if (ReturnValue == nullptr)
      Logger.log("Failed to find compilation database for " + Twine(File) +
                 "in overriden directory " + CompileCommandsDir.getValue());
    return ReturnValue;
  }

  for (auto Path = path::parent_path(File); !Path.empty();
       Path = path::parent_path(Path)) {
    auto CDB = tryLoadDatabaseFromPath(Path);
    if (!CDB)
      continue;
    // FIXME(ibiryukov): Invalidate cached compilation databases on changes
    return CDB;
  }

  Logger.log("Failed to find compilation database for " + Twine(File));
  return nullptr;
}

} // namespace clangd
} // namespace clang
