#ifndef TOOLS_CLANG_TOOLS_EXTRA_CLANGD_INDEX_CLANGDINDEXDATAPROVIDER_H_
#define TOOLS_CLANG_TOOLS_EXTRA_CLANGD_INDEX_CLANGDINDEXDATAPROVIDER_H_

#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"

#include "clang/Index/IndexSymbol.h"

#include <memory>

namespace clang {

class SourceManager;

namespace clangd {

using USR = llvm::SmallString<256>;

class ClangdIndexDataOccurrence {
public:
  enum class OccurrenceType : uint16_t {
     OCCURRENCE,
     DEFINITION_OCCURRENCE
   };

  virtual OccurrenceType getKind() const = 0;
  virtual std::string getPath() = 0;
  virtual uint32_t getStartOffset(SourceManager &SM) = 0;
  virtual uint32_t getEndOffset(SourceManager &SM) = 0;
  virtual ~ClangdIndexDataOccurrence() = default;

  static bool classof(const ClangdIndexDataOccurrence *O) { return O->getKind() == OccurrenceType::OCCURRENCE; }
};

class ClangdIndexDataDefinitionOccurrence : public ClangdIndexDataOccurrence {
public:
  virtual uint32_t getDefStartOffset(SourceManager &SM) = 0;
  virtual uint32_t getDefEndOffset(SourceManager &SM) = 0;

  static bool classof(const ClangdIndexDataOccurrence *O) { return O->getKind() == OccurrenceType::DEFINITION_OCCURRENCE; }
};

class ClangdIndexDataProvider {
public:
  virtual void foreachOccurrence(const USR& Buf, index::SymbolRoleSet Roles, llvm::function_ref<bool(ClangdIndexDataOccurrence&)> Receiver) = 0;

  virtual void dumpIncludedBy(StringRef File) {}
  virtual void dumpInclusions(StringRef File) {}

  virtual ~ClangdIndexDataProvider() = default;
};

} // namespace clangd
} // namespace clang

#endif
