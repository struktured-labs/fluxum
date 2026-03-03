#!/bin/bash
# PostToolUse hook: auto-build after OCaml file edits
# Catches compilation errors immediately instead of cascading later

INPUT=$(cat)
FILE_PATH=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty')

# Only trigger for OCaml files
case "$FILE_PATH" in
  *.ml|*.mli) ;;
  *) exit 0 ;;
esac

cd "$CLAUDE_PROJECT_DIR" || exit 0

# Source setenv if it exists (needed for some build deps)
[ -f setenv.sh ] && source setenv.sh 2>/dev/null

# Run build, capture output
BUILD_OUTPUT=$(dune build 2>&1)
BUILD_EXIT=$?

if [ $BUILD_EXIT -ne 0 ]; then
  echo "BUILD FAILED after editing $FILE_PATH:" >&2
  echo "$BUILD_OUTPUT" >&2
  exit 1
fi

exit 0
