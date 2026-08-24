#!/bin/bash
# Syncs the unlisted PSCI 2075 course materials (Setup Guide, Syllabus)
# from the Teaching project into this website and publishes them.
#
# Live URLs (unlisted — not linked anywhere on the site):
#   https://fgneuner.github.io/files/psci-2075/setup-guide.html
#   https://fgneuner.github.io/files/psci-2075/syllabus.html
#   https://fgneuner.github.io/files/psci-2075/syllabus.pdf
#
# Usage:
#   ./scripts/sync-psci-2075.sh          # render, copy, commit, push
#   ./scripts/sync-psci-2075.sh --no-push  # render, copy, commit only

set -euo pipefail

QUARTO="/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto"
SITE_DIR="/Users/fabianneuner/Dropbox/Fabian/Fabian Neuner Website"
PREP_DIR="/Users/fabianneuner/Dropbox/Fabian/Teaching/CU Boulder/PSCI/PSCI 2075/Fall 2026/Course Prep"
SYLDEV_DIR="/Users/fabianneuner/Dropbox/Fabian/Teaching/CU Boulder/PSCI/PSCI 2075/Fall 2026/Syllabus Dev"
DEST_DIR="$SITE_DIR/files/psci-2075"

NO_PUSH=false
if [[ "${1:-}" == "--no-push" ]]; then
  NO_PUSH=true
fi

echo "==> Rendering Setup Guide..."
cd "$PREP_DIR"
"$QUARTO" render setup-guide.qmd --to html

echo "==> Rendering Syllabus (html + pdf)..."
cd "$SYLDEV_DIR"
# SOURCE_DATE_EPOCH pins LaTeX's embedded PDF creation timestamp so the
# PDF comes out byte-identical across runs when nothing actually changed
# (otherwise every render diffs in git purely from metadata churn, even
# with identical visible content). The visible "last updated" date in the
# syllabus text still uses the real render date - this only affects
# invisible PDF metadata.
SOURCE_DATE_EPOCH=1700000000 "$QUARTO" render syllabus.qmd

echo "==> Copying into $DEST_DIR ..."
mkdir -p "$DEST_DIR"
cp "$PREP_DIR/setup-guide.html" "$DEST_DIR/setup-guide.html"
cp "$SYLDEV_DIR/syllabus.html" "$DEST_DIR/syllabus.html"
cp "$SYLDEV_DIR/syllabus.pdf" "$DEST_DIR/syllabus.pdf"

echo "==> Rendering website (copies resources into docs/)..."
cd "$SITE_DIR"
"$QUARTO" render

if git -C "$SITE_DIR" diff --quiet && git -C "$SITE_DIR" diff --cached --quiet && [ -z "$(git -C "$SITE_DIR" status --porcelain)" ]; then
  echo "==> No changes to commit — files were already up to date."
  exit 0
fi

echo "==> Committing..."
cd "$SITE_DIR"
git add files/psci-2075 docs/files/psci-2075
git commit -m "Sync PSCI 2075 setup guide and syllabus"

if [ "$NO_PUSH" = true ]; then
  echo "==> Committed locally. Skipping push (--no-push passed)."
else
  echo "==> Pushing..."
  git push
  echo "==> Done. Live at:"
  echo "    https://fgneuner.github.io/files/psci-2075/setup-guide.html"
  echo "    https://fgneuner.github.io/files/psci-2075/syllabus.html"
  echo "    https://fgneuner.github.io/files/psci-2075/syllabus.pdf"
fi
