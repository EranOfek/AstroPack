#!/bin/bash
# ============================================================
#  Attach to running MATLAB Slew Service tmux session
# ============================================================

SESSION="soc_slew_matlab"
if tmux has-session -t "$SESSION" 2>/dev/null; then
  echo "Attaching to [$SESSION]..."
  tmux attach-session -t "$SESSION"
else
  echo "Session [$SESSION] not found."
fi
