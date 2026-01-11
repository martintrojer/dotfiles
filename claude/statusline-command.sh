#!/bin/bash

# Read JSON input from stdin
input=$(cat)

# Extract current directory from workspace
current_dir=$(echo "$input" | jq -r '.workspace.current_dir')
dir_name=$(basename "$current_dir")

# Catppuccin Mocha colors (true color)
CRUST="17;17;27"
BLUE="137;180;250"
GREEN="166;227;161"
PEACH="250;179;135"
TEAL="148;226;213"
YELLOW="249;226;175"
MAUVE="203;166;247"

# Powerline separator
SEP=""

# Helper functions
bg() { printf "\e[48;2;%sm" "$1"; }
fg() { printf "\e[38;2;%sm" "$1"; }
reset() { printf "\e[0m"; }

# Build directory segment (Blue)
dir_seg="$(bg $BLUE)$(fg $CRUST)\e[1m  $dir_name $(reset)"
prev_color="$BLUE"

# Build VCS segment
vcs_seg=""

# Try Git first
if git -C "$current_dir" rev-parse --git-dir >/dev/null 2>&1; then
  branch=$(git -C "$current_dir" --no-optional-locks branch --show-current 2>/dev/null)
  [ -z "$branch" ] && branch=$(git -C "$current_dir" --no-optional-locks rev-parse --short HEAD 2>/dev/null)

  if [ -n "$branch" ]; then
    status=""
    modified=$(git -C "$current_dir" --no-optional-locks diff --name-only 2>/dev/null | wc -l | tr -d ' ')
    staged=$(git -C "$current_dir" --no-optional-locks diff --cached --name-only 2>/dev/null | wc -l | tr -d ' ')
    untracked=$(git -C "$current_dir" --no-optional-locks ls-files --others --exclude-standard 2>/dev/null | wc -l | tr -d ' ')
    
    [ "$modified" -gt 0 ] && status="${status}~${modified} "
    [ "$staged" -gt 0 ] && status="${status}+${staged} "
    [ "$untracked" -gt 0 ] && status="${status}?${untracked} "

    if [ -n "$status" ]; then
      vcs_seg="$(bg $PEACH)$(fg $prev_color)${SEP}$(bg $PEACH)$(fg $CRUST)\e[1m  $branch ${status}$(reset)"
      prev_color="$PEACH"
    else
      vcs_seg="$(bg $GREEN)$(fg $prev_color)${SEP}$(bg $GREEN)$(fg $CRUST)\e[1m  $branch $(reset)"
      prev_color="$GREEN"
    fi
  fi

# Try Jujutsu
elif jj root --ignore-working-copy -R "$current_dir" >/dev/null 2>&1; then
  # Get bookmark or change-id
  branch=$(jj log --ignore-working-copy -R "$current_dir" -r @ --no-graph -T 'if(bookmarks, bookmarks.join(" "), change_id.shortest(8))' 2>/dev/null)

  if [ -n "$branch" ]; then
    status=""
    jj_status=$(jj status --ignore-working-copy -R "$current_dir" 2>/dev/null)
    modified=$(echo "$jj_status" | grep -c '^M')
    added=$(echo "$jj_status" | grep -c '^A')
    deleted=$(echo "$jj_status" | grep -c '^D')

    [ "$modified" -gt 0 ] && status="${status}~${modified} "
    [ "$added" -gt 0 ] && status="${status}+${added} "
    [ "$deleted" -gt 0 ] && status="${status}-${deleted} "

    if [ -n "$status" ]; then
      vcs_seg="$(bg $PEACH)$(fg $prev_color)${SEP}$(bg $PEACH)$(fg $CRUST)\e[1m 󰘬 $branch ${status}$(reset)"
      prev_color="$PEACH"
    else
      vcs_seg="$(bg $GREEN)$(fg $prev_color)${SEP}$(bg $GREEN)$(fg $CRUST)\e[1m 󰘬 $branch $(reset)"
      prev_color="$GREEN"
    fi
  fi

# Try Mercurial
elif hg -R "$current_dir" root >/dev/null 2>&1; then
  # Get bookmark or branch
  bookmark=$(hg -R "$current_dir" id -B 2>/dev/null | awk '{print $1}')
  if [ -n "$bookmark" ]; then
    branch="$bookmark"
  else
    branch=$(hg -R "$current_dir" id -b 2>/dev/null)
  fi

  if [ -n "$branch" ]; then
    status=""
    hg_status=$(hg -R "$current_dir" status 2>/dev/null)
    modified=$(echo "$hg_status" | grep -c '^M')
    added=$(echo "$hg_status" | grep -c '^A')
    untracked=$(echo "$hg_status" | grep -c '^?')
    
    [ "$modified" -gt 0 ] && status="${status}~${modified} "
    [ "$added" -gt 0 ] && status="${status}+${added} "
    [ "$untracked" -gt 0 ] && status="${status}?${untracked} "

    if [ -n "$status" ]; then
      vcs_seg="$(bg $PEACH)$(fg $prev_color)${SEP}$(bg $PEACH)$(fg $CRUST)\e[1m ☿ $branch ${status}$(reset)"
      prev_color="$PEACH"
    else
      vcs_seg="$(bg $GREEN)$(fg $prev_color)${SEP}$(bg $GREEN)$(fg $CRUST)\e[1m ☿ $branch $(reset)"
      prev_color="$GREEN"
    fi
  fi
fi

# Calculate context window usage
usage=$(echo "$input" | jq '.context_window.current_usage')
if [ "$usage" != "null" ]; then
  current=$(echo "$usage" | jq '.input_tokens + .cache_creation_input_tokens + .cache_read_input_tokens')
  size=$(echo "$input" | jq '.context_window.context_window_size')
  pct=$((current * 100 / size))
else
  pct=0
fi

# Context color based on usage
if [ $pct -lt 50 ]; then
  ctx_color="$TEAL"
elif [ $pct -lt 80 ]; then
  ctx_color="$YELLOW"
else
  ctx_color="$MAUVE"
fi

ctx_seg="$(bg $ctx_color)$(fg $prev_color)${SEP}$(bg $ctx_color)$(fg $CRUST)\e[1m 󰍛 ${pct}% $(reset)$(fg $ctx_color)${SEP}$(reset)"

# Output
printf "%b%b%b" "$dir_seg" "$vcs_seg" "$ctx_seg"
