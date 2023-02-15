#!/bin/bash

answer=(
  "The answer is"
  42
  "according to our calculations"
  "done on 'Earth'."
)

cmd=(
  "printf"
  "%s\n%s %d %s %s\n%s %s\n"
  "What is the meaning of life, the universe, and everything?"
  "${answer[@]}"
)

# Can be split!
cmd=(
  "${cmd[@]}"
  "The \"\$(hostname)\" is $(hostname)."
  'The "$(hostname)" is $(hostname).'
)

echo "-------------------"

echo "${cmd[@]}"

echo "-------------------"

for item in "${cmd[@]}"; do
  echo "$item"
done

echo "-------------------"

"${cmd[@]}"

echo "-------------------"

"${cmd[@]}" &
echo $!
sleep 0.1

echo "-------------------"

cmd_str='"${cmd[@]}"'
eval "$cmd_str"

echo "-------------------"

cmd_str='"${cmd[@]}"'
eval "$cmd_str &"
echo $!
sleep 0.1

echo "-------------------"
