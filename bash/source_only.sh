#!/bin/bash

sourcerers_only() {
  echo "You are a wizard! Go to platform 9 3/4."
}

main() {
  echo "main"
}

if [[ "${__name__:-__main__}" == "__main__" ]]; then
  main "$@"
fi
